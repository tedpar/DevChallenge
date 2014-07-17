namespace System.Net

    open System
    open System.Threading
    open System.Net
    open System.Net.Sockets
    open System.Collections.Generic
    open System.Xml.Linq

    type ConnectionState =
        | Created
        | Connecting
        | Connected
        | Disconnecting
        | Disconnected
        | Failed of ex:Exception
        override this.ToString() =
            match this with
            | Created -> "Created"
            | Connecting -> "Connecting"
            | Connected -> "Connected"
            | Disconnecting -> "Disconnecting"
            | Disconnected -> "Disconnected"
            | Failed(ex) -> sprintf "Failed: %s" ex.Message

    type AsyncSocket() as this =
        inherit Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)

        let mutable connectionState = ConnectionState.Created
        let connectionStateChangedEvent = Event<ConnectionState>()
        let receivedDataEvent = Event<ArraySegment<Byte>>()
        let receivedMessageEvent = Event<XElement>()

        let sendString (stringToSend:String) = async {
            let bytesToSend = System.Text.Encoding.UTF8.GetBytes(stringToSend)
            let! bytesSent = Async.FromBeginEnd(new List<ArraySegment<Byte>>([ArraySegment(bytesToSend)]), new SocketFlags(), (fun (data, flags, callback, state) -> this.BeginSend(data, flags, callback, state)), this.EndSend)
            let timestamp = DateTime.Now.ToString("HH:mm:ss.fff")
            printfn "%s Data has been sent" timestamp
            return bytesSent
        } 

        let receiveBuffer = Array.zeroCreate<byte> 4096
        let rec readDataWhileConnected offset = async {
            try
                let! numBytesReceived = Async.FromBeginEnd(List<ArraySegment<Byte>>([ArraySegment(receiveBuffer, offset, 4096-offset)]), new SocketFlags(), (fun (data, flags, callback, state) -> this.BeginReceive(data, flags, callback, state)), this.EndReceive)
                if numBytesReceived > 0 then
                    receivedDataEvent.Trigger (ArraySegment(receiveBuffer, offset, numBytesReceived))

                    let rec parseMessages length =
                        let segment = ArraySegment(receiveBuffer, 0, length)
                        let result = XmlFramer.extractMessageString segment
                        match result with 
                        | Some(xmlString, parsedBytes) ->
                            let xml = XElement.Parse(xmlString)
                            receivedMessageEvent.Trigger xml
                            let remainingBytes = length - parsedBytes
                            Array.Copy(receiveBuffer, parsedBytes, receiveBuffer, 0, remainingBytes)
                            if remainingBytes = 0 
                            then 0 
                            else parseMessages remainingBytes
                        | None -> length
                        
                    let newOffset = parseMessages (offset+numBytesReceived)
                    do! readDataWhileConnected newOffset
            with
            | ex -> 
                connectionState <- Failed(ex)
                connectionStateChangedEvent.Trigger(connectionState)
        }

        let connect (hostAddress:String) (port:int) = async {
            try
                this.NoDelay <- true
                connectionState <- Connecting
                connectionStateChangedEvent.Trigger(connectionState)
                do! Async.FromBeginEnd(Dns.GetHostEntry(hostAddress).AddressList.[0], port, (fun (host, port, callback, state) -> this.BeginConnect(host, port, callback, state)), this.EndConnect)
                connectionState <- Connected
                connectionStateChangedEvent.Trigger(connectionState)
                do! readDataWhileConnected 0
                connectionState <- Disconnected
                connectionStateChangedEvent.Trigger(connectionState)
                this.Close()
            with
            | ex -> connectionStateChangedEvent.Trigger(Failed(ex))
        }

        member this.ConnectionState with get () = connectionState 
        member this.ConnectionStateChanged = connectionStateChangedEvent.Publish
        member this.ReceivedData = receivedDataEvent.Publish
        member this.ReceivedMessage = receivedMessageEvent.Publish
        member this.AsyncConnectToHost = connect
        member this.AsyncSendString = sendString
