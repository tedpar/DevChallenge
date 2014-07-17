namespace DevChallenge

module Program =

    open System.Net

    [<EntryPoint>]
    let main argv = 
        while true do
            let mutable allOk = true
            let client = new DevChallenge.MineSweeperClient()
            let socket = client.Start "username" "password" false
            while allOk do
                System.Threading.Thread.Sleep(1000)
                try
                    match (socket.ConnectionState, client.LastMessageReceived) with
                    | (Connected, Some(timestamp)) when timestamp < System.DateTime.Now.AddSeconds(-30.0) -> 
                        allOk <- false
                        socket.Shutdown(Sockets.SocketShutdown.Both)
                        socket.Dispose()
                    | (Failed(ex), _) ->
                        allOk <- false
                        socket.Dispose()
                    | (Disconnected, Some(timestamp)) ->
                        allOk <- false
                        socket.Dispose()
                    | (_, _) -> ()
                with
                | ex -> ()
        0
