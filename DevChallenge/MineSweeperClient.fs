namespace DevChallenge

    open System
    open System.Net
    open DevChallenge
    open DevChallenge.MessageTypes
    open DevChallenge.PathFinder

    type MineSweeperClient () =
        let serverAddress = "devchallenge1.cloudapp.net"
        let serverPort = 8231
        let fullName = "Ted Parnefors"
        let email = "ted.parnefors@kentor.se"
        let agentName = "AStarWalker"
        let agentRevision = "1"
        let randomizer = System.Random()

        let mutable lastPosition = (-1, -1)
        let mutable lastMove = MineSweeperDirection.None

        let calculateMove (minePositions:MinePosition list) (sweeperPositions:SweeperPosition list) clientSweeperId =
            let myPos = sweeperPositions |> Seq.find (fun pos -> pos.SweeperId = clientSweeperId)
            if minePositions.IsEmpty then 
                printfn "There are no mines"
                MineSweeperDirection.None
            elif (myPos.X, myPos.Y) = lastPosition && lastMove <> MineSweeperDirection.None && randomizer.Next(2) = 0 then
                printfn "Trying to break dead-lock by not moving for one iteration"
                lastMove <- MineSweeperDirection.None
                lastMove
            else
                let closestMinePos = minePositions |> Seq.sortBy (fun pos -> Math.Abs(pos.X - myPos.X) + Math.Abs(pos.Y - myPos.Y)) |> Seq.head
                printfn "Closest mine @ %dx%d (Delta: %d,%d)" closestMinePos.X closestMinePos.Y (myPos.X - closestMinePos.X) (myPos.Y - closestMinePos.Y)
                let otherSweepers = sweeperPositions |> Seq.filter (fun p -> (p.X = myPos.X && p.Y = myPos.Y) = false) |> Seq.map (fun p -> (p.X, p.Y))
                let path = pathFind otherSweepers (myPos.X, myPos.Y) (closestMinePos.X, closestMinePos.Y)
                if path.Length < 2 then MineSweeperDirection.None
                else
                    lastPosition <- (myPos.X, myPos.Y)
                    lastMove <-
                        match myPos.X - path.[1].x, myPos.Y - path.[1].y with
                        | (dx, _) when dx < 0 -> MineSweeperDirection.Right
                        | (dx, _) when dx > 0 -> MineSweeperDirection.Left
                        | (_, dy) when dy < 0 -> MineSweeperDirection.Down
                        | (_, dy) when dy > 0 -> MineSweeperDirection.Up
                        | (_, _) -> MineSweeperDirection.None
                    lastMove

        member this.Start login (password:string) (createAccount:bool) = 
            let socket = new AsyncSocket()
            let clientSweeperId:int option ref = ref Option.None
            let ticks = ref 0
            let minesCollected = ref 0

            let registerLoginMessage = MessageTypes.RegisterUser(login, password, fullName, email)
            let loginMessage = Login(login, password, agentName, agentRevision)

            let connectionSubscription = socket.ConnectionStateChanged.Subscribe (fun newState ->
                let timestamp = System.DateTime.Now.ToString("HH:mm:ss.fff")
                printfn "%s ConnectionState changed: %A" timestamp newState
            )

            let messageSubscription = socket.ReceivedMessage.Subscribe (fun e -> 
                let mutable isLoggedIn = false
                let message = ServerMessage.Parse e
                this.LastMessageReceived <- Some(DateTime.Now)
                let timestamp = System.DateTime.Now.ToString("HH:mm:ss.fff")

                match message with
                | ServerMessage.DevChallenge(version) -> 
                    printfn "%s Recevied DevChallenge version: %s" timestamp version
                    printfn "%s Sending Login message" timestamp
                    if createAccount = true then 
                        printfn "%s Sending Register Login message" timestamp
                        socket.AsyncSendString (registerLoginMessage.ToString()) |> Async.Ignore |> Async.Start
                    else 
                        printfn "%s Sending Login message" timestamp
                    socket.AsyncSendString (loginMessage.ToString()) |> Async.Ignore |> Async.Start
                | ServerMessage.Ok(correlationId) -> 
                    if correlationId = loginMessage.CorrelationId then
                        printfn "%s Received Login Ok" timestamp
                    elif correlationId = registerLoginMessage.CorrelationId then
                        printfn "%s Received Register Login Ok" timestamp
                        printfn "%s Sending Login message" timestamp
                        socket.AsyncSendString (loginMessage.ToString()) |> Async.Ignore |> Async.Start
                    else printfn "%s Received Ok(%s)" timestamp correlationId
                | ServerMessage.Error(correlationId, message) -> 
                    if correlationId = loginMessage.CorrelationId
                    then printfn "%s Received Login Error(%s, %s)" timestamp correlationId message
                    else printfn "%s Received Error(%s, %s)" timestamp correlationId message
                | ServerMessage.Scenarios(scenarios, correlationId) -> 
                    printfn "%s Received scenarios: %A, correlationId: %A" timestamp scenarios correlationId
                    let joinMessage = ScenarioJoin("minesweeper", correlationId)
                    let timestamp2 = DateTime.Now.ToString("HH:mm:ss.fff")
                    printfn "%s Sending Join 'minesweeper'" timestamp2
                    socket.AsyncSendString (joinMessage.ToString()) |> Async.Ignore |> Async.Start
                | ServerMessage.MineSweeperInit(sweeperId, worldWidth, worldHeight, maxMineCount, mineRegenerationTime) ->
                    clientSweeperId := Some(sweeperId)
                    printfn "%s Received MineSweeperInit, SweeperId: %d, World width: %d, World height: %d, Max mine count: %d, Mine regeneration time: %d" timestamp sweeperId worldWidth worldHeight maxMineCount mineRegenerationTime
                | ServerMessage.MineSweeperMineCreated(x,y) ->
                    printfn "%s Received MineCreated @ x:%d, y:%d" timestamp x y
                | ServerMessage.MineSweeperMineCollected ->
                    minesCollected := (!minesCollected)+1
                    printfn "%s Received Mine collected" timestamp
                | ServerMessage.MineSweeperState(minePositions, sweeperPositions, correlationId) ->
                    if (!clientSweeperId).IsSome then
                        let myPos = sweeperPositions |> Seq.find (fun pos -> pos.SweeperId = (!clientSweeperId).Value)
                        System.Console.Clear()
                        for y in [0..9] do
                            for x in [0..9] do
                                if (minePositions |> Seq.exists (fun mine -> mine.X = x && mine.Y = y)) then 
                                    System.Console.Write("M")
                                elif myPos.X = x && myPos.Y = y then
                                    System.Console.Write("S")
                                elif (sweeperPositions |> Seq.exists (fun sweeper -> sweeper.X = x && sweeper.Y = y)) then
                                    System.Console.Write("s")
                                else System.Console.Write(".")
                            System.Console.WriteLine()
                        printfn "%s Received state update (%s)... My position: %d x %d, Number of sweepers: %d, Number of mines: %d, Ticks: %d, Mines Collected: %d" timestamp correlationId myPos.X myPos.Y sweeperPositions.Length minePositions.Length !ticks !minesCollected
                        let direction = calculateMove minePositions sweeperPositions (!clientSweeperId).Value
                        let timestamp2 = System.DateTime.Now.ToString("HH:mm:ss.fff")
                        printfn "%s Sending move: %A (%s)" timestamp2 direction correlationId
                        socket.AsyncSendString (MineSweeperMove(direction, correlationId).ToString()) |> Async.Ignore |> Async.Start
                        ticks := (!ticks)+1
                    else
                       printfn "%s Received state update (%s)..." timestamp correlationId
                | ServerMessage.Timeout ->
                    let timestamp = System.DateTime.Now.ToString("HH:mm:ss.fff")
                    printfn "%s Received Timeout!" timestamp
            )

            socket.AsyncConnectToHost serverAddress serverPort |> Async.Start
            socket

        member val LastMessageReceived:System.DateTime option = Option.None with get, set

