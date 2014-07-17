namespace DevChallenge

    module MessageTypes =
        open System
        open System.Linq
        open System.Xml
        open System.Xml.Linq

        type RegisterUser(login:string, password:string, fullname:string, email:string) =
            let correlationId = Guid.NewGuid().ToString()
            member this.CorrelationId = correlationId
            override this.ToString() =
                let request =
                    XElement(XName.Get("request"), XAttribute(XName.Get("id"), correlationId),
                        XElement(XName.Get("user.register"),
                            XElement(XName.Get("login"), login),
                            XElement(XName.Get("password"), password),
                            XElement(XName.Get("fullname"), fullname),
                            XElement(XName.Get("email"), email)
                        )
                    )
                request.ToString()

        type Login(login:string, password:string, agentname:string, agentrevision:string) =
            let correlationId = Guid.NewGuid().ToString()
            member this.CorrelationId = correlationId
            override this.ToString() =
                let request =
                    XElement(XName.Get("request"), XAttribute(XName.Get("id"), correlationId),
                        XElement(XName.Get("devchallenge.login"), 
                            XElement(XName.Get("login"), login),
                            XElement(XName.Get("password"), password),
                            XElement(XName.Get("agentname"), agentname),
                            XElement(XName.Get("agentrevision"), agentrevision)
                        )
                    )
                request.ToString()

        type ScenarioJoin(scenario:string, correlationId:string) =
            override this.ToString() =
                let response =
                    XElement(XName.Get("response"), XAttribute(XName.Get("id"), correlationId),
                        XElement(XName.Get("scenario.join"), XAttribute(XName.Get("name"), scenario))
                    )
                response.ToString() 

        type MineSweeperDirection = Up | Down | Left | Right | None

        type MineSweeperMove(direction:MineSweeperDirection, id:string) =
            override this.ToString() =
                let (info, o) = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(direction, typedefof<MineSweeperDirection>)
                let directionName = info.Name
                let response =
                    XElement(XName.Get("response"), XAttribute(XName.Get("id"), id),
                        XElement(XName.Get("direction"), directionName)
                    )
                response.ToString() 

        type MinePosition(x:int, y:int) =
            member this.X = x
            member this.Y = y

        type SweeperPosition(sweeperId: int, x:int, y:int) =
            member this.X = x
            member this.Y = y
            member this.SweeperId = sweeperId

        type ClientMessage = 
            | RegisterUserMessage of RegisterUser
            | LoginMessage of Login
            | ScenarioJoinMessage of ScenarioJoin
            | MineSweeperMoveMessage of MineSweeperMove

        type ServerMessage =
            | DevChallenge of version:string
            | Ok of id:string
            | Error of id:string * message:string
            | Scenarios of scenarios:string list * correlationId:string
            | MineSweeperInit of sweeperId:int * worldWidth:int * worldHeight:int * maxMineCount:int * mineRegenerationTime:int
            | MineSweeperState of minePositions:MinePosition list * sweeperPositions:SweeperPosition list * correlationId:string
            | MineSweeperMineCreated of x:int * y:int
            | MineSweeperMineCollected
            | Timeout
            with 
            static member Parse(xml:XElement) =
                match xml.Name.ToString().ToLower() with
                | "notification" ->
                    let elem = xml.Elements().First()
                    match elem.Name.ToString().ToLower() with
                    | "devchallenge" -> DevChallenge(xml.Elements().First().Attribute(XName.Get("version")).Value.ToString())
                    | "ok" -> Ok("")
                    | "error" -> Error("", xml.Elements().First().Attribute(XName.Get("message")).Value.ToString())
                    | "init" ->
                        let sweeperId = int (elem.Attribute(XName.Get("sweeperid")).Value.ToString())
                        let worldWidth = int (elem.Attribute(XName.Get("worldwidth")).Value.ToString())
                        let worldHeight = int (elem.Attribute(XName.Get("worldheight")).Value.ToString())
                        let maxMineCount = int (elem.Attribute(XName.Get("maxminecount")).Value.ToString())
                        let mineRegenerationTime = int (elem.Attribute(XName.Get("mineregenerationtime")).Value.ToString())
                        MineSweeperInit(sweeperId, worldWidth, worldHeight, maxMineCount, mineRegenerationTime)
                    | "minecreated" ->
                        let x = int (elem.Attribute(XName.Get("x")).Value.ToString())
                        let y = int (elem.Attribute(XName.Get("y")).Value.ToString())
                        MineSweeperMineCreated(x, y)
                    | "minecollected" -> 
                        MineSweeperMineCollected
                    | "timeout" ->
                        Timeout
                    | _ -> failwith "Unhandled message type"
                | "response" ->
                    let correlationId = xml.Attribute(XName.Get("id")).Value.ToString()
                    let elem = xml.Elements().First()
                    match elem.Name.ToString().ToLower() with
                    | "ok" -> Ok(correlationId)
                    | "error" -> Error(correlationId, xml.Elements().First().Attribute(XName.Get("message")).Value.ToString())
                    | _ -> failwith "Unhandled message type"
                | "request" ->
                    let correlationId = xml.Attribute(XName.Get("id")).Value.ToString()
                    let elem = xml.Elements().First()
                    match elem.Name.ToString().ToLower() with
                    | "state" ->
                        let minePositions =
                            elem.Element(XName.Get("mines")).Elements(XName.Get("mine"))
                            |> Seq.map (fun e -> MinePosition(int (e.Attribute(XName.Get("x")).Value.ToString()), int (e.Attribute(XName.Get("y")).Value.ToString())))
                            |> Seq.toList
                        let sweeperPositions =
                            elem.Element(XName.Get("sweepers")).Elements(XName.Get("sweeper"))
                            |> Seq.map (fun e -> SweeperPosition(int (e.Attribute(XName.Get("id")).Value.ToString()), int (e.Attribute(XName.Get("x")).Value.ToString()), int (e.Attribute(XName.Get("y")).Value.ToString())))
                            |> Seq.toList
                        MineSweeperState(minePositions, sweeperPositions, correlationId)
                    | "scenario.select" -> 
                        let scenarios =
                            elem.Element(XName.Get("scenarios")).Elements(XName.Get("scenario"))
                            |> Seq.map (fun e -> e.Attribute(XName.Get("name")).Value.ToString())
                            |> Seq.toList
                        Scenarios(scenarios, correlationId)
                    | _ -> failwith "Unhandled message type"
                | _ -> failwith "Unhandled message type"

        type Message = ClientMessage | ServerMessage
