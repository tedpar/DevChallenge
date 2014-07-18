namespace DevChallenge

module PathFinder =

    open System

    type Coordinate = {x:int;y:int}   
        with
            static member (-) (a :Coordinate , b :Coordinate) = {x=a.x-b.x ; y=a.y-b.y}
            static member (+) (a :Coordinate , b :Coordinate) = {x=a.x+b.x ; y=a.y+b.y}
            static member (*) (a :Coordinate , b :int) = {x=a.x*b ; y=a.y*b}             
            static member (/) (a :Coordinate , b :int) = {x=a.x/b ; y=a.y/b}

    type MapPoint = { coordinate:Coordinate; value:int } 
        with
            member this.Distance mp = sqrt (float(this.coordinate.x+mp.coordinate.x)**2.0 + float(this.coordinate.y+mp.coordinate.y)**2.0)

    type PathNode = { mapPoint:MapPoint; h:float; g:float; parent:PathNode option }

    let mapPointToPathNode parent goal node = { mapPoint = node; h = node.Distance goal; g = (parent.g+1.0); parent=Some(parent)}

    type Map = { width:int; height:int; map:int list }
        with
            member this.GetMapPoint x y = 
                { coordinate = { x = x; y = y }; value = this.map.[x % this.height + y * this.width] }
            member this.GetNeighborMapPoints x y =
                [ for (x',y') in [(x, y-1); (x-1, y); (x+1, y); (x, y+1)] do
                    if (y' >= 0 && x' >= 0 && x' < this.width && y' < this.height) then yield this.GetMapPoint x' y' ]

    //The A* Algorithm
    let rec aStar (map:Map) start goal (openNodes: PathNode list) (closedNodes: PathNode list) =
        let isShorter nodeA nodeB = 
            nodeA = nodeB && (nodeA.g + nodeA.h) < (nodeB.g + nodeB.h) 

        let rec checkNeighbors neighbors openNodeAcc = 
            match neighbors with
            | [] -> openNodeAcc
            | currentNode::rest ->
                let likeCurrent = fun n -> (n.mapPoint) = (currentNode.mapPoint) // Vale of n == value of current
                let containsCurrent = List.exists likeCurrent                    // List contains likeCurrent

                if openNodeAcc |> List.exists (isShorter currentNode) then // The current node is a shorter path than than one we already have.
                    let shorterPath = openNodeAcc |> List.filter (fun x -> (likeCurrent x) = false) // So remove the old one...
                    checkNeighbors rest (currentNode::shorterPath)            //...and carry on with the new one.
                elif not(containsCurrent closedNodes) && not(containsCurrent openNodeAcc) then //The current node has not been queried
                    checkNeighbors rest (currentNode::openNodeAcc)             // So add it to the open set
                else checkNeighbors rest openNodeAcc                           // else carry on

        let walkableNeighbors = 
            (map.GetNeighborMapPoints openNodes.Head.mapPoint.coordinate.x openNodes.Head.mapPoint.coordinate.y)
            |> List.filter (fun n -> n.value = 0)
            |> List.map (mapPointToPathNode openNodes.Head goal)

        let pathToGoal = walkableNeighbors |> List.tryFind (fun x -> x.mapPoint = goal) 
        if pathToGoal.IsSome then pathToGoal
        else
            let nextSet = 
                checkNeighbors walkableNeighbors openNodes.Tail
                |> List.sortBy (fun n -> (n.g + n.h))
            if nextSet.Length > 0 then
                aStar map start goal nextSet (nextSet.Head::closedNodes)
            else None //if there are no open nodes pathing has failed

    let pathFind obstacleCoordinates start goal worldWidth worldHeight = 
        let map = [
            for y in 0..worldHeight-1 do
                for x in 0..worldWidth-1 do 
                    if obstacleCoordinates |> Seq.exists (fun (pX,pY) -> pX = x && pY = y) then yield 1 else yield 0 ]

        let collisionMap = { width = worldWidth; height = worldHeight; map = map }
        let start = collisionMap.GetMapPoint (fst start) (snd start)
        let goal = collisionMap.GetMapPoint (fst goal) (snd goal)
        let endNode = aStar collisionMap start goal [{mapPoint = start; h = start.Distance goal; g = 0.0; parent = None}] []
        match endNode with 
        | None -> [] 
        | Some(n) -> 
            let rec extractPath (path:PathNode) (list:Coordinate list) =
                match path.parent.IsNone with
                | true -> path.mapPoint.coordinate::list
                | false -> extractPath path.parent.Value (path.mapPoint.coordinate::list)
            extractPath n []
