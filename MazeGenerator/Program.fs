open System


//Constants for the size of the maze
let WIDTH = 40
let HEIGHT = 40

//Edge direction enum
type direction =
    | North = 1
    | West = 2

//Tree vertex type that stores the cell it contains and a reference to the parent (or Null if it is a root)
type vertex() =
    let mutable parent: option<vertex ref> = None

    member this.GetParent: option<vertex ref> = parent

    member this.SetParent(p: option<vertex ref>) = parent <- p

//Main class that represents the union find data structure and other methods used for generation
type board() =
    
    //Generate a matrix to store all the vertex nodes
    let vertexMatrix =
        [| for _ = 0 to WIDTH - 1 do
               [| for _ = 0 to HEIGHT - 1 do
                      vertex () |] |]

    
    //Generate all the possible edges of pairs of cells
    let mutable edges =
        [ for y = 0 to HEIGHT - 1 do
              for x = 0 to WIDTH - 1 do
                  if y > 0 then
                      (x, y, direction.North)

                  if x > 0 then
                      (x, y, direction.West) ]

    let mutable edgesSeen = edges
    member this.GetVertex(x: int, y: int) : vertex = vertexMatrix[y][x]

    //Selects a random edge from those not yet visited and checks it against the criteria
    member this.ComputeRandomEdge() =
        let rnd = Random().Next(edgesSeen.Length)
        let edge = edges[rnd]
        edgesSeen <- List.removeAt rnd edgesSeen

        let cell1 =
            match edge with
            | x, y, _ -> this.GetVertex(x, y)

        let cell2 =
            match edge with
            | x, y, d ->
                if d = direction.North then
                    this.GetVertex(x, y - 1)
                else
                    this.GetVertex(x - 1, y)

        if this.Find(ref cell1) <> this.Find(ref cell2) then
            edges <- List.removeAt rnd edges
            this.Join(ref cell1, ref cell2)

    //Finds the root node of a given vertex
    member this.Find(v: vertex ref) : vertex ref =
        let root =
            match v.contents.GetParent with
            | None -> v
            | _ -> this.Find(v.contents.GetParent.Value)

        if (root <> v) then
            v.contents.SetParent(Some(root))

        root

    //Finds the root node of a given vertex and also returns the depth of the tree
    member this.FindDepth(v: vertex ref * int) : vertex ref * int =
        let root =
            match v with
            | l, r ->
                match l.contents.GetParent with
                | None -> (l, r)
                | _ -> this.FindDepth(l.contents.GetParent.Value, r + 1)

        if (fst root <> fst v) then
            (fst v).contents.SetParent(Some(fst root))

        root

    //Joins the trees of two given vertexes together. Parenting the one with the smaller depth under the other
    member this.Join(v1: vertex ref, v2: vertex ref) =
        let r1 = this.FindDepth(v1, 0)
        let r2 = this.FindDepth(v2, 0)

        if snd r1 >= snd r2 then
            (fst r2).contents.SetParent(Some(fst r1))
        else
            (fst r1).contents.SetParent(Some(fst r2))

    //Converts the list of walls into a form that is displayable (and displays it)
    member this.MatrixPrint() =
        let stringMatrix =
            [| for _ = 0 to WIDTH - 1 do
                   [| for _ = 0 to HEIGHT - 1 do
                          "  " |] |]

        for wall in edges do
            match wall with
            | x, y, d ->
                if d = direction.West then
                    if stringMatrix[y][x] = "__" then
                        stringMatrix[y][x] <- "|_"
                    else
                        stringMatrix[y][x] <- "| "
                else if stringMatrix[y - 1][x] = "| " then
                    stringMatrix[y - 1][x] <- "|_"
                else
                    stringMatrix[y - 1][x] <- "__"

        for line in stringMatrix do
            Console.WriteLine(Array.fold (fun s v -> s + v) "" line)

    
    //Main loop for the generation. (Thread.Sleep present for displaying)
    member this.Run() =
        while edgesSeen.Length > 0 do
            Console.Clear()
            this.MatrixPrint()
            this.ComputeRandomEdge()
            Threading.Thread.Sleep(10)

let maze = board ()
maze.Run()
Console.WriteLine("===DONE===")
Console.ReadKey() |> ignore
