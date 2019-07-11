namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = Nought | Cross

        type GameState = 
            { mutable turn : Player;
              size : int;
              pieces : string[,] }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = this.pieces.[row, col]

        type Move = 
            { row : int;
              col : int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        let CreateMove row col   = 
            { row = row; 
              col = col }

        let GameStart first size = 
            { turn = first;
              size = size;
              pieces =  Array2D.create size size "" }

        let Lines (size:int) : seq<seq<int*int>> = 
            seq {
                for x in 0..size - 1 do
                    yield seq { 
                        for y in 0..size - 1 do
                            yield (x, y)
                    }
                    yield seq {
                        for y in 0..size - 1 do 
                            yield(y, x)
                    }
                yield seq {
                    for x in 0..size - 1 do
                        for y in 0..size - 1 do
                            if x = y then
                                yield(x, y)
                }
                yield seq {
                    for x in 0..size - 1 do
                        for y in 0..size - 1 do
                            if x + y = size - 1 then
                                yield(x, y)
                }    
            }

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            let pieces = line  
                         |> Seq.map (fun (row, col) -> game.pieces.[row, col])

            if Seq.exists ((=) "O") pieces && Seq.exists ((=) "X") pieces then
                Draw
            else if Seq.exists ((=) "") pieces  then
                Undecided
            else if Seq.exists ((=) "O") pieces  then
                Win (Nought, line)
            else
                Win (Cross, line)
            
        // Recursively checks all the lines of a gameboard to determine the current outcome
        let GameOutcome game =
            let rec checkOutcomes lines isUndecided = 
                match lines with
                | [] -> if isUndecided then Undecided else Draw
                | head::tail -> 
                    match head with
                    | Win(winner = w; line = l) -> Win (w, l)
                    | Undecided -> checkOutcomes tail true
                    | _ -> checkOutcomes tail isUndecided

            let outcomes = game.size
                           |> Lines
                           |> Seq.map (fun line -> CheckLine game line)
                           |> Seq.toList

            checkOutcomes outcomes false
                
        // Applies a move to the board and changes the turn
        // This is done using mutability, so a new game isn't created, just altered
        let ApplyMove game move  = 
            game.pieces.[move.row, move.col] <- if game.turn = Nought then "O" else "X"
            game.turn <- if game.turn = Nought then Cross else Nought
            game

        // Undoes a the move from the board and changes the turn
        // This is done using mutability, so the game is altered
        let UndoMove game move = 
            game.pieces.[move.row, move.col] <- ""
            game.turn <- if game.turn = Nought then Cross else Nought

        // Find all available moves for the current board state
        let MoveGenerator game = 
            seq {   for row in 0 .. game.size - 1 do 
                        for col in 0 .. game.size - 1 do
                            yield CreateMove row col }
            |> Seq.filter (fun move -> game.pieces.[move.row, move.col] = "") 
            |> Seq.toList

        // Utilises MiniMax with Alpha/Beta pruning and mutability to find the best move from any board state
        let FindBestMove game =
            let rec MiniMax alpha beta game perspective =
                NodeCounter.Increment()
                if GameOutcome game <> Undecided then
                    match GameOutcome game with
                    | Win (winner = w) -> if perspective = w then (None, 1) else (None, -1)
                    | _ -> (None, 0)
                else
                    let possibleMoves = MoveGenerator game
                    let mutable searching = true
                    let mutable alpha = alpha
                    let mutable beta = beta

                    if game.turn = perspective then
                        let mutable result = (Some possibleMoves.[0], alpha)
                        for move in possibleMoves do
                            if searching then
                                
                                // The move is applied and MiniMax is run against it. Because of mutability
                                // The move has to be undone afterwords otherwise any other checks would
                                // Contain the change, skewing the result
                                ApplyMove game move |> ignore
                                alpha <- max alpha (snd(MiniMax alpha beta game perspective))
                                if alpha > snd(result) then
                                    result <- (Some move, alpha)
                                UndoMove game move

                                if alpha >= beta then 
                                    searching <- false
                        result
                    else
                        let mutable result = (Some possibleMoves.[0], beta)
                        for move in possibleMoves do
                            if searching then
                                ApplyMove game move |> ignore
                                beta <- min beta (snd(MiniMax alpha beta game perspective))
                                if beta < snd(result) then
                                    result <- (Some move, beta)
                                UndoMove game move

                                if alpha >= beta then 
                                    searching <- false
                        result
            NodeCounter.Reset()

            Option.get(fst(MiniMax -1 1 game game.turn))

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game