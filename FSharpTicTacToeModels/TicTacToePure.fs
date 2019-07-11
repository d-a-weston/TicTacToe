namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { row : int;
              col : int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { turn : Player;
              size : int;
              pieces : List<string> }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.turn
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = this.pieces.[(this.size * row) + col]

        // Creates a new move from row and col and returns it
        let CreateMove row col = 
            { row = row; 
              col = col }
        
        // Applies a the given move to the given game, this process involves splitting the List of pieces at the index
        // Of the move. A new game is created with the next player set to turn and the pieces replaced with the
        // Updated state.
        let ApplyMove (oldState:GameState) (move: Move) = 
            let newTurn, newPiece = 
                if oldState.turn = Nought then
                    Cross, "O"
                else
                    Nought, "X"

            // Pieces is a List<string>, to simulate a 2D board and small bit of math is used to save memory
            // This takes the row and multiplies it by the game.size, this means on a 3x3 board column 1
            // Will be {0, 1, 2}, column 2 will be {3, 4, 5} and so on.
            let split = (move.row * oldState.size) + move.col
            let newPieces = List.mapi (fun i oldPiece -> if i = split then newPiece else oldPiece) oldState.pieces

            { turn = newTurn;
              size = oldState.size;
              pieces = newPieces }


        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
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
            // Gets all the pieces from the associated line as a sequence
            let pieces = line 
                         |> Seq.map (fun (row, col) -> (game :> ITicTacToeGame<Player>).getPiece(row, col))

            // Checks for a draw first, then if it's undecided, and if it's neither of those, determine
            // Which player won.
            if Seq.exists ((=) "O") pieces && Seq.exists ((=) "X") pieces then
                Draw
            else if Seq.exists ((=) "") pieces  then
                Undecided
            else if Seq.exists ((=) "O") pieces  then
                Win (Nought, line)
            else
                Win (Cross, line)
            
        // Check all lines in a game and determine the outcome between Undecided, Draw or a Win
        let GameOutcome game =
            
            // Get all lines from the board and check each line, storing the result in a sequence, which is then
            // Converted to a list to be used in a recursive match function
            let outcomes = game.size
                           |> Lines
                           |> Seq.map (fun line -> CheckLine game line)
                           |> Seq.toList

            // Checks each outcome, if it's a winner, stop checking and return it, if there's an undecided line
            // Raise a flag and call checkOutcomes again. If the list is empty and isDecided is false, then
            // The outcome must be a draw, otherwise it is undecided
            let rec checkOutcomes lines isUndecided = 
                match lines with
                | [] -> if isUndecided then Undecided else Draw
                | head::tail -> 
                    match head with
                    | Win(winner = w; line = l) -> Win (w, l)
                    | Undecided -> checkOutcomes tail true
                    | _ -> checkOutcomes tail isUndecided

            checkOutcomes outcomes false

        // Creates and returns a new game with turn set to first player, size to size
        // And pieces is a list of length size containing ""
        let GameStart (firstPlayer:Player) size = 
            { turn = firstPlayer; 
              size = size;
              pieces = List.init (size*size) (fun index -> "") }

        // Return a score based on the games state, 1 for a win, -1 for a loss, 0 for a draw or undecided
        let HeuristicScore game perspective = 
            match GameOutcome game with
            | Win (winner = w) -> if perspective = w then 1 else -1
            | _ -> 0

        // Find all available moves for the current board state
        let MoveGenerator game = 
            seq {   for row in 0 .. game.size - 1 do 
                        for col in 0 .. game.size - 1 do
                            yield CreateMove row col }
            |> Seq.filter (fun move -> (game :> ITicTacToeGame<Player>).getPiece(move.row, move.col) = "") 

        // Test to see if a game is finished
        let GameOver game = 
            match GameOutcome game with
            | Undecided -> false
            | _ -> true

        // Find out who's turn it is
        let GetTurn game =
            game.turn

        let MiniMax game = 
            let generatedMiniMax = GameTheory.MiniMaxGenerator HeuristicScore GetTurn GameOver MoveGenerator ApplyMove
            generatedMiniMax game game.turn

        let MiniMaxWithPruning game = 
            let generatedAlphaBeta = GameTheory.MiniMaxWithAlphaBetaPruningGenerator HeuristicScore GetTurn GameOver MoveGenerator ApplyMove
            generatedAlphaBeta -1 1 game game.turn

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = Option.get(fst(MiniMax game))

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = Option.get(fst(MiniMaxWithPruning game))