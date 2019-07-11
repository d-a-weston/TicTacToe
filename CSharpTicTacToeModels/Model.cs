using System;
using System.Collections.Generic;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Nought;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        // Apply move to game, swapping the turn
        public Game ApplyMove(Game game, Move move)
        {
            game.applyMove(move, game.Turn == Nought ? "O" : "X");
            game.Turn = game.Turn == Nought ? Cross : Nought;
            return game;
        }
        
        // Undo move to game, swapping the turn
        public Game UndoMove(Game game, Move move)
        {
            game.undoMove(move);
            game.Turn = game.Turn == Nought ? Cross : Nought;
            return game;
        }

        // Create a new move with row and col
        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }

        // Calls MiniMax and returns the best move to take based on the current board state
        public Move FindBestMove(Game game)
        {
            NodeCounter.Reset();
            return MiniMaxWithAlphaBeta(-1, 1, game, game.Turn).Item1;
        }

        // MiniMax with alpha and beta pruning, called recursively
        // Takes alpha and beta as ints, the game and who's perspective is being measured
        public Tuple<Move, int> MiniMaxWithAlphaBeta(int alpha, int beta, Game game, Player perspective)
        {
            NodeCounter.Increment();
            var outcome = GameOutcome(game);

            // If the game is not undecided it is a gameover, return the heuristic score
            if (outcome != TicTacToeOutcome<Player>.Undecided) {
                
                // If it's a win, check whether it is a win for perspective and return 1
                // If the opponent won, return -1
                if (outcome.IsWin)
                {
                    var win = outcome as TicTacToeOutcome<Player>.Win;
                    return Tuple.Create(new Move(0, 0), win.winner == perspective ? 1 : -1);
                }

                // If it's not a win or undecided, it must be a draw and return 0
                else
                {
                    return Tuple.Create(new Move(0, 0), 0);
                }
            }
            else
            {
                // Get all the possible moves
                List<Move> possibleMoves = MoveGenerator(game);

                // Check to see whether this pass in Minimizing or Maximizing
                if (game.Turn == perspective)
                {
                    // Initialise the result to be the first move and alpha
                    Tuple<Move, int> result = Tuple.Create(possibleMoves[0], alpha);

                    // For each possible move, check the outcome
                    foreach (Move move in possibleMoves)
                    {
                        // Apply the move, return the heuristic score from the MiniMax of this state
                        // Then undo the move to allow for further games to be checked
                        ApplyMove(game, move);
                        int value = MiniMaxWithAlphaBeta(alpha, beta, game, perspective).Item2;
                        UndoMove(game, move);

                        // If the value of the game is better than the alpha, it becomes the new best move
                        // Then alpha becomes the greater of alpha and value
                        // If the new alpha is better than beta, no more games need to be checked
                        if (value > alpha) { result = Tuple.Create(move, value); }
                        alpha = Math.Max(alpha, value);
                        if (alpha >= beta) { break; }
                    }

                    // Return the best move found
                    return result;
                }
                else
                {
                    // Initialise the result to be the first move and beta
                    Tuple<Move, int> result = Tuple.Create(possibleMoves[0], beta);

                    // For each possible move, check the outcome
                    foreach (Move move in possibleMoves)
                    {
                        // Apply the move, return the heuristic score from the MiniMax of this state
                        // Then undo the move to allow for further games to be checked
                        ApplyMove(game, move);
                        int value = MiniMaxWithAlphaBeta(alpha, beta, game, perspective).Item2;
                        UndoMove(game, move);

                        // If the value of the game is worse than the beta, it becomes the new worst move
                        // Then beta becomes the lesser of beta and value
                        // If the new beta is better than alpha, no more games need to be checked
                        if (value < beta) { result = Tuple.Create(move, value); }
                        beta = Math.Min(beta, value);
                        if (alpha >= beta) { break; }
                    }

                    // Return the best move found
                    return result;
                }
            }
        }

        // Returns a list of possible moves, move which currently have no piece in the board state
        public List<Move> MoveGenerator(Game game)
        {
            List<Move> possibleMoves = new List<Move>();

            for (int row = 0; row < game.Size; row++)
            {
                for (int col = 0; col < game.Size; col++)
                {
                    if (game.getPiece(row, col) == "")
                    {
                        possibleMoves.Add(new Move(row, col));
                    }
                }
            }

            return possibleMoves;
        }

        // Returns the outcome of a current game state
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            List<List<Tuple<int, int>>> lines = GetLines(game);
            bool isUndecided = false;

            // Check each line from GetLines
            foreach (List<Tuple<int, int>> line in lines)
            {
                // Flags for checking the outcome of a line
                bool hasNought = false;
                bool hasCross = false;
                bool hasEmpty = false;

                // For each piece in the line, check for the existance of X's, O's and empty spaces
                // Set each flag when appropriate
                foreach (Tuple<int, int> piece in line)
                {
                    if (game.getPiece(piece.Item1, piece.Item2) == "O") { hasNought = true; }
                    else if (game.getPiece(piece.Item1, piece.Item2) == "X") { hasCross = true; }
                    else { hasEmpty = true; }
                }

                // If a line has a nought and cross, it's a draw
                if (hasNought && hasCross) { }
                // If a line doesn't have both a nought and cross, but has an empty space, it's an undecided line
                else if (hasEmpty) { isUndecided = true; }
                // If it's not a draw or undecided, the outcome must be a win of some player
                // This win is returned, breaking the loop, no further outcomes matter
                else if (hasNought) { return TicTacToeOutcome<Player>.NewWin(Nought, line); }
                else { return TicTacToeOutcome<Player>.NewWin(Cross, line); }
            }

            // If the game hasn't been won, and has a single undecided line it is undecided
            // Otherwise it is a draw
            if (isUndecided) { return TicTacToeOutcome<Player>.Undecided;  }
            else { return TicTacToeOutcome<Player>.Draw; }
        }

        // Returns a list containing lists of tuples, which represent the possible lines on the board
        public List<List<Tuple<int, int>>> GetLines (Game game)
        {
            List<List<Tuple<int, int>>> lines = new List<List<Tuple<int, int>>>();
            List<Tuple<int, int>> diag1 = new List<Tuple<int, int>>();
            List<Tuple<int, int>> diag2 = new List<Tuple<int, int>>();

            // Loop through the possible board positions to assemble the lines and add them to the list
            for (int x = 0; x < game.Size; x++)
            {
                List<Tuple<int, int>> curHoriz = new List<Tuple<int, int>>();
                List<Tuple<int, int>> curVert = new List<Tuple<int, int>>();

                for (int y = 0; y < game.Size; y++)
                {
                    curHoriz.Add(Tuple.Create(x, y));
                    curVert.Add(Tuple.Create(y, x));
                    
                    if (x == y) { diag1.Add(Tuple.Create(x, y)); }
                    if (x + y == game.Size - 1) { diag2.Add(Tuple.Create(x, y)); }
                }

                lines.Add(curHoriz);
                lines.Add(curVert);
            }

            lines.Add(diag1);
            lines.Add(diag2);

            return lines;
        }
        public Game GameStart(Player first, int size)
        {
            return new Game(first, size);
        }
    }
}