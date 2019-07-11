namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =
                NodeCounter.Increment()
                // If it's an end state, return the heuristic score from the perspective provided
                if gameOver game then
                    (None, heuristic game perspective)
                // If it's not an end state, run MiniMax recursively
                else
                    // Generate all possible moves, apply them to the game storing the move and it's gameState
                    // Then run Minimax against each game state, ending with a Sequence of tuples containing
                    // A Move and the Score it generated
                    let outcomes = moveGenerator game
                                   |> Seq.map (fun move -> (move, applyMove game move))
                                   |> Seq.map (fun (move, newGame) -> (Some move, snd(MiniMax newGame perspective)))
                    
                    // If the current state is the perspectives turn, then get the best result
                    if getTurn game = perspective then 
                        outcomes
                        |> Seq.maxBy snd
                    // If it's the opposing player's turn, get the worst result
                    else
                        outcomes
                        |> Seq.minBy snd
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                // If it's an end state, return the heuristic score from the perspective provided
                if gameOver oldState then
                    (None, heuristic oldState perspective)
                else
                    // Generate all possible moves, then apply them to the current game, keeping a tuple of (Move, Game)
                    // Then convert this to a list to be used in Min and Max
                    let newGames =  moveGenerator oldState
                                    |> Seq.map (fun move -> (move, applyMove oldState move))
                                    |> Seq.toList

                    // If it's the perspective player's turn find the best result
                    if getTurn oldState = perspective then 
                        // Fold the List of (Move, Game) with an accumulator. This accumulator is an appended list of
                        // Of the scores from each state. To determine whether a best state has been found the alpha
                        // Is passed in as an option. This option is set to None when a best result is found.
                        List.fold(fun results (move, newGame) ->
                                        // If there is an alpha then continue
                                        if Option.isSome(snd results) then
                                            // Find the best value between the current alpha and the outcome of the current game
                                            let newAlpha = max (Option.get(snd results)) (snd(MiniMax (Option.get(snd results)) beta newGame perspective))
                                            
                                            // If the new alpha is better or equal to beta, then we've found the best result
                                            // This move is appended to the list with it's score, and the alpha is set to none
                                            // So the search won't run MiniMax against any other games.
                                            if newAlpha >= beta then
                                                ((fst results)@[(Some move, newAlpha)], None)

                                            // If the new alpha isn't better, append the move and score to the list and pass through
                                            // The current alpha to be used in the next search
                                            else
                                                ((fst results)@[(Some move, newAlpha)], Some newAlpha)

                                        // If there is no alpha, append empty games to the list
                                        else
                                            ((fst results)@[(None, -1)], None)
                        ) ([], Some alpha) newGames
                        
                        // Get the list of results from the fold and return the best move MiniMax found
                        |> fst
                        |> List.maxBy snd
                       
                    else
                        // The same process is repeated here as above, but from the perspective of the minimizing player
                        // So the worst results are added to the list until it is better than or equal to alpha
                        // At which point it stops searching and returns the result
                        List.fold(fun results (move, newGame) ->
                                        if Option.isSome(snd results) then
                                            let newBeta = min (Option.get(snd results)) (snd(MiniMax alpha (Option.get(snd results)) newGame perspective))
                                            
                                            if alpha >= newBeta then
                                                ((fst results)@[(Some move, newBeta)], None)
                                            else
                                                ((fst results)@[(Some move, newBeta)], Some newBeta)
                                        else
                                            ((fst results)@[(None, 0)], None)
                        ) ([], Some beta) newGames
                        |> fst
                        |> List.minBy snd
            NodeCounter.Reset()
            MiniMax
