using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        
        // Instantiates a new game with a player and gameSize
        // A 2D Array is created to store the state of pieces and is filled with "" to begin
        public Game(Player first, int gameSize)
        {
            Size = gameSize;
            Turn = first;

            Pieces = new string[Size, Size];
            for (int row = 0; row < Size; row++)
            {
                for (int col = 0; col < Size; col++)
                {
                    Pieces[row, col] = "";
                }
            }
        }

        public int Size { get; }
        public Player Turn { get; set; }
        public string[,] Pieces { get; set; }

        public string getPiece(int row, int col)
        {
            return Pieces[row, col];
        }

        public void applyMove(Move move, string piece)
        {
            Pieces[move.Row, move.Col] = piece;
        }

        public void undoMove(Move move)
        {
            Pieces[move.Row, move.Col] = "";
        }
    }
}