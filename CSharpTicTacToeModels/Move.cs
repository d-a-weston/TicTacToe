namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        public Move(int row, int col)
        {
            Row = row;
            Col = col;
        }

        public int Row { get; }
        public int Col { get; }
    }
}
