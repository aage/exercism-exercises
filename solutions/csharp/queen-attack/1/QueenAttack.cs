public class Queen
{
    public Queen(int row, int column)
    {
        if (row < 0 || column < 0 || row > 7 || column > 7)
            throw new ArgumentOutOfRangeException();

        Row = row;
        Column = column;
    }

    public int Row { get; }
    public int Column { get; }
}

public static class QueenAttack
{
    static int Distance(int lhs, int rhs) => Math.Abs(lhs - rhs);

    public static Queen Create(int row, int column) => new(row, column);

    public static bool CanAttack(Queen white, Queen black)
    {
        if (white.Column == black.Column) return true;
        if (white.Row == black.Row) return true;
        if (Distance(white.Row, black.Row)
            == Distance(white.Column, black.Column)) return true;
        return false;
    }
}