public class Queen
{
    const int MinPosition = 0;
    const int MaxPosition = 7;

    public Queen(int row, int column)
    {
        if (row < MinPosition || row > MaxPosition ||
            column < MinPosition || column > MaxPosition)
            throw new ArgumentOutOfRangeException();

        Row = row;
        Column = column;
    }

    public int Row { get; }
    public int Column { get; }

    public bool SharesRowWith(Queen other) => Row == other.Row;
    public bool SharesColumnWith(Queen other) => Column == other.Column;
    public bool SharesDiagonalWith(Queen other) =>
        Math.Abs(Row - other.Row) == Math.Abs(Column - other.Column);
}

public static class QueenAttack
{
    public static Queen Create(int row, int column) => new(row, column);

    public static bool CanAttack(Queen white, Queen black) =>
        white.SharesRowWith(black) ||
        white.SharesColumnWith(black) ||
        white.SharesDiagonalWith(black);
}