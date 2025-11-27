public static class EliudsEggs
{
    public static int EggCount(int encodedCount) =>
         Convert
            .ToString(encodedCount, 2)
            .Sum(c => (int)char.GetNumericValue(c));
}
