public static class Proverb
{
    public static string[] Recite(string[] subjects)
    {
        IEnumerable<(string, string)> Pairs()
        {
            for (var idx = 0; idx < subjects.Length - 1; idx++)
                yield return (subjects[idx], subjects[idx + 1]);
        }

        IEnumerable<string> Loop()
        {
            foreach (var pair in Pairs())
                yield return $"For want of a {pair.Item1} the {pair.Item2} was lost.";
            yield return $"And all for the want of a {subjects[0]}.";
        }

        return subjects.Length == 0
            ? System.Array.Empty<string>()
            : Loop().ToArray();
    }
}