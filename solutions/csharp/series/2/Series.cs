using System.Linq;

public static class Series
{
    public static string[] Slices(string nums, int length)
    {
        if (length > nums.Length || length < 1 || nums == "")
            throw new ArgumentException("NoNo");
        else
        {
            IEnumerable<string> Loop()
            {
                foreach (var idx in nums.Select((_, idx) => idx))
                {
                    var next = nums.Skip(idx).Take(length).ToArray();
                    if (next.Count() == length) yield return new string(next);
                    else break;
                }
            }

            return Loop().ToArray();
        }
    }
}