public enum Plant { Violets = 1, Radishes, Clover, Grass }

public class KindergartenGarden(string diagram)
{
    static Plant GetPlant(char c) => c switch
    {
        'R' => Plant.Radishes,
        'V' => Plant.Violets,
        'G' => Plant.Grass,
        'C' => Plant.Clover,
        _ => throw new Exception()
    };

    readonly List<string> _students =
        [ "Alice", "Bob", "Charlie", "David",
          "Eve", "Fred", "Ginny", "Harriet",
          "Ileana", "Joseph", "Kincaid", "Larry" ];

    readonly string[] _diagram = diagram.Split('\n');

    public Plant[] Plants(string student) =>
        [.. _diagram
            .SelectMany(x => x
                .Skip(_students.IndexOf(student) * 2)
                .Take(2)
                .Select(GetPlant))];
}