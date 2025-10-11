module ResistorColorDuo

// based on: https://exercism.org/tracks/fsharp/exercises/resistor-color-duo/solutions/ErikSchierboom

let private colors =
    [ "black"
      "brown"
      "red"
      "orange"
      "yellow"
      "green"
      "blue"
      "violet"
      "grey"
      "white" ]

let private colorCode c = List.findIndex ((=) c) colors

let value (colors: string list) =
    colorCode colors.[0] * 10 + colorCode colors.[1]
