module Gigasecond

open System

let add (beginDate: DateTime) : DateTime =
    let gigasecond = TimeSpan.FromSeconds(1000000000)
    beginDate + gigasecond
