module Bandwagoner

type Coach = { Name: string; FormerPlayer: bool }
type Stats = { Wins: int; Losses: int }

type Team =
    { Name: string
      Coach: Coach
      Stats: Stats }

let createCoach (name: string) (formerPlayer: bool) : Coach =
    { Name = name
      FormerPlayer = formerPlayer }

let createStats (wins: int) (losses: int) : Stats = { Wins = wins; Losses = losses }

let createTeam (name: string) (coach: Coach) (stats: Stats) : Team =
    { Name = name
      Coach = coach
      Stats = stats }

let replaceCoach (team: Team) (coach: Coach) : Team = { team with Coach = coach }

let isSameTeam (homeTeam: Team) (awayTeam: Team) : bool = homeTeam = awayTeam

let rootForTeam (team: Team) : bool =

    match (team.Coach, team.Stats) with
    | (coach, _) when coach.Name = "Gregg Popovich" -> true
    | (coach, _) when coach.FormerPlayer -> true
    | (_, stats) when stats.Wins >= 60 -> true
    | (_, stats) when stats.Losses > stats.Wins -> true
    | _ when team.Name = "Chicago Bulls" -> true
    | _ -> false
