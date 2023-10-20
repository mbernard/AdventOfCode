module day09

open System
open Xunit
open Common
open FsUnit

type Route = string * int

type Graph =
    { graph: Map<string, Route seq>
      cities: Set<string> }

type dfsState =
    { graph: Graph
      visited: Set<string>
      distance: int
      best: int }

let buildGraph (s: Graph) (x: string * string array seq) =
    let fromCity = fst x
    let routes = x |> snd |> Seq.map (fun y -> y[1], int y[2])

    { graph = s.graph |> Map.add fromCity routes
      cities =
        s.cities
        |> Set.add fromCity
        |> Set.union (routes |> Seq.map fst |> Set.ofSeq) }

let rec dfs (s: dfsState) ((city, d): Route) =
    let newVisited = s.visited |> Set.add city
    let newDistance = s.distance + d

    if s.visited |> Set.contains city then
        // city already visited
        s
    elif s.best < newDistance then
        // worst then current best
        s
    elif  s.graph.cities = newVisited then
        // is a valid solution
        // visited all city once
        { s with best = newDistance }
    else
        // visit each neighbor
        s.graph.graph
        |> Map.tryFind city
        |> Option.map (fun x ->
            x
            |> Seq.map (
                dfs
                    { s with
                        visited = newVisited
                        distance = newDistance }
            )
            |> Seq.minBy (fun y -> y.best)
            |> (fun x -> if x.best < s.best then {s with best = x.best } else s))
        |> Option.defaultValue s

let findShortestPath (g: Graph) =
    g.cities
    |> Seq.map (fun x -> Route(x, 0))
    |> Seq.map (fun x ->
        dfs
            { graph = g
              visited = Set.empty
              distance = 0
              best = Int32.MaxValue }
            x)
    |> Seq.map (fun x -> x.best)
    |> Seq.min

[<Fact>]
let solve1 () =
    "../../../Solutions/09/data.txt"
    |> parseEachLine (withRegex "(.*) to (.*) = (\d+)")
    |> Seq.groupBy (fun x -> x[0])
    |> Seq.fold
        buildGraph
        { graph = Map.empty
          cities = Set.empty }
    |> findShortestPath
    |> should equal 0
