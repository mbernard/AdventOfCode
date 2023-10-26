module day09

open System
open Xunit
open Common
open FsUnit

type Region = {
    citySet : Set<string>
    routes : Map<string, Map<string,int>>
}

type Route = {
    cost : int
    path : string list
}

let getNonVisitedNeighbors region visited currentCity =
    region.routes[currentCity]
    |> Map.filter (fun k _ -> visited |> List.contains k |> not)

let findShortestPath region =
    let cityCount = region.citySet |> Set.count
    let getNonVisitedNeighbors = getNonVisitedNeighbors region

    let rec findShortestPathRec currentCity cost (path : string list) =
        if path |> List.length = cityCount then
            // possible solution
            Some {cost = cost; path = path}
        else
            getNonVisitedNeighbors path currentCity
            |> Map.fold (fun bestRoute k v -> 
                match findShortestPathRec k ( cost + v) (k :: path) with
                | Some x -> if bestRoute |> Option.isNone || x.cost < bestRoute.Value.cost then Some x else bestRoute
                | None -> bestRoute
                ) None
    
    region.citySet
    |> Set.fold (fun bestRoute city -> 
        match findShortestPathRec city 0 [city] with
        | Some x -> if bestRoute |> Option.isNone || x.cost < bestRoute.Value.cost then Some x else bestRoute
        | None -> bestRoute
    ) None    
    |> Option.get  

let findLongestPath region =
    let cityCount = region.citySet |> Set.count
    let getNonVisitedNeighbors = getNonVisitedNeighbors region

    let rec findLongestPathRec currentCity cost (path : string list) =
        if path |> List.length = cityCount then
            // possible solution
            Some {cost = cost; path = path}
        else
            getNonVisitedNeighbors path currentCity
            |> Map.fold (fun bestRoute k v -> 
                match findLongestPathRec k ( cost + v) (k :: path) with
                | Some x -> if bestRoute |> Option.isNone || x.cost > bestRoute.Value.cost then Some x else bestRoute
                | None -> bestRoute
                ) None
    
    region.citySet
    |> Set.fold (fun bestRoute city -> 
        match findLongestPathRec city 0 [city] with
        | Some x -> if bestRoute |> Option.isNone || x.cost > bestRoute.Value.cost then Some x else bestRoute
        | None -> bestRoute
    ) None    
    |> Option.get    
      
let addRoute region (xs : string array) =
    let s,d,x = xs[0], xs[1], int xs[2]
    let cs = 
        region.citySet 
        |> Set.add s
        |> Set.add d

    let rs = 
        region.routes
        |> Map.add s
            (region.routes 
            |> Map.tryFind s
            |> Option.defaultValue Map.empty
            |> Map.add d (int x))
        |> Map.add d
            (region.routes
            |> Map.tryFind d
            |> Option.defaultValue Map.empty
            |> Map.add s (int x))

    {citySet = cs; routes = rs}

let parseRegion (xs : string array seq) =
    xs 
    |> Seq.fold addRoute { citySet = Set.empty; routes = Map.empty }
   

[<Theory>]
[<InlineDataAttribute(605, "../../../Solutions/09/data_test.txt")>]
[<InlineDataAttribute(207, "../../../Solutions/09/data.txt")>]
let solve1 (expected, path) =
    path
    |> parseEachLine (withRegex "(.*) to (.*) = (\d+)")
    |> parseRegion
    |> findShortestPath
    |> (fun r -> r.cost)
    |> should equal expected

[<Theory>]
[<InlineDataAttribute(982, "../../../Solutions/09/data_test.txt")>]
[<InlineDataAttribute(0, "../../../Solutions/09/data.txt")>]
let solve2 (expected, path) =
    path
    |> parseEachLine (withRegex "(.*) to (.*) = (\d+)")
    |> parseRegion
    |> findLongestPath
    |> (fun r -> r.cost)
    |> should equal expected