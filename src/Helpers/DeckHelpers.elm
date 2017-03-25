module DeckHelpers exposing (makeDeck, shuffleDeck)
import Random

-- Make my deck, shuffle it
suits : List String
suits = ["C", "D", "H", "S"]

makeCard : String -> Int -> (Int, String)
makeCard string val = (val, string)

makeSuit : String -> Int -> (Int, String)
makeSuit suit = makeCard suit

makeDeck : List Int -> List (Int, String)
makeDeck values =
  let
    clubs     = List.map (makeSuit "C") values
    diamonds  = List.map (makeSuit "D") values
    hearts    = List.map (makeSuit "H") values
    spades    = List.map (makeSuit "S") values
  in
    List.append clubs <| List.append diamonds <| List.append hearts spades

shuffleDeck : List (Int, String) -> Int -> List (Int, String)
shuffleDeck original seed =
  let
    listOfRandoms =
      Random.step
        (Random.list (List.length original) (Random.int 1 100000))
        (Random.initialSeed seed)
        |> Tuple.first
    zipped = List.map2 (,) listOfRandoms original
    sorted = zipped |> List.sortBy Tuple.first
  in
    List.unzip sorted |> Tuple.second
