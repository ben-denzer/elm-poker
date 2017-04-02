module UpdateHelpers exposing (..)
import CustomTypes exposing (..)
import Array
import Time

checkForWinners : CardList -> (HandStatus, List CardWinnerStatus)
checkForWinners hand =
  let
    isFlush : CardList -> Bool
    isFlush hand =
      let
        suitList = List.map Tuple.second hand
      in
        case List.head suitList of
          Nothing -> False
          Just val ->
            List.all (\x -> x == val) suitList
  in
    if isFlush hand then
      (Flush, List.repeat 5 Winner)
    else
      (NoWinner, List.repeat 5 NotAWinner)

drawCards : CardList -> CardList -> List Int -> Array.Array (Int, String)
drawCards hand cards held =
  let
    next5 = Array.slice 5 10 <| Array.fromList cards

    isHeld : Int -> Bool
    isHeld index =
      List.member index held

    mapHand index thisCard =
      if isHeld index then
        thisCard
      else
        case Array.get index next5 of
          Nothing -> (0, "error")
          Just val -> val

  in
    Array.indexedMap mapHand <| Array.fromList hand

flipCards : CardList -> List (Int) -> List CardStatus
flipCards hand heldCards =
  let
    isHeld : Int -> (Int, String) -> CardStatus
    isHeld index card =
      if List.member index heldCards then
        FaceUp
      else
        FaceDown
  in
    Array.toList <| Array.indexedMap isHeld <| Array.fromList hand

makeTimeInt : Float -> Int
makeTimeInt num =
  floor <| Time.inMilliseconds 1.23948

updateHeld : Int -> List Int -> List Int
updateHeld index heldCards =
  let
    filterOut : Int -> Bool
    filterOut val =
      val /= index
  in
    case List.member index heldCards of
      True -> List.filter filterOut heldCards
      False -> index :: heldCards
