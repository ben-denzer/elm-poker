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
        suitList : List String
        suitList = List.map Tuple.second hand
      in
        case List.head suitList of
          Nothing -> False
          Just val ->
            List.all (\x -> x == val) suitList

    isStraight : CardList -> Bool
    isStraight hand =
      let
        sortedVals : List Int
        sortedVals = List.sort <| List.map Tuple.first hand

        lowest : Int
        lowest =
          case List.head sortedVals of
            Nothing -> -100
            Just val -> val

        highest : Int
        highest =
          case List.head <| List.drop 4 sortedVals of
            Nothing -> -100
            Just val -> val
      in
        if highest - lowest == 4 then
          True
        else if List.length (List.filter (\x -> x > 9) sortedVals) == 4 && List.sum sortedVals == 47 then
          True
        else
          False

    checkForPairs : Int -> List Int -> List PairCheck -> List PairCheck
    checkForPairs index vals newList =
      let
        thisVal : Int
        thisVal =
          case List.head <| List.drop index <| List.take (index + 1) <| vals of
            Nothing -> -1
            Just val -> val

        makeNewList : Int -> List PairCheck
        makeNewList val =
          let
            filtered : List PairCheck
            filtered = List.filter (\x -> x.val == val) newList
          in
            case List.head filtered of
              Nothing ->
                {val = val, times = 1} :: newList
              Just found ->
                {val = val, times = found.times + 1} :: List.filter (\x -> x.val /= val) newList
      in
        if index == 5 then
          List.filter (\x -> x.times > 1) newList
        else
          checkForPairs (index + 1) vals <| makeNewList thisVal

    getVals : CardList -> List Int
    getVals list =
      List.map Tuple.first list

    pairList : List PairCheck
    pairList =
      checkForPairs 0 (getVals hand) []
  in
    if isFlush hand && isStraight hand then
      (StraightFlush, List.repeat 5 Winner)
    else if isFlush hand then
      (Flush, List.repeat 5 Winner)
    else if isStraight hand then
      (Straight, List.repeat 5 Winner)
    else if List.isEmpty pairList == False then
      if List.length pairList == 1 then
        case List.head pairList of
          Nothing -> (NoWinner, List.repeat 5 NotAWinner)
          Just val ->
            case val.times of
              2 ->
                if (val.val > 10) then
                  (JacksOrBetter, List.repeat 5 Winner)
                else
                  (NoWinner, List.repeat 5 NotAWinner)
              3 ->
                (ThreeOfAKind, List.repeat 5 Winner)
              4 ->
                (FourOfAKind, List.repeat 5 Winner)
              default ->
                (NoWinner, List.repeat 5 NotAWinner)
      else
        (NoWinner, List.repeat 5 NotAWinner)
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
