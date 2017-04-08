module UpdateHelpers exposing (..)
import CustomTypes exposing (..)
import Array

checkForWinners : CardList -> (HandStatus, List CardWinnerStatus)
checkForWinners hand =
  let
    getVals : CardList -> List Int
    getVals list =
      List.map Tuple.first list

    pairList : List PairCheck
    pairList =
      checkForPairs 0 (getVals hand) []
  in
    if isFlush hand && isStraight hand pairList then
      if (List.member 1 <| getVals hand) && (List.member 13 <| getVals hand) then
        (RoyalFlush, List.repeat 5 Winner)
      else
        (StraightFlush, List.repeat 5 Winner)
    else if findPairHands pairList == FourOfAKind then
      (FourOfAKind, highlightWinners hand pairList)
    else if findPairHands pairList == FullHouse then
      (FullHouse, highlightWinners hand pairList)
    else if isFlush hand then
      (Flush, List.repeat 5 Winner)
    else if isStraight hand pairList then
      (Straight, List.repeat 5 Winner)
    else
      case findPairHands pairList of
        ThreeOfAKind ->
          (ThreeOfAKind, highlightWinners hand pairList)
        TwoPair ->
          (TwoPair, highlightWinners hand pairList)
        JacksOrBetter ->
          (JacksOrBetter, highlightWinners hand pairList)
        default ->
          (NoWinner, List.repeat 5 NotAWinner)

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

findPairHands : List PairCheck -> HandStatus
findPairHands pairList =
  if List.length pairList == 0 then
    NoWinner
  else if List.length pairList == 1 then
    case List.head pairList of
      Nothing -> NoWinner
      Just val ->
        case val.times of
          2 ->
            if val.val > 10 || val.val == 1 then
              JacksOrBetter
            else
              NoWinner
          3 ->
            ThreeOfAKind
          4 ->
            FourOfAKind
          default ->
            NoWinner
-- Pair / Full House
  else
    let
      checkForThree : Int
      checkForThree =
      case List.maximum <| List.map .times pairList of
        Nothing -> -1
        Just val -> val
    in
      if checkForThree == 3 then
        FullHouse
      else
        TwoPair


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

highlightWinners : CardList -> List PairCheck -> List CardWinnerStatus
highlightWinners hand pairList =
  let
    vals : List PairCheck -> List Int
    vals pairList =
      List.map .val pairList

    checkVal : Card -> CardWinnerStatus
    checkVal card =
      if List.member (Tuple.first card)  (vals pairList) == True then
        Winner
      else
        NotAWinner
  in
    if List.length pairList == 0 then
      List.repeat 5 NotAWinner
    else
      List.map checkVal hand

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

isStraight : CardList -> List PairCheck-> Bool
isStraight hand pairList =
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
    if List.length pairList /= 0 then
      False
    else if highest - lowest == 4 then
      True
    else if List.length (List.filter (\x -> x > 9) sortedVals) == 4 && List.sum sortedVals == 47 then
      True
    else
      False

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
