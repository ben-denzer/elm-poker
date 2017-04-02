module ViewHelpers exposing (..)
import CustomTypes exposing (..)
import Array
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)

allHands : List (String, Int)
allHands =
  [ ("Jacks Or Better", 1)
  , ("Two Pair", 2)
  , ("Three Of A Kind", 3)
  , ("Straight", 4)
  , ("Flush", 6)
  , ("Full House", 9)
  , ("Four Of A Kind", 25)
  , ("Straight Flush", 50)
  , ("Royal Flush", 250)
  ]

displayIfHeld : Int -> List Int -> String
displayIfHeld index heldCards =
  case List.member index heldCards of
    True -> "HELD"
    False -> ""

displayImg : CardList -> Int -> String
displayImg hand index =
  let
    suit = case Tuple.second <| getCardVal hand index of
      "C" -> "clubs"
      "D" -> "diamonds"
      "H" -> "hearts"
      "S" -> "spades"
      default -> "cardback" -- error case
  in
    "https://bdenzer.com/projects/videopoker/images/" ++ suit ++ ".png"

displayStatus : List CardStatus -> List Int -> Int -> String
displayStatus cardStatusList heldList index =
  let
    cardFaceStatus : List CardStatus -> Int -> String
    cardFaceStatus statusList index =
      case Array.get index <| Array.fromList statusList of
        Nothing -> ""
        Just val ->  toString val

    cardHeldStatus : List Int -> Int -> String
    cardHeldStatus heldCards index =
      case List.member index heldCards of
        False -> ""
        True -> "held"
  in
    "card " ++ (cardFaceStatus cardStatusList index) ++ " " ++ (cardHeldStatus heldList index)

displayTotal : Int -> Float -> String
displayTotal total coinVal =
  let
    subTotal = toFloat total * coinVal
    testCase = toFloat <| truncate subTotal
  in
    if subTotal == testCase then
      "$" ++ (toString subTotal) ++ ".00"
    else if subTotal - testCase == 0.5 then
      "$" ++ (toString subTotal) ++ "0"
    else
      "$" ++ toString subTotal

displayVal : CardList -> Int -> String
displayVal hand index =
  let
    cardVal = Tuple.first <| getCardVal hand index
  in
  case cardVal of
    13 -> "K"
    12 -> "Q"
    11 -> "J"
    1  -> "A"
    default -> toString cardVal

getCardVal : CardList -> Int -> Card
getCardVal hand index =
    case Array.get index <| Array.fromList hand of
      Nothing -> (0, "")
      Just val -> val

getNextBet : Int -> Int
getNextBet bet =
  if bet == 5 then
    1
  else
    bet + 1

makeCardHtml : Model -> Int -> Html Msg
makeCardHtml model index =
    let
      isWinner : String
      isWinner =
        case Array.get index <| Array.fromList model.cardWinnerList of
          Nothing ->
            ""
          Just val ->
            if val == Winner then
              " winner"
            else
              ""
    in
      div [ class "cardContainer", id ("card" ++ toString index), onClick <| Hold index ]
        [ div [ class <| displayStatus model.cardStatusList model.heldCards index ++ isWinner ]
          [ div [ class "topLeft" ]  [ text <| displayVal model.hand index ]
          , img [ class "cardSuit", src <| displayImg model.hand index ] []
          , div [ class "bottomRight" ]  [ text <| displayVal model.hand index ]
          ]
        ]

makeHeldHtml : Model -> Int -> Html Msg
makeHeldHtml model index =
  div [ class "holdContainer" ] [ displayIfHeld index model.heldCards |> text ]


makePayTableRow : (String, Int) -> Html Msg
makePayTableRow hand =
  let
    handName : (String, Int) -> String
    handName hand = Tuple.first hand

    handVal : (String, Int) -> Int
    handVal hand = Tuple.second hand

    rowClassName : (String, Int) -> String
    rowClassName hand =
      String.join "_" <| String.split " " <| Tuple.first hand

    checkForBonus : (String, Int) -> Int
    checkForBonus hand =
      if Tuple.first hand == "Royal Flush" then
        Tuple.second hand * 16
      else
        Tuple.second hand * 5
  in
    div [ class <| "payRow " ++ rowClassName hand ]
    [ div [ class "payRowCol payRowLabel" ]   [ text <| handName hand ]
    , div [ class "payRowCol payOne" ]        [ text <| toString <| handVal hand ]
    , div [ class "payRowCol payTwo" ]        [ text <| toString <| handVal hand * 2 ]
    , div [ class "payRowCol payThree" ]      [ text <| toString <| handVal hand * 3 ]
    , div [ class "payRowCol payFour" ]       [ text <| toString <| handVal hand * 4 ]
    , div [ class "payRowCol payFive" ]       [ text <| toString <| checkForBonus hand ]
    ]
