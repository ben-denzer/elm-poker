module ViewHelpers exposing (..)
import CustomTypes exposing (..)
import Array
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)

allHands : List WinningHand
allHands =
  [ { msgName = JacksOrBetter,  handName = "Jacks Or Better", payVal = 1 }
  , { msgName = TwoPair,        handName = "Two Pair",        payVal = 2 }
  , { msgName = ThreeOfAKind,   handName = "Three of a Kind", payVal = 3 }
  , { msgName = Straight,       handName = "Straight",        payVal = 4 }
  , { msgName = Flush,          handName = "Flush",           payVal = 6 }
  , { msgName = FullHouse,      handName = "Full House",      payVal = 9 }
  , { msgName = FourOfAKind,    handName = "Four of a Kind",  payVal = 25 }
  , { msgName = StraightFlush,  handName = "Straight Flush",  payVal = 50 }
  , { msgName = RoyalFlush,     handName = "Royal Flush",     payVal = 250 }
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


makePayTableRow : Model -> WinningHand -> Html Msg
makePayTableRow model hand =
  let
    activeBet : Int -> String
    activeBet bet =
      if model.bet == bet then
        " activeCol"
      else
        ""

    activeHand : WinningHand -> String
    activeHand hand =
      if hand.msgName == model.handStatus then
        " activeHand"
      else
        ""

    checkForBonus : WinningHand -> Int
    checkForBonus hand =
      if hand.msgName == RoyalFlush then
        hand.payVal * 16
      else
        hand.payVal * 5

  in
    div [ class <| "payRow" ++ activeHand hand ]
    [ div [ class <| "payRowCol payRowLabel" ]   [ text hand.handName ]
    , div [ class <| "payRowCol" ++ activeBet 1] [ text <| toString hand.payVal ]
    , div [ class <| "payRowCol" ++ activeBet 2] [ text <| toString <| hand.payVal * 2 ]
    , div [ class <| "payRowCol" ++ activeBet 3] [ text <| toString <| hand.payVal * 3 ]
    , div [ class <| "payRowCol" ++ activeBet 4] [ text <| toString <| hand.payVal * 4 ]
    , div [ class <| "payRowCol" ++ activeBet 5] [ text <| toString <| checkForBonus hand ]
    ]
