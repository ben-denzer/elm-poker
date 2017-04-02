module ViewHelpers exposing (..)
import CustomTypes exposing (..)
import Array
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, id, src)
import Html.Events exposing (onClick)

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

makeCardHtml : Model -> Int -> Html Msg
makeCardHtml model index =
    div [ class "cardContainer", id ("card" ++ toString index), onClick <| Hold index ]
      [ div [ class <| displayStatus model.cardStatusList model.heldCards index ]
        [ div [ class "topLeft" ]  [ text <| displayVal model.hand index ]
        , img [ class "cardSuit", src <| displayImg model.hand index ] []
        , div [ class "bottomRight" ]  [ text <| displayVal model.hand index ]
        ]
      ]

makeHeldHtml : Model -> Int -> Html Msg
makeHeldHtml model index =
  div [ class "holdContainer" ] [ displayIfHeld index model.heldCards |> text ]

makePayTableRow : String -> Int -> Html Msg
makePayTableRow str int =
  div [ class "payRow" ]
  [ div [ class "payRowCol payRowLabel" ]   [ text str ]
  , div [ class "payRowCol payOne" ]        [ text <| toString int ]
  , div [ class "payRowCol payTwo" ]        [ text <| toString <| int * 2 ]
  , div [ class "payRowCol payThree" ]      [ text <| toString <| int * 3 ]
  , div [ class "payRowCol payFour" ]       [ text <| toString <| int * 4 ]
  , div [ class "payRowCol payFive" ]       [ text <| toString <| int * 5 ]
  ]
