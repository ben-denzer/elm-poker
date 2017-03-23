import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array
import Tuple

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- Make my deck, shuffle it
suits : List String
suits = ["C", "D", "H", "S"]

cardValues : List Int
cardValues = List.range 1 13

makeCard : String -> Int -> (Int, String)
makeCard string val = (val, string)

makeSuit : String -> Int -> (Int, String)
makeSuit suit = makeCard suit

makeDeck : List Int -> CardArray
makeDeck values =
  let
    clubs     = Array.map (makeSuit "C") <| Array.fromList values
    diamonds  = Array.map (makeSuit "D") <| Array.fromList values
    hearts    = Array.map (makeSuit "H") <| Array.fromList values
    spades    = Array.map (makeSuit "S") <| Array.fromList values
  in
    Array.append clubs (Array.append diamonds (Array.append hearts spades))
-- makeDeck : List Int -> List String -> CardArray
-- makeDeck values suits =
--

-- Model

type alias CardArray = Array.Array (Int, String)

type alias Model =
  { cards       : CardArray
  , bet         : Int
  , total       : Float
  , dealOrDraw  : String
  , hand        : CardArray
  , heldCards   : List Int
  }

init : (Model, Cmd Msg)
init =
  (
    { cards = makeDeck cardValues
    , bet = 1
    , hand = Array.fromList []
    , total = 100.00
    , dealOrDraw = "Deal"
    , heldCards = []
    }
    , Cmd.none
  )

-- Update

type Msg =
  DealOrDraw
  | PlayerPays
  | PlayerWins
  | Hold Int

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

drawCards : CardArray -> CardArray -> List Int -> CardArray
drawCards hand cards held =
  let
    next5 = Array.slice 5 10 cards

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
    Array.indexedMap mapHand hand

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DealOrDraw ->
      if model.dealOrDraw == "Deal" then
        ( { model | hand = Array.slice 0 5 model.cards, dealOrDraw = "Draw", heldCards = [] }, Cmd.none)
      else
        ( { model | hand = drawCards model.hand model.cards model.heldCards, dealOrDraw = "Deal" }, Cmd.none)
    PlayerPays -> (model, Cmd.none)
    PlayerWins -> (model, Cmd.none)
    Hold index ->
      if model.dealOrDraw == "Draw" then
        ( { model | heldCards = updateHeld index model.heldCards }, Cmd.none)
      else
        ( model, Cmd.none )

-- View

getCardVal : CardArray -> Int -> String
getCardVal hand index =
  let thisCard =
    case Array.get index hand of
      Nothing -> ("", "")
      Just val -> (toString <| Tuple.first val, Tuple.second val)
  in
    Tuple.first thisCard ++ " " ++ Tuple.second thisCard

displayIfHeld : Int -> List Int -> String
displayIfHeld index heldCards =
  case List.member index heldCards of
    True -> "HELD"
    False -> ""


view : Model -> Html Msg
view model =
    div [ id "gameArea" ]
    [ div [ id "cardRow" ]
      [ div [ class "cardContainer", id "cardZero" ]
        [ div [ class "holdContainer" ] [ displayIfHeld 0 model.heldCards |> text ]
        , div [ class "card" ] [ text <| getCardVal model.hand 0 ]
        , button [ class "holdButton", onClick <| Hold 0 ] [ text "HOLD" ]
        ],

        div [ class "cardContainer", id "cardOne" ]
        [ div [ class "holdContainer" ] [ displayIfHeld 1 model.heldCards |> text ]
        , div [ class "card" ] [ text <| getCardVal model.hand 1 ]
        , button [ class "holdButton", onClick <| Hold 1 ] [ text "HOLD" ]
        ],

        div [ class "cardContainer", id "cardTwo" ]
        [ div [ class "holdContainer" ] [ displayIfHeld 2 model.heldCards |> text ]
        , div [ class "card" ] [ text <| getCardVal model.hand 2 ]
        , button [ class "holdButton", onClick <| Hold 2 ] [ text "HOLD" ]
        ],

        div [ class "cardContainer", id "cardThree" ]
        [ div [ class "holdContainer" ] [ displayIfHeld 3 model.heldCards |> text ]
        , div [ class "card" ] [ text <| getCardVal model.hand 3 ]
        , button [ class "holdButton", onClick <| Hold 3 ] [ text "HOLD" ]
        ],

        div [ class "cardContainer", id "cardFour" ]
        [ div [ class "holdContainer" ] [ displayIfHeld 4 model.heldCards |> text ]
        , div [ class "card" ] [ text <| getCardVal model.hand 4 ]
        , button [ class "holdButton", onClick <| Hold 4 ] [ text "HOLD" ]
        ]
      ],
      div [ id "gameButtonRow" ]
      [ button [ onClick DealOrDraw ] [ text model.dealOrDraw ]
      , div [] [ text <| toString model.cards ]
      ]
    ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
