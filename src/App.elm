import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import DeckHelpers exposing (makeDeck, shuffleDeck)
import UpdateHelpers exposing (..)
import Array
import Tuple
import Time

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- Model
type alias Card = (Int, String)
type alias CardList = List Card

type alias Model =
  { cards       : CardList
  , bet         : Int
  , cardStatus  : String
  , dealOrDraw  : String
  , hand        : CardList
  , heldCards   : List Int
  , initialSeed : Float
  , seed        : Int
  , total       : Float
  }

makeTimeInt : Float -> Int
makeTimeInt num =
  floor <| Time.inMilliseconds 1.23948

init : (Model, Cmd Msg)
init =
  (
    { cards = shuffleDeck (makeDeck <| List.range 1 13) (makeTimeInt 23498)
    , bet = 1
    , cardStatus = "faceDown"
    , dealOrDraw = "Deal"
    , hand = []
    , heldCards = []
    , initialSeed = Time.millisecond * 24747
    , seed = floor <| Time.inMilliseconds 98798709.97
    , total = 100.00
    }
    , Cmd.none
  )

-- Update

type alias Time = Float

type Msg =
  DealOrDraw Int
  | GenerateSeed Int
  | PlayerPays
  | PlayerWins
  | Hold Int
  | Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DealOrDraw last->
      if model.dealOrDraw == "Deal" then
        ( { model |
            cardStatus = "faceUp",
            hand = Array.toList <| Array.slice 0 5 <| Array.fromList model.cards,
            dealOrDraw = "Draw",
            heldCards = [],
            seed = (floor model.initialSeed) + last
          }, Cmd.none
        )
      else
        ( { model |
            cards = shuffleDeck model.cards <| floor model.initialSeed + last,
            hand = Array.toList <| drawCards model.hand model.cards model.heldCards,
            dealOrDraw = "Deal"
          }, Cmd.none
        )
    GenerateSeed last ->
      ( { model | seed = (floor model.initialSeed) + last }, Cmd.none )
    Tick time ->
      ( { model | initialSeed = Time.inMilliseconds time }, Cmd.none )
    PlayerPays -> (model, Cmd.none)
    PlayerWins -> (model, Cmd.none)
    Hold index ->
      if model.dealOrDraw == "Draw" then
        ( { model | heldCards = updateHeld index model.heldCards }, Cmd.none)
      else
        ( model, Cmd.none )

-- View

getCardVal : CardList -> Int -> Card
getCardVal hand index =
    case Array.get index <| Array.fromList hand of
      Nothing -> (0, "")
      Just val -> val

displayIfHeld : Int -> List Int -> String
displayIfHeld index heldCards =
  case List.member index heldCards of
    True -> "HELD"
    False -> ""


makeCardHtml : Model -> Int -> Html Msg
makeCardHtml model index =
  div [ class "cardContainer", id ("card" ++ toString index), onClick <| Hold index ]
    [ div [ class model.cardStatus ]
      [ div [ class "topLeft" ]  [ text <| toString <| Tuple.first <| getCardVal model.hand index ]
      , div [ class "cardSuit" ] [ text <| Tuple.second <| getCardVal model.hand index ]
      , div [ class "bottomRight" ]  [ text <| toString <| Tuple.first <| getCardVal model.hand index ]
      ]
    ]

makeHeldHtml : Model -> Int -> Html Msg
makeHeldHtml model index =
  div [ class "holdContainer" ] [ displayIfHeld index model.heldCards |> text ]


view : Model -> Html Msg
view model =
  let
    makeEachCard : Int -> Html Msg
    makeEachCard =
      makeCardHtml model

    cards : List (Html Msg)
    cards = List.map makeEachCard <| List.range 0 4

    makeEachHeldBlock : Int -> Html Msg
    makeEachHeldBlock =
      makeHeldHtml model

    heldBlocks : List (Html Msg)
    heldBlocks = List.map makeEachHeldBlock <| List.range 0 4
  in
    div [ id "gameArea" ]
      [ div [ id "heldRow" ] heldBlocks,
        div [ id "cardRow" ] cards,
        div [ id "gameButtonRow" ]
        [ button [ onClick <| DealOrDraw model.seed ] [ text model.dealOrDraw ]
        , div [] [ text <| toString model.cards ]
        , div [] [ text <| toString <| model.seed ]
        ]
      ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.millisecond * 257) Tick
