import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array
import Tuple
import Random
import Time

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
    clubs     = List.map (makeSuit "C") values
    diamonds  = List.map (makeSuit "D") values
    hearts    = List.map (makeSuit "H") values
    spades    = List.map (makeSuit "S") values
  in
    List.append clubs <| List.append diamonds <| List.append hearts spades

shuffleDeck : CardArray -> Int -> CardArray
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

-- Model
type alias Card = (Int, String)
type alias CardArray = List Card

type alias Model =
  { cards       : CardArray
  , bet         : Int
  , cardStatus  : String
  , dealOrDraw  : String
  , hand        : CardArray
  , heldCards   : List Int
  , initialSeed : Float
  , seed        : Int
  , total       : Float
  }

init : (Model, Cmd Msg)
init =
  (
    { cards = shuffleDeck (makeDeck cardValues) 1234
    , bet = 1
    , cardStatus = "faceDown"
    , dealOrDraw = "Deal"
    , hand = []
    , heldCards = []
    , initialSeed = 1.234
    , seed = 1234
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

drawCards : CardArray -> CardArray -> List Int -> Array.Array (Int, String)
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

getCardVal : CardArray -> Int -> Card
getCardVal hand index =
    case Array.get index <| Array.fromList hand of
      Nothing -> (0, "")
      Just val -> val

displayIfHeld : Int -> List Int -> String
displayIfHeld index heldCards =
  case List.member index heldCards of
    True -> "HELD"
    False -> ""


view : Model -> Html Msg
view model =
    div [ id "gameArea" ]
      [ div [ id "heldRow" ]
        [ div [ class "holdContainer" ] [ displayIfHeld 0 model.heldCards |> text ]
        , div [ class "holdContainer" ] [ displayIfHeld 1 model.heldCards |> text ]
        ],
        div [ id "cardRow" ]
          [ div [ class "cardContainer", id "cardZero", onClick <| Hold 0 ]
            [ div [ class model.cardStatus ]
              [ div [ class "topLeft" ]  [ text <| toString <| Tuple.first <| getCardVal model.hand 0 ]
              , div [ class "cardSuit" ] [ text <| Tuple.second <| getCardVal model.hand 0 ]
              , div [ class "bottomRight" ]  [ text <| toString <| Tuple.first <| getCardVal model.hand 0 ]
              ]
            ],
            div [ class "cardContainer", id "cardOne", onClick <| Hold 1]
            [ div [ class model.cardStatus ]
              [ div [ class "topLeft" ]  [ text <| toString <| Tuple.first <| getCardVal model.hand 1 ]
              , div [ class "cardSuit" ] [ text <| Tuple.second <| getCardVal model.hand 1 ]
              , div [ class "bottomRight" ]  [ text <| toString <| Tuple.first <| getCardVal model.hand 1 ]
              ]
            ]
        ],
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
