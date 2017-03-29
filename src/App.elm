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
type CardStatus = FaceDown | FaceUp
type CardWinnerStatus = NotAWinner | Winner
type GameStatus =
  Begin
  | GameOver
  | Draw
  | Win
  | Lose

type alias Model =
  { cards                   : CardList
  , bet                     : Int
  , cardStatusList          : List CardStatus
  , cardWinnerList          : List CardWinnerStatus
  , currentlyFlippingCards  : Bool
  , dealOrDraw              : String
  , gameStatus              : GameStatus
  , hand                    : CardList
  , heldCards               : List Int
  , initialSeed             : Float
  , seed                    : Int
  , total                   : Float
  }

makeTimeInt : Float -> Int
makeTimeInt num =
  floor <| Time.inMilliseconds 1.23948

init : (Model, Cmd Msg)
init =
  (
    { cards                   = shuffleDeck (makeDeck <| List.range 1 13) (makeTimeInt 23498)
    , bet                     = 1
    , cardStatusList          = List.repeat 5 FaceDown
    , cardWinnerList          = List.repeat 5 NotAWinner
    , currentlyFlippingCards  = False
    , dealOrDraw              = "Deal"
    , gameStatus              = Begin
    , hand                    = []
    , heldCards               = []
    , initialSeed             = Time.millisecond * 24747
    , seed                    = floor <| Time.inMilliseconds 98798709.97
    , total                   = 100.00
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

checkForWinners : CardList -> List CardWinnerStatus
checkForWinners hand =
  Debug.log "Hit checkfor"
  List.repeat 5 NotAWinner

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DealOrDraw last->
      if model.dealOrDraw == "Deal" then
        ( { model |
            cardStatusList = List.repeat 5 FaceDown,
            hand = Array.toList <| Array.slice 0 5 <| Array.fromList model.cards,
            dealOrDraw = "Draw",
            gameStatus = Draw,
            heldCards = [],
            seed = (floor model.initialSeed) + last
          }, Cmd.none
        )
      else
        ( { model |
            -- shuffle for next hand
            cards = shuffleDeck model.cards <| floor model.initialSeed + last,
            cardStatusList = flipCards model.hand model.heldCards,
            gameStatus = GameOver,
            hand = Array.toList <| drawCards model.hand model.cards model.heldCards,
            dealOrDraw = "Deal"
          }, Cmd.none
        )
    GenerateSeed last ->
      ( { model | seed = (floor model.initialSeed) + last }, Cmd.none )
    Tick time ->
      let
        cardsDown : Bool
        cardsDown =
          if model.gameStatus /= Begin then
            case List.member FaceDown model.cardStatusList of
              True -> True
              False -> False
          else
            False

        nextToFlip : Int
        nextToFlip =
          case
            List.head
            <| List.filter
              (\x -> Tuple.second x == FaceDown)
              <| List.map2
                (\a b -> (a, b))
                (List.range 0 4)
                model.cardStatusList
          of
            Nothing -> -1
            Just val -> Tuple.first val
      in
        if cardsDown == True then
          let
            newStatusList : List CardStatus
            newStatusList = Array.toList <| Array.set nextToFlip FaceUp <| Array.fromList model.cardStatusList
          in
            ( { model
                | cardStatusList = newStatusList
                , currentlyFlippingCards = True
              }, Cmd.none
            )
        else
          if model.currentlyFlippingCards == True then
            let
              winnerList : List CardWinnerStatus
              winnerList = checkForWinners model.hand
            in
            ( { model | currentlyFlippingCards = False, cardWinnerList = winnerList }, Cmd.none )
          else
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
  let
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
  in
    div [ class "cardContainer", id ("card" ++ toString index), onClick <| Hold index ]
      [ div [ class <| "card " ++ (toString <| Array.get index <| Array.fromList model.cardStatusList) ]
        [ div [ class "topLeft" ]  [ text <| displayVal model.hand index ]
        , div [ class "cardSuit" ] [ text <| Tuple.second <| getCardVal model.hand index ]
        , div [ class "bottomRight" ]  [ text <| displayVal model.hand index ]
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
        , div [] [ text "" ]
        ]
      ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.millisecond * 200) Tick
