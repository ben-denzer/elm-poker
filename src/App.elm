import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import DeckHelpers exposing (makeDeck, shuffleDeck)
import ViewHelpers exposing (..)
import UpdateHelpers exposing
  ( checkForWinners
  , drawCards
  , flipCards
  , makeTimeInt
  , updateHeld
  )
import CustomTypes exposing (..)
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

    payRows : List (Html Msg)
    payRows = List.reverse <| List.map makePayTableRow allHands
  in
    div [ id "gameArea" ]
    [ div [ id "payTable" ]
      [ div [] payRows
      ],
      div [ id "heldRow" ] heldBlocks,
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
