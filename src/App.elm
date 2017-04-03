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
    , coinVal                 = 0.25
    , currentlyFlippingCards  = False
    , dealOrDraw              = "Deal"
    , gameStatus              = Begin
    , hand                    = []
    , handStatus              = NoWinner
    , heldCards               = []
    , initialSeed             = Time.millisecond * 24747
    , seed                    = floor <| Time.inMilliseconds 98798709.97
    , total                   = 400
    }
    , Cmd.none
  )

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    winnerList : (HandStatus, List CardWinnerStatus)
    winnerList = checkForWinners model.hand

    winnings : Int
    winnings =
      case List.head <| List.filter (\x -> x.msgName == Tuple.first winnerList) allHands of
        Nothing -> 0
        Just val ->
          if val.msgName == RoyalFlush && model.bet == 5 then
            val.payVal * 16
          else
            model.bet * val.payVal
  in
    case msg of
      DealOrDraw last->
        if model.dealOrDraw == "Deal" then
          ( { model |
              cardStatusList = List.repeat 5 FaceDown,
              cardWinnerList = List.repeat 5 NotAWinner,
              hand = Array.toList <| Array.slice 0 5 <| Array.fromList model.cards,
              dealOrDraw = "Draw",
              gameStatus = Draw,
              heldCards = [],
              seed = (floor model.initialSeed) + last,
              total = model.total - model.bet
            }, Cmd.none
          )
        else
          if List.length model.heldCards == 5 then
            ( { model
              | cards = shuffleDeck model.cards <| floor model.initialSeed + last
              , cardWinnerList = Tuple.second winnerList
              , dealOrDraw = "Deal"
              , gameStatus = GameOver
              , handStatus = Tuple.first winnerList
              , total = model.total + winnings
              }, Cmd.none
            )
          else
            ( { model |
                cards = shuffleDeck model.cards <| floor model.initialSeed + last,
                cardStatusList = flipCards model.hand model.heldCards,
                cardWinnerList = List.repeat 5 NotAWinner,
                gameStatus = GameOver,
                hand = Array.toList <| drawCards model.hand model.cards model.heldCards,
                dealOrDraw = "Deal"
              }, Cmd.none
            )
      GenerateSeed last ->
        ( { model | seed = (floor model.initialSeed) + last }, Cmd.none )
      MakeFlush ->
        ( { model
            | cards =
            [ (1, "H")
            , (3, "H")
            , (5, "H")
            , (2, "H")
            , (4, "H")
            , (1, "S")
            , (3, "C")
            , (9, "H")
            , (11, "H")
            , (8, "H")
            ]
          }, Cmd.none
        )
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
            --------- PAYOUT FUNCTION --------
            if model.currentlyFlippingCards == True then
                if Tuple.first winnerList /= NoWinner then
                  if model.gameStatus == GameOver then
                    ( { model
                      | currentlyFlippingCards = False
                      , cardWinnerList = Tuple.second winnerList
                      , handStatus = Tuple.first winnerList
                      , total = model.total + winnings
                      }, Cmd.none
                    )
                  else
                    ( { model
                      | currentlyFlippingCards = False
                      , cardWinnerList = Tuple.second winnerList
                      , handStatus = Tuple.first winnerList
                      }, Cmd.none
                    )

                else
                  ( { model
                    | currentlyFlippingCards = False
                    , cardWinnerList = Tuple.second winnerList
                    , handStatus = Tuple.first winnerList
                    }, Cmd.none
                  )
            else if model.gameStatus == Begin then
              ( { model
                | cards = shuffleDeck (makeDeck <| List.range 1 13) (floor <| Time.inMilliseconds time )
                }, Cmd.none
              )
            else
              ( { model | initialSeed = Time.inMilliseconds time }, Cmd.none )
      PlayerPays -> (model, Cmd.none)
      PlayerWins -> (model, Cmd.none)
      RaiseBet bet ->
        if model.gameStatus == Draw then
          ( model, Cmd.none )
        else
          ( { model | bet = bet }, Cmd.none)
      Hold index ->
        if model.dealOrDraw == "Draw" then
          ( { model | heldCards = updateHeld index model.heldCards }, Cmd.none)
        else
          ( model, Cmd.none )

-- View

view : Model -> Html Msg
view model =
  let
    displayWinner : String
    displayWinner =
      if model.handStatus /= NoWinner then
        case List.head <| List.filter (\x -> x.msgName == model.handStatus) allHands of
          Nothing -> "Error"
          Just val ->  val.handName
      else
        ""

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

    nextBet : Int
    nextBet = getNextBet model.bet

    payRows : List (Html Msg)
    payRows = List.reverse <| List.map (makePayTableRow model) allHands
  in
    div [ id "gameArea" ]
    [ div [ id "payTable", class <| "bet" ++ toString model.bet ]
      [ div [] payRows
      ]
    , div [ id "displayWinningHand" ] [ text displayWinner ]
    , div [ id "heldRow" ] heldBlocks
    , div [ id "cardRow" ] cards
    , div [ id "gameStatusRow" ]
      [ div [ id "currentBet" ]   [ text <| "BET: " ++ toString model.bet ]
      , div [ id "currentTotal" ] [ text <| displayTotal model.total model.coinVal ]
      ]
    , div [ id "gameButtonRow" ]
      [ button [ onClick MakeFlush ]                [ text "Test-Hand = Flush" ]
      , button [ onClick <| RaiseBet nextBet ]      [ text <| "Bet " ++ toString nextBet ]
      , button [ onClick <| RaiseBet 5 ]            [ text <| "Bet Max" ]
      , button [ onClick <| DealOrDraw model.seed ] [ text model.dealOrDraw ]
      , div [] [ text <| "GameStatus" ++ toString model.gameStatus ]
      ]
    ]

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.millisecond * 200) Tick
