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

-- Model

type alias Model =
  { cards : Array.Array (Int, String)
  , bet : Int
  , total : Float
  , dealOrDraw : String
  , heldCards : List Int
  }

init : (Model, Cmd Msg)
init =
  (
    { cards = Array.fromList [(1, "H"), (2, "D"), (3, "S"), (4, "S"), (9, "C")]
    , bet = 1
    , total = 100.00
    , dealOrDraw = "Deal"
    , heldCards = [0,1]
    }
    , Cmd.none
  )

-- Update

type Msg =
  Deal
  | Draw
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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Deal -> (model, Cmd.none)
    Draw -> (model, Cmd.none)
    PlayerPays -> (model, Cmd.none)
    PlayerWins -> (model, Cmd.none)
    Hold index -> ( { model | heldCards = updateHeld index model.heldCards }, Cmd.none)

-- View

getCardVal : Array.Array (Int, String) -> Int -> String
getCardVal hand index =
  let thisCard =
    case Array.get index hand of
      Nothing -> ("error", "error")
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
    [ div [ class "cardContainer", id "cardOne" ]
      [ div [ class "holdContainer" ] [ displayIfHeld 0 model.heldCards |> text ]
      , div [ class "card" ] [ text <| getCardVal model.cards 0 ]
      , button [ class "holdButton", onClick <| Hold 0 ] [ text "HOLD" ]
      ],

      div [ class "cardContainer", id "cardOne" ]
      [ div [ class "holdContainer" ] [ displayIfHeld 1 model.heldCards |> text ]
      , div [ class "card" ] [ text <| getCardVal model.cards 1 ]
      ]
    ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
