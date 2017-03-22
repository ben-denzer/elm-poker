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
    , heldCards = []
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Deal -> (model, Cmd.none)
    Draw -> (model, Cmd.none)
    PlayerPays -> (model, Cmd.none)
    PlayerWins -> (model, Cmd.none)
    Hold index -> ( { model | heldCards = index :: model.heldCards }, Cmd.none)

-- View

getCardVal : Array.Array (Int, String) -> Int -> String
getCardVal hand index =
  let thisCard =
    case Array.get index hand of
      Nothing -> ("error", "error")
      Just val -> (toString <| Tuple.first val, Tuple.second val)
  in
    Tuple.first thisCard ++ " " ++ Tuple.second thisCard

checkIfHeld : Int -> List Int -> String
checkIfHeld index heldCards =
  case List.member index heldCards of
    True -> "HELD"
    False -> ""


view : Model -> Html Msg
view model =
    div [ id "gameArea" ]
    [ div [ class "cardContainer", id "cardOne" ]
      [ div [ class "holdContainer" ] [ checkIfHeld 0 model.heldCards |> text ]
      , div [ class "card" ] [ text <| getCardVal model.cards 0 ]
      , button [ class "holdButton", onClick <| Hold 0 ] [ text "HOLD" ]
      ],

      div [ class "cardContainer", id "cardOne" ]
      [ div [ class "holdContainer" ] [ checkIfHeld 1 model.heldCards |> text ]
      , div [ class "card" ] [ text <| getCardVal model.cards 1 ]
      ]
    ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
