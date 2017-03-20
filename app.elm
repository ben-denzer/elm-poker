import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array

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
  { cards : Array.Array Int
  , bet : Int
  , total : Float
  , dealOrDraw : String
  }

init : (Model, Cmd Msg)
init =
  (
    { cards = Array.fromList [1,2,3,4,5]
    , bet = 1
    , total = 100.00
    , dealOrDraw = "Deal"
    }
    , Cmd.none
  )

-- Update

type Msg =
  Deal | Draw | PlayerPays | PlayerWins

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Deal -> (model, Cmd.none)
    Draw -> (model, Cmd.none)
    PlayerPays -> (model, Cmd.none)
    PlayerWins -> (model, Cmd.none)

-- View

view : Model -> Html Msg
view model =
  let card1 =
    case Array.get 0 model.cards of
      Nothing -> "Not Found"
      Just val -> toString val
  in
    div [] [
      div [ class "card" ] [ text card1 ]
    ]


-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
