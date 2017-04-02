module CustomTypes exposing (..)

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

type alias Time = Float

type Msg =
  DealOrDraw Int
  | GenerateSeed Int
  | PlayerPays
  | PlayerWins
  | Hold Int
  | Tick Time

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
