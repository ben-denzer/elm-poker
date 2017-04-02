module CustomTypes exposing (..)

type alias Card       = (Int, String)
type alias CardList   = List Card
type CardStatus       = FaceDown | FaceUp
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
  , coinVal                 : Float
  , currentlyFlippingCards  : Bool
  , dealOrDraw              : String
  , gameStatus              : GameStatus
  , hand                    : CardList
  , heldCards               : List Int
  , initialSeed             : Float
  , seed                    : Int
  , total                   : Int
  }

type Msg =
  DealOrDraw Int
  | GenerateSeed Int
  | Hold Int
  | PlayerPays
  | PlayerWins
  | RaiseBet Int
  | Tick Time

type alias Time = Float
