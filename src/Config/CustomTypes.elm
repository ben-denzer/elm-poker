module CustomTypes exposing (..)

type alias Card       = (Int, String)
type alias CardList   = List Card
type CardStatus       = FaceDown | FaceUp
type CardWinnerStatus = NotAWinner | Winner

type GameStatus =
  Begin
  | GameOver
  | Draw

type HandStatus =
  NoWinner
  | JacksOrBetter
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyalFlush

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
  , handStatus              : HandStatus
  , heldCards               : List Int
  , seed                    : Int
  , total                   : Int
  }

type Msg =
  DealOrDraw
  | Hold Int
  | RaiseBet Int
  | Tick Time

type alias PairCheck = { val : Int, times : Int }

type alias Time = Float

type alias WinningHand = { msgName : HandStatus, handName : String, payVal : Int }
