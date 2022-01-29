module Model exposing
  ( Model
  , Msg(..)
  )

type alias Model =
  { cells : Maybe Int
  , media : Maybe Int
  }

type Msg
  = CellsInput String
  | MediaInput String
