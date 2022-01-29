module Update exposing
  ( main
  )

import Model exposing (Model, Msg(..))

gcd : Int -> Int -> Int
gcd a b =
  if a == 0 then
    b
  else
    gcd (b |> modBy a) a

update : Msg -> Model -> Model
update msg model =
  case msg of
    CellsInput s ->
      { model | cells = String.toInt s }

    MediaInput s ->
      { model | media = String.toInt s }
