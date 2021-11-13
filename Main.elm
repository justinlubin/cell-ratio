import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

-- Helpers

gcd : Int -> Int -> Int
gcd a b =
  if a == 0 then
    b
  else
    gcd (b |> modBy a) a

-- Model

type alias Model =
  { cells : Maybe Int
  , media : Maybe Int
  }

-- Update

type Msg
  = CellsInput String
  | MediaInput String

update : Msg -> Model -> Model
update msg model =
  case msg of
    CellsInput s ->
      { model | cells = String.toInt s }

    MediaInput s ->
      { model | media = String.toInt s }

dilutionRatio : Model -> String
dilutionRatio model =
  case (model.cells, model.media) of
    (Just c, Just m) ->
      let num = c in
      let den = m + c in
      let g = gcd num den in
      String.fromInt (num // g) ++ ":" ++ String.fromInt (den // g)

    _ ->
      ""

-- View

row : List (Html Msg) -> Html Msg
row =
  div
    [ style "padding" "30px"
    ]

lab : String -> Html Msg
lab s =
  div
    [ style "margin-bottom" "10px"
    ]
    [ text s
    ]

blab : String -> Html Msg
blab s =
  div
    [ style "margin-bottom" "20px"
    ]
    [ b [] [ text s ]
    ]

bigInput : (String -> Msg) -> Html Msg
bigInput msg =
  input
    [ type_ "number"
    , onInput msg
    , style "font-size" "2em"
    , style "width" "100%"
    , style "display" "block"
    ]
    []

big : String -> Html Msg
big s =
  div
    [ style "font-size" "5em"
    ]
    [ text s
    ]

view : Model -> Html Msg
view model =
  div
    [ style "font-size" "2em"
    , style "font-family" "sans-serif"
    ]
    [ row
        [ lab "Cells:"
        , bigInput CellsInput
        ]
    , row
        [ lab "Media:"
        , bigInput MediaInput
        ]
    , row
        [ blab "Dilution ratio:"
        , big (dilutionRatio model)
        ]
    ]

-- Main

main : Program () Model Msg
main =
  Browser.sandbox
    { init = { cells = Nothing, media = Nothing }
    , update = update
    , view = view
    }

