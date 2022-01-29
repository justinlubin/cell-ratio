module Main exposing (..)

import Browser
import Browser.Dom

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task
import Dict exposing (Dict)

-- Helpers

guard : (a -> Bool) -> Maybe a -> Maybe a
guard pred mx =
  case mx of
    Just x ->
      if pred x then Just x else Nothing

    Nothing ->
      Nothing

gcd : Int -> Int -> Int
gcd a b =
  if a == 0 then
    b
  else
    gcd (b |> modBy a) a

-- Model

type InputType
  = Cells
  | Media
  | Ratio

allInputTypes : List InputType
allInputTypes =
  [ Cells, Media, Ratio ]

inputTypeToString : InputType -> String
inputTypeToString it =
  case it of
    Cells -> "Cells"
    Media -> "Media"
    Ratio -> "1:X"

type alias Model =
  { cells : String
  , media : String
  , ratio : String
  , free : InputType
  }

getInput : InputType -> Model -> String
getInput it m =
  case it of
    Cells -> m.cells
    Media -> m.media
    Ratio -> m.ratio

setInput : InputType -> String -> Model -> Model
setInput it s m =
  case it of
    Cells -> { m | cells = s }
    Media -> { m | media = s }
    Ratio -> { m | ratio = s }

-- Update

type Msg
  = Input InputType String
  | FreeSelection InputType
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input it s ->
      ( refresh (setInput it s model)
      , Cmd.none
      )

    FreeSelection it ->
      ( refresh { model | free = it }
      , Cmd.none
      )

    NoOp ->
      (model, Cmd.none)

refresh : Model -> Model
refresh model =
  case model.free of
    Cells ->
      case (String.toFloat model.media, String.toFloat model.ratio) of
        (Just m, Just r) ->
          setInput Cells (String.fromFloat <| m / (r - 1)) model

        _ ->
          setInput Cells "" model

    Media ->
      case (String.toFloat model.cells, String.toFloat model.ratio) of
        (Just c, Just r) ->
          setInput Media (String.fromFloat <| c * (r - 1)) model

        _ ->
          setInput Media "" model

    Ratio ->
      case (String.toFloat model.cells, String.toFloat model.media) of
        (Just c, Just m) ->
          setInput Ratio (String.fromFloat <| (m + c) / c) model

        _ ->
          setInput Ratio "" model

{-
dilutionRatio : Model -> Maybe (Int, Int)
dilutionRatio model =
  case (String.toInt model.cells, String.toInt model.media) of
    (Just c, Just m) ->
      if c > 0 && m > 0 then
        let num = c in
        let den = m + c in
        let g = gcd num den in
        Just (num // g, den // g)
      else
        Nothing

    _ ->
      Nothing
-}

-- View

viewInput : Model -> InputType -> Html Msg
viewInput model it =
  let
    its =
      inputTypeToString it

    selected =
      model.free == it
  in
  div
    [ classList
        [ ("main-input-row", True)
        , ("selected", selected)
        ]
    ]
    [ input
        [ type_ "radio"
        , name "main-input"
        , checked (model.free == it)
        , onInput (\_ -> FreeSelection it)
        , id its
        ]
        []
    , label
        [ for its
        ]
        [ text its
        ]
    , input
        [ type_ "number"
        , onInput (Input it)
        , disabled selected
        , value (getInput it model)
        , id (its ++ "-text")
        ]
        []
    ]

view : Model -> Html Msg
view model =
  div
    [ id "main"
    ] <|
    [ div
        [ class "main-inputs" ]
        (List.map (viewInput model) allInputTypes)
    , div
        [ id "output"
        ]
        [ text <|
            getInput (model.free) model
        ]
    ]

-- Main

init : {} -> (Model, Cmd Msg)
init _ =
  ( { cells = "", media = "", ratio = "", free = Ratio }
  , Cmd.none
  )

main : Program {} Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.none
    }
