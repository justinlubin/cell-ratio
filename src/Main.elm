module Main exposing (..)

import Browser
import Browser.Dom

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Task
import Dict exposing (Dict)

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
  , history : List InputType
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

addToHistory : InputType -> Model -> Model
addToHistory it m =
  { m | history = it :: List.filter ((/=) it) m.history }

liveInput : Model -> Maybe InputType
liveInput m =
  case m.history of
    [] ->
      Nothing

    [_] ->
      Nothing

    [Cells, Media] -> Just Ratio
    [Media, Cells] -> Just Ratio

    [Cells, Ratio] -> Just Media
    [Ratio, Cells] -> Just Media

    [Media, Ratio] -> Just Cells
    [Ratio, Media] -> Just Cells

    [_, _, _] ->
      m.history
        |> List.reverse
        |> List.head

    _ ->
      Nothing

-- Update

type Msg
  = Input InputType String
  | Focus InputType

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Input it s ->
      ( liveUpdate (setInput it s model)
      , Cmd.none
      )

    Focus it ->
      ( addToHistory it model
      , Cmd.none
      )

liveUpdate : Model -> Model
liveUpdate model =
  case liveInput model of
    Nothing ->
      model

    Just Cells ->
      case (String.toFloat model.media, String.toFloat model.ratio) of
        (Just m, Just r) ->
          setInput Cells (String.fromFloat <| m / (r - 1)) model

        _ ->
          setInput Cells "" model

    Just Media ->
      case (String.toFloat model.cells, String.toFloat model.ratio) of
        (Just c, Just r) ->
          setInput Media (String.fromFloat <| c * (r - 1)) model

        _ ->
          setInput Media "" model

    Just Ratio ->
      case (String.toFloat model.cells, String.toFloat model.media) of
        (Just c, Just m) ->
          setInput Ratio (String.fromFloat <| (m + c) / c) model

        _ ->
          setInput Ratio "" model

-- View

viewInput : Model -> InputType -> Html Msg
viewInput model it =
  let
    its =
      inputTypeToString it
  in
  div
    [ classList
        [ ("main-input-row", True)
        , ("live-input", liveInput model == Just it)
        ]
    ]
    [ label
        [ for its ]
        [ text its ]
    , input
        [ id its
        , type_ "number"
        , step "any"
        , onInput (Input it)
        , onFocus (Focus it)
        , value (getInput it model)
        ]
        []
    ]

view : Model -> Html Msg
view model =
  div [] <|
    List.map
      (viewInput model)
      allInputTypes

-- Main

init : {} -> (Model, Cmd Msg)
init _ =
  ( { cells = "", media = "", ratio = "", history = [] }
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
