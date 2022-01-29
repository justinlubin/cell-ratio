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
  | Total

allInputTypes : List InputType
allInputTypes =
  [ Cells, Media, Total ]

inputTypeToString : InputType -> String
inputTypeToString it =
  case it of
    Cells -> "Cells"
    Media -> "Media"
    Total -> "Total"

type alias Model =
  { cells : String
  , media : String
  , total : String
  , free : InputType
  }

getInput : InputType -> Model -> String
getInput it m =
  case it of
    Cells -> m.cells
    Media -> m.media
    Total -> m.total

setInput : InputType -> String -> Model -> Model
setInput it s m =
  case it of
    Cells -> { m | cells = s }
    Media -> { m | media = s }
    Total -> { m | total = s }

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
      case (String.toInt model.total, String.toInt model.media) of
        (Just t, Just m) ->
          let v = t - m in
          setInput
            Cells
            (if v > 0 then String.fromInt v else "")
            model

        _ ->
          setInput Cells "" model

    Media ->
      case (String.toInt model.total, String.toInt model.cells) of
        (Just t, Just c) ->
          let v = t - c in
          setInput
            Media
            (if v > 0 then String.fromInt v else "")
            model

        _ ->
          setInput Media "" model

    Total ->
      case (String.toInt model.cells, String.toInt model.media) of
        (Just c, Just m) ->
          setInput Total (String.fromInt (c + m)) model

        _ ->
          setInput Total "" model

dilutionRatio : Model -> Maybe (Int, Int)
dilutionRatio model =
  case (String.toInt model.cells, String.toInt model.total) of
    (Just c, Just t) ->
      if t <= c then
        Nothing
      else
        let g = gcd c t in
        Just (c // g, t // g)

    _ ->
      Nothing

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
        [ id "dilution-ratio"
        ]
        [ text <|
            case dilutionRatio model of
              Just (left, right) ->
                String.fromInt left ++ ":" ++ String.fromInt right

              Nothing ->
                ""
        ]
    ]

-- Main

init : {} -> (Model, Cmd Msg)
init _ =
  ( { cells = "", media = "", total = "", free = Total }
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
