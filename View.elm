module View exposing
  ( view
  )

import Model exposing (Model, Msg(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

view : Model -> Html Msg
view model =
  div []
    [ div []
        [ input
            [ type_ "radio"
            , name "option"
            ]
            []
        , text "Cells:"
        , input
            [ type_ "number"
            , onInput CellsInput
            ]
            []
        ]
    , div []
        [ input
            [ type_ "radio"
            , name "option"
            ]
            []
        , text "Media:"
        , input
            [ type_ "number"
            , onInput MediaInput
            ]
            []
        ]
    , div []
        [ text <|
            "Dilution ratio:" ++ dilutionRatio model
        ]
    ]
