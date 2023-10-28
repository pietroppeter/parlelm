module Main exposing (..)

import Browser
import Element exposing (Element, alignRight, centerY, el, fill, mouseOver, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    List (List Char)


init : Model
init =
    []


type Msg
    = KeyPressed Char
    | Backspace
    | Confirm


update : Msg -> Model -> Model
update msg model =
    model


view : Model -> Html Msg
view model =
    Element.layout []
        (Element.column []
            [ viewHeader
            , text "Grid"
            , text "Keyboard"
            ]
        )


viewHeader =
    Element.row []
        [ viewHeaderBurger
        , viewHeaderTitle
        , viewHeaderButton
        ]


viewHeaderBurger =
    text "Burger"


viewHeaderTitle =
    text "Title"


viewHeaderButton =
    text "Button"
