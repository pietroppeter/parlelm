module Main exposing (..)

-- Element, alignRight, centerX, centerY, el, fill, mouseOver, padding, px, rgb255, row, spacing, text, width)

import Browser
import Element exposing (..)
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


bgYellow =
    Background.color (rgb255 255 255 240)


bgCyan =
    Background.color (rgb255 240 255 255)


bgPink =
    Background.color (rgb255 255 240 240)


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ]
        (column [ width (px 500), height fill, centerX, bgCyan ]
            [ viewHeader
            , viewGridArea
            , viewKeyboardArea
            ]
        )


viewHeader =
    row
        [ width fill
        , Border.color (rgb255 255 0 0)
        , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
        ]
        [ viewHeaderBurger
        , viewHeaderTitle
        , viewHeaderButton
        ]


viewHeaderBurger =
    el [ alignLeft, bgPink ] (text "Burger")


viewHeaderTitle =
    el [ centerX, bgYellow ] (text "Title")


viewHeaderButton =
    el [ alignRight, bgPink ] (text "Button")


viewGridArea =
    el [ bgYellow, width fill, height (fillPortion 2) ] viewGrid


viewGrid =
    el [ centerX, centerY ] (text "Grid")


viewKeyboardArea =
    el [ bgPink, width fill, height (fillPortion 1) ] viewKeyboard


viewKeyboard =
    el [ centerX, centerY ] (text "Keyboard")



--- EOF
