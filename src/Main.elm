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
    { guesses : List (List Char)
    , solution : List Char
    }


init : Model
init =
    { guesses = []
    , solution = []
    }


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
    column [ centerX, centerY, spacing 5 ]
        [ viewTileRow
        , viewTileRow
        , viewTileRow
        , viewTileRow
        , viewTileRow
        , viewTileRow
        ]


viewTileRow =
    row [ spacing 5 ]
        [ viewTile
        , viewTile
        , viewTile
        , viewTile
        , viewTile
        ]


viewTile =
    el
        [ width (px 62)
        , Border.color colorGray
        , Border.width 2
        , height (px 62)
        , centerX
        , centerY
        , bgPink
        ]
        viewChar


viewChar =
    el [ centerX, centerY ] (text "A")


type Keyboard
    = Key Char
    | KeyBackspace
    | KeyEnter


viewKeyboardArea =
    el [ bgPink, width fill, height (fillPortion 1) ] viewKeyboard


viewKeyboard =
    column [ centerX, alignBottom, spacing 5 ]
        [ viewKeyboardRow (List.map Key (String.toList "QWERTYUIOP"))
        , viewKeyboardRow (List.map Key (String.toList "ASDFGHJKL"))
        , viewKeyboardRow ((KeyEnter :: List.map Key (String.toList "ZXCVBNM")) ++ [ KeyBackspace ])
        ]


viewKeyWidth k =
    case k of
        Key c ->
            43

        _ ->
            65


viewKeyString k =
    case k of
        Key c ->
            String.fromChar c

        KeyEnter ->
            "INVIO"

        KeyBackspace ->
            "<-"


viewMakeButton : Keyboard -> Element msg
viewMakeButton k =
    el [ bgCyan, height (px 58), width (px (viewKeyWidth k)) ]
        (el [ centerX, centerY ] (text (viewKeyString k)))


viewKeyboardRow : List Keyboard -> Element msg
viewKeyboardRow keys =
    row [ spacing 5, centerX ]
        (List.map viewMakeButton keys)


colorGray =
    rgb255 211 214 218



--- EOF
