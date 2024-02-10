module Main exposing (main, tileFontColor)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { guesses : List Word
    , current : Word
    , solution : List Char
    }


init : Model
init =
    { guesses =
        [ testGuess1
        , testGuess2
        ]
    , current = testCurrent
    , solution = testSolution
    }


type Msg
    = KeyPressed Char
    | Backspace
    | Confirm


update : Msg -> Model -> Model
update msg model =
    model



--- VIEW


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ]
        (column [ width (fill |> maximum 500), height fill, centerX, bgCyan ]
            [ viewHeader
            , viewGridArea model
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
    el [ centerX, bgYell ] (text "Title")


viewHeaderButton =
    el [ alignRight, bgPink ] (text "Button")



---  GRID


type Match
    = No --grey
    | Exact --green
    | Almost --yellow
    | Unmatched --white


type alias Letter =
    { char : Char
    , match : Match
    }


type Tile
    = EmptyTile
    | FilledTile Letter


type alias Word =
    List Tile


viewGridArea model =
    el [ bgYell, width fill, height (fillPortion 2) ] (viewGrid model)


emptyTile =
    EmptyTile


testWord : Word
testWord =
    [ FilledTile { char = 'A', match = No }
    , FilledTile { char = 'B', match = Exact }
    , FilledTile { char = 'C', match = Almost }
    , FilledTile { char = 'D', match = Unmatched }
    , EmptyTile
    ]


testGuess1 =
    [ FilledTile { char = 'P', match = No }
    , FilledTile { char = 'O', match = No }
    , FilledTile { char = 'S', match = No }
    , FilledTile { char = 'T', match = No }
    , FilledTile { char = 'A', match = Exact }
    ]


testGuess2 =
    [ FilledTile { char = 'F', match = Almost }
    , FilledTile { char = 'U', match = Exact }
    , FilledTile { char = 'R', match = No }
    , FilledTile { char = 'B', match = Almost }
    , FilledTile { char = 'A', match = Exact }
    ]


testCurrent =
    [ FilledTile { char = 'B', match = Unmatched }
    , FilledTile { char = 'U', match = Unmatched }
    , FilledTile { char = 'F', match = Unmatched }
    ]


testSolution =
    [ 'B', 'U', 'F', 'F', 'A' ]


emptyWord =
    List.repeat 5 emptyTile


padRightTake n padFill aList =
    List.take n (aList ++ List.repeat n padFill)


getWords model =
    padRightTake
        6
        emptyWord
        (model.guesses ++ [ padRightTake 5 EmptyTile model.current ])


viewGrid model =
    column [ centerX, centerY, spacing 5 ]
        (List.map viewTileRow (getWords model))


viewTileRow word =
    row [ spacing 5 ]
        (List.map viewTile
            (padRightTake 5 emptyTile word)
        )


tileBgColor tile =
    case tile of
        EmptyTile ->
            bgWhite

        FilledTile { match } ->
            case match of
                No ->
                    bgDarkGray

                Exact ->
                    bgGreen

                Almost ->
                    bgYellow

                Unmatched ->
                    bgWhite


tileBorderColor tile =
    case tile of
        EmptyTile ->
            colorGray

        FilledTile { match } ->
            case match of
                No ->
                    colorDarkGray

                Exact ->
                    colorGreen

                Almost ->
                    colorYellow

                Unmatched ->
                    colorBlack


tileFontColor : Match -> Color
tileFontColor match =
    case match of
        No ->
            colorWhite

        Exact ->
            colorWhite

        Almost ->
            colorWhite

        Unmatched ->
            colorBlack


viewTile tile =
    el
        [ width (px 62)
        , Border.color (tileBorderColor tile)
        , Border.width 2
        , height (px 62)
        , centerX
        , centerY
        , tileBgColor tile
        ]
        (viewTileChar tile)


viewTileChar : Tile -> Element Msg
viewTileChar tile =
    case tile of
        EmptyTile ->
            el [ centerX, centerY ] (text (String.fromChar ' '))

        FilledTile ftile ->
            el
                [ centerX
                , centerY
                , Font.color (tileFontColor ftile.match)
                , Font.size 32
                , Font.bold
                ]
                (text (String.fromChar ftile.char))



-- KEYBOARD


type Keyboard
    = Key Char
    | KeyBackspace
    | KeyEnter


viewKeyboardArea =
    el [ bgPink, width fill, height (fillPortion 1) ] viewKeyboard


viewKeyboard =
    column [ centerX, centerY, spacing 5 ]
        [ viewKeyboardRow (List.map Key (String.toList "QWERTYUIOP"))
        , viewKeyboardRow (List.map Key (String.toList "ASDFGHJKL"))
        , viewKeyboardRow ((KeyEnter :: List.map Key (String.toList "ZXCVBNM")) ++ [ KeyBackspace ])
        ]


viewKeyWidth k =
    case k of
        Key _ ->
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



--- COLORS


colorGray =
    rgb255 211 214 218


colorDarkGray =
    rgb255 134 136 138


colorWhite =
    rgb255 255 255 255


colorBlack =
    rgb255 33 33 33


colorGreen =
    rgb255 106 170 100


colorYellow =
    rgb255 181 159 59


bgGray =
    Background.color colorGray


bgDarkGray =
    Background.color colorDarkGray


bgWhite =
    Background.color colorWhite


bgGreen =
    Background.color colorGreen


bgYellow =
    Background.color colorYellow



-- colors used to debug UI


bgYell =
    Background.color (rgb255 255 255 240)


bgCyan =
    Background.color (rgb255 240 255 255)


bgPink =
    Background.color (rgb255 255 240 240)



--- EOF
