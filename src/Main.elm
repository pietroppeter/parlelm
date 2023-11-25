module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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
    { guesses =
        [ [ 'A', 'B', 'C', 'D', 'E' ]
        , [ 'A', 'B', 'C' ]
        ]
    , solution = []
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
        (column [ width (px 500), height fill, centerX, bgCyan ]
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
    | Unmatched


type alias MatchedTile =
    { char : Char
    , match : Match
    }


type Tile
    = EmptyTile
    | FilledTile MatchedTile


viewGridArea model =
    el [ bgYell, width fill, height (fillPortion 2) ] (viewGrid model)


emptyTile =
    EmptyTile


defaultWord : List Tile
defaultWord =
    [ FilledTile { char = 'A', match = No }
    , FilledTile { char = 'B', match = Exact }
    , FilledTile { char = 'C', match = Almost }
    , FilledTile { char = 'D', match = Unmatched }
    , EmptyTile
    ]


viewGrid model =
    column [ centerX, centerY, spacing 5 ]
        (List.map viewTileRow
            (List.repeat 6 defaultWord)
         {--
            (List.take 6
                (model.guesses ++ List.repeat 6 [])
            )
-}
        )


viewTileRow word =
    row [ spacing 5 ]
        (List.map viewTile
            (List.take 5
                (word ++ List.repeat 5 emptyTile)
            )
        )


tileBgColor tile =
    case tile of
        EmptyTile ->
            bgWhite

        FilledTile { char, match } ->
            case match of
                No ->
                    bgGray

                Exact ->
                    bgGreen

                Almost ->
                    bgYellow

                Unmatched ->
                    bgWhite


viewTile tile =
    el
        [ width (px 62)
        , Border.color colorGray
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
            el [ centerX, centerY ] (text (String.fromChar ftile.char))



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


colorWhite =
    rgb255 255 255 255


colorGreen =
    rgb255 106 170 100


colorYellow =
    rgb255 181 159 59


bgGray =
    Background.color colorGray


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
