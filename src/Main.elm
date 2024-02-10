module Main exposing (main, tileFontColor)

import Array exposing (Array)
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Events exposing (onClick)
import Word exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { guesses : List (List MatchedChar) -- diventa quella della logica
    , current : List Char -- diventa List Char
    , solution : List Char -- rimane
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
-- type Match
--     = Noxx --grey
--     | Exactxx --green
--     | Almostxx --yellow
--     | Unmatchedxx --white
-- type alias Letter =
--    { char : Char
--    , match : Match
--    }


type Tile
    = EmptyTile
    | FilledTile MatchedChar



-- type Word
--     = List Letter


viewGridArea : Model -> Element Msg
viewGridArea model =
    el [ bgYell, width fill, height (fillPortion 2) ] (viewGrid model)


emptyTile =
    EmptyTile



-- testWord : Word
-- testWord =
--     [ FilledTile { char = 'A', match = No }
--     , FilledTile { char = 'B', match = Exact }
--     , FilledTile { char = 'C', match = Almost }
--     , FilledTile { char = 'D', match = Unmatched }
--     , EmptyTile
--     ]


testGuess1 =
    [ Missing 'P'
    , Missing 'O'
    , Missing 'S'
    , Missing 'T'
    , Exact 'A'
    ]


testGuess2 =
    [ Present 'F'
    , Exact 'U'
    , Missing 'R'
    , Present 'B'
    , Exact 'A'
    ]


testCurrent =
    [ 'B', 'U', 'F' ]


testSolution =
    [ 'B', 'U', 'F', 'F', 'A' ]


emptyWord =
    List.repeat 5 emptyTile


padRightTake : Int -> f -> List f -> List f
padRightTake n padFill aList =
    List.take n (aList ++ List.repeat n padFill)


guessToTile : List MatchedChar -> List Tile
guessToTile x =
    List.map FilledTile x


getWords : Model -> List (List Tile)
getWords model =
    padRightTake
        6
        emptyWord
        (List.map guessToTile model.guesses ++ [ padRightTake 5 EmptyTile (List.map (\c -> EmptyTile) model.current) ])


viewGrid : Model -> Element Msg
viewGrid model =
    -- column [ centerX, centerY, spacing 5 ] (List.map viewTileRow (getWords model))
    column [ centerX, centerY, spacing 5 ]
        (List.map viewTileRow (getWords model))



-- [ row [ spacing 5 ]
--     (List.map viewTile
--         [ EmptyTile, EmptyTile, EmptyTile ]
--     )
-- ]


viewTileRow : List Tile -> Element Msg
viewTileRow word =
    row [ spacing 5 ]
        (List.map viewTile
            (padRightTake 5 EmptyTile word)
        )


tileBgColor tile =
    case tile of
        EmptyTile ->
            bgWhite

        FilledTile match ->
            case match of
                Missing _ ->
                    bgDarkGray

                Exact _ ->
                    bgGreen

                Present _ ->
                    bgYellow


tileBorderColor tile =
    case tile of
        EmptyTile ->
            colorGray

        FilledTile match ->
            case match of
                Missing _ ->
                    colorDarkGray

                Exact _ ->
                    colorGreen

                Present _ ->
                    colorYellow


tileFontColor : MatchedChar -> Color
tileFontColor match =
    case match of
        Missing _ ->
            colorWhite

        Exact _ ->
            colorWhite

        Present _ ->
            colorWhite


viewTile : Tile -> Element Msg
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

        FilledTile match ->
            let
                ( color, char ) =
                    case match of
                        Missing c ->
                            ( colorWhite, c )

                        Exact c ->
                            ( colorWhite, c )

                        Present c ->
                            ( colorWhite, c )
            in
            el
                [ centerX
                , centerY
                , Font.color color
                , Font.size 32
                , Font.bold
                ]
                (text (String.fromChar char))



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
