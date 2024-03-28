module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Ev
import Element.Font as Font
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Word exposing (..)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    { guesses : List (List MatchedChar)
    , current : List Char
    , solution : List Char
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { guesses = []
      , current = []
      , solution = testSolution
      }
    , Cmd.none
    )


type Msg
    = KeyPressed Char
    | Backspace
    | Confirm


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentGuessLen =
            List.length model.current
    in
    ( case msg of
        KeyPressed c ->
            -- Add to the current solution as long as it's shorter than 5,
            -- then just ignore letters
            if currentGuessLen < 5 then
                Debug.log "Pressed" { model | current = model.current ++ [ Char.toUpper c ] }

            else
                model

        Confirm ->
            -- Confirmation can happen only if guess has length 5
            if currentGuessLen < 5 then
                model

            else
                confirmGuess model

        Backspace ->
            -- Remove last character from current, as long as it's not empty
            Debug.log "Chomped" { model | current = List.take (currentGuessLen - 1) model.current }
    , Cmd.none
    )


confirmGuess : Model -> Model
confirmGuess model =
    let
        matched =
            rematch model.current model.solution
    in
    { model | current = [], guesses = model.guesses ++ [ matched ] }



--- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    -- Let's decode the `key` field as a string, then try to decode
    -- the string and see if it is a valid character: if the character is invalid,
    -- the decoding should fail and no message shall be send: only Enter,
    -- Backspace and letters are valid.
    Browser.Events.onKeyDown
        (Decode.andThen decodeKey (Decode.field "key" Decode.string))



-- Take a string and returns a decoder: it will succeed for Enter, Backpace and
-- ASCII alphas, but fail for anything else


decodeKey : String -> Decode.Decoder Msg
decodeKey str =
    case str of
        "Enter" ->
            Decode.succeed Confirm

        "Backspace" ->
            Decode.succeed Backspace

        _ ->
            case String.uncons str of
                Just ( ch, "" ) ->
                    if Char.isAlpha ch then
                        Decode.succeed (KeyPressed ch)

                    else
                        Decode.fail "Not alpha"

                _ ->
                    Decode.fail "Another control"



--- VIEW


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ]
        (column [ width (fill |> maximum 500), height fill, centerX, bgCyan ]
            [ viewHeader
            , viewGridArea model
            , viewKeyboardArea model
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



-- A tile in the game, it can be empty or contains a matched char


type Tile
    = EmptyTile
    | FilledTile MatchedChar
    | UncheckedTile Char


viewGridArea : Model -> Element Msg
viewGridArea model =
    el [ bgYell, width fill, height (fillPortion 2) ] (viewGrid model)


emptyTile =
    EmptyTile


testSolution =
    [ 'B', 'U', 'F', 'F', 'A' ]


emptyWord =
    List.repeat 5 emptyTile



-- Given a list, it will take the first n element from it; if there are less,
-- they are filled with padFill.


padRightTake : Int -> f -> List f -> List f
padRightTake n padFill aList =
    List.take n (aList ++ List.repeat n padFill)



-- Convert a list matched chars into a list of (filled) tiles


tiledGuess : List MatchedChar -> List Tile
tiledGuess x =
    List.map FilledTile x


getWords : Model -> List (List Tile)
getWords model =
    let
        -- Convert previous guesses into tiles
        tiledGuesses =
            List.map tiledGuess model.guesses

        -- Convert current guess into tiles
        tiledCurrent =
            List.map (\c -> UncheckedTile c) model.current

        -- Pad current guess
        paddedCurrent =
            padRightTake 5 EmptyTile tiledCurrent
    in
    padRightTake
        6
        emptyWord
        (tiledGuesses ++ [ paddedCurrent ])


viewGrid : Model -> Element Msg
viewGrid model =
    -- column [ centerX, centerY, spacing 5 ] (List.map viewTileRow (getWords model))
    column [ centerX, centerY, spacing 5 ]
        (List.map viewTileRow (getWords model))


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

        UncheckedTile _ ->
            bgWhite


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

        UncheckedTile _ ->
            colorDarkGray


tileFontColor : MatchedChar -> Color
tileFontColor match =
    case match of
        Missing _ ->
            colorWhite

        Exact _ ->
            colorWhite

        Present _ ->
            colorWhite


tileChar : MatchedChar -> Char
tileChar match =
    case match of
        Missing c ->
            c

        Exact c ->
            c

        Present c ->
            c


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
            el
                [ centerX
                , centerY
                , Font.color (tileFontColor match)
                , Font.size 32
                , Font.bold
                ]
                (text (String.fromChar (tileChar match)))

        UncheckedTile ch ->
            el
                [ centerX
                , centerY

                -- , Font.color (tileFontColor match)
                , Font.size 32
                , Font.bold
                ]
                (text (String.fromChar ch))



-- KEYBOARD


type Keyboard
    = Key Char
    | KeyBackspace
    | KeyEnter


viewKeyboardArea : Model -> Element Msg
viewKeyboardArea model =
    el [ bgPink, width fill, height (fillPortion 1) ] (viewKeyboard model)


viewKeyboard : Model -> Element Msg
viewKeyboard model =
    column [ centerX, centerY, spacing 5 ]
        [ viewKeyboardRow model (List.map Key (String.toList "QWERTYUIOP"))
        , viewKeyboardRow model (List.map Key (String.toList "ASDFGHJKL"))
        , viewKeyboardRow model ((KeyEnter :: List.map Key (String.toList "ZXCVBNM")) ++ [ KeyBackspace ])
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


viewKeyEvent k =
    case k of
        Key c ->
            Ev.onClick (KeyPressed c)

        KeyBackspace ->
            Ev.onClick Backspace

        KeyEnter ->
            Ev.onClick Confirm



-- bestLetterColor : Model -> Char -> Element.Attr d m


bestLetterColor model char =
    let
        improve : MatchedChar -> MatchedChar -> MatchedChar
        improve ch st =
            case ch of
                Missing c ->
                    -- We cannot improve with another missing char, leave state as is
                    st

                Exact c ->
                    if c == char then
                        -- If we find an exact match, that's the best possible
                        Exact char

                    else
                        -- The character is different, leave state as is
                        st

                Present c ->
                    if c == char then
                        case st of
                            Missing _ ->
                                Present char

                            _ ->
                                st

                    else
                        st

        -- Initially, we say that char is missing
        initialState =
            Missing char
    in
    -- bgYell
    List.foldl improve initialState (List.concat model.guesses)



-- Returns the color of a keyboard button depending on matching of past guesses


buttonColor model k =
    -- TODO the color is the "best" match found so far for a letter: if C is
    -- Present in the first guess (yellow) and Exact in the second guess (green)
    -- the C key should be colored green.
    case k of
        Key ch ->
            tileBgColor (FilledTile (bestLetterColor model ch))

        KeyBackspace ->
            bgCyan

        KeyEnter ->
            bgCyan



-- Creates the element for a keyboard button. It will highlight the element
-- depending on the current guesses.


viewMakeButton : Model -> Keyboard -> Element Msg
viewMakeButton model k =
    el
        [ buttonColor model k
        , height (px 58)
        , width (px (viewKeyWidth k))
        , viewKeyEvent k
        ]
        (el [ centerX, centerY ] (text (viewKeyString k)))


viewKeyboardRow : Model -> List Keyboard -> Element Msg
viewKeyboardRow model keys =
    row [ spacing 5, centerX ]
        (List.map (viewMakeButton model) keys)



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
