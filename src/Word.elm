module Word exposing (..)
import Test exposing (Test, describe, test)
import Expect

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


type Word
    = Word (List Tile)

fromCharList : List Char -> Word
fromCharList chars =
    Word (List.map (\char -> FilledTile { char = char, match = Unmatched }) chars)


zip a b =
    List.map2 Tuple.pair a b


matchExactTile : (Letter, Letter) -> (Letter, Letter)
matchExactTile ( a, b ) =
    if a.char == b.char then
        ( { a | match = Exact }, { b | match = Exact } )
        -- default format with a blank line -> error when pasting in elm repl
    else
        ( a, b )


matchExact ( sol, guess ) =
    List.unzip (List.map matchExactTile (zip sol guess))


suite : Test
suite =
    describe "Wordle Match Logic"
        [ describe "MatchExact" [
            test "FURBA -> BUFFA" <|
                \_ ->
                    let
                        guess = "FURBA"
                        solution = "BUFFA"
                    in
                        Expect.equal guess solution
        ]
        ]
