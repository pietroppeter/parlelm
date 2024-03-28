module Word exposing (MatchedChar(..), rematch, suite)

import Expect
import Test exposing (Test, describe, test)


type MatchedChar
    = Missing Char --grey
    | Exact Char --green
    | Present Char --yellow


toMatched : List Char -> List Char -> List MatchedChar
toMatched matchStatus chars =
    List.map2
        (\m c ->
            case m of
                'e' ->
                    Exact c

                'p' ->
                    Present c

                _ ->
                    Missing c
        )
        matchStatus
        chars



-- Prepend element to second element of a tuple
-- (1, [9, 8, 7]) 10 -> (1, [10, 9, 8, 7])


t2prepend : ( t, List t ) -> t -> ( t, List t )
t2prepend ( a, y ) x =
    ( a, x :: y )



-- finder looks for a (guessed) character inside a list of previously matched chars;
-- returns the state of the match and a new list of matched chars.
-- E.g. 'F' in 'bUffA' -> 'bUFfA' (upper case is a match)


finder : Char -> List MatchedChar -> List Char -> ( MatchedChar, List MatchedChar )
finder g matchState solution =
    case ( matchState, solution ) of
        ( [], _ ) ->
            ( Missing g, [] )

        ( _, [] ) ->
            ( Missing g, [] )

        ( (Missing c) :: ls, l :: ll ) ->
            if g == l then
                -- We found the char matching with a unassigned character
                ( Present g, Present g :: ls )

            else
                t2prepend (finder g ls ll) (Missing c)

        -- If it's Exact or Present, then we skip the letter and try the next one.
        ( m :: ls, l :: ll ) ->
            t2prepend (finder g ls ll) m



-- Given a guess and a solution, returns a list of matched characters
-- Does not handle the case of guess longer than soludion


rematch : List Char -> List Char -> List MatchedChar
rematch guess solution =
    let
        equalChars : List MatchedChar
        equalChars =
            List.map2
                (\g s ->
                    if g == s then
                        Exact g

                    else
                        Missing g
                )
                guess
                solution

        matchGuessChars : Int -> List Char -> List MatchedChar -> List Char -> List MatchedChar
        matchGuessChars i gue state target =
            case ( gue, List.drop i state ) of
                -- It's over when there are no more letters in the guess to match.
                ( [], _ ) ->
                    []

                -- If the state is empty, we cannot match anything
                ( _, [] ) ->
                    []

                ( g :: gs, (Exact _) :: ss ) ->
                    -- If a letter is an exact match, it means we found it already
                    -- so no need to find it. Let's proceed to the next letter.
                    Exact g :: matchGuessChars (i + 1) gs state target

                ( g :: gs, _ :: ss ) ->
                    -- Not sure if this letter is present: let's search for it
                    -- in the whole solution. This might update the state.
                    case finder g state target of
                        ( Exact _, newState ) ->
                            Exact g :: matchGuessChars (i + 1) gs newState target

                        ( Missing _, newState ) ->
                            Missing g :: matchGuessChars (i + 1) gs newState target

                        ( Present _, newState ) ->
                            Present g :: matchGuessChars (i + 1) gs newState target
    in
    matchGuessChars 0 guess equalChars solution


suite : Test
suite =
    let
        testMatch g s e =
            let
                guess =
                    String.toList g

                solution =
                    String.toList s

                expect =
                    String.toList e
            in
            test (g ++ "-" ++ s ++ "=" ++ e) <|
                \_ ->
                    Expect.equal (rematch guess solution) (toMatched expect guess)
    in
    describe "Wordle Match Logic"
        [ describe "MatchExact"
            [ testMatch "FURBA" "BUFFA" "pe.pe"
            , testMatch "BABBA" "CACCA" ".e..e"
            , testMatch "BAA" "CCB" "p.."
            , testMatch "SMAL" "Soooosoos" "e..."
            , testMatch "LLxxx" "yyLLL" "pp..."
            , testMatch "yyLLL" "LLxxx" "..pp."
            , testMatch "AxyA" "zAAw" "p..p"

            -- Pattern longer than solution is not supported by rematch
            -- , testMatch "longer" "smal" "p....."
            -- , testMatch "ABB" "AA" "e.."
            ]
        , describe "finding"
            [ test "B in FuRbBo" <|
                \_ ->
                    Expect.equal
                        (finder 'B' [ Missing 'F', Exact 'U', Missing 'R', Exact 'B', Missing 'B', Missing 'O' ] [ 'F', 'U', 'R', 'B', 'B', 'O' ])
                        ( Present 'B', [ Missing 'F', Exact 'U', Missing 'R', Exact 'B', Present 'B', Missing 'O' ] )
            , test "B in Rbbo" <|
                \_ ->
                    Expect.equal
                        (finder 'B' [ Missing 'R', Exact 'B', Present 'B', Missing 'O' ] [ 'R', 'B', 'B', 'O' ])
                        ( Missing 'B', [ Missing 'R', Exact 'B', Present 'B', Missing 'O' ] )
            , test "finding F(URBA) in (B)UFFA" <|
                \_ ->
                    Expect.equal
                        (finder 'F' [ Exact 'U', Missing 'F', Missing 'F', Exact 'A' ] [ 'U', 'F', 'F', 'A' ])
                        ( Present 'F', [ Exact 'U', Present 'F', Missing 'F', Exact 'A' ] )
            ]
        ]
