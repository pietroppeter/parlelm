module Word exposing (..)

import Expect
import Test exposing (Test, describe, test)


type Match
    = Missing --grey
    | Exact --green
    | Present --yellow


type alias Letter =
    { char : Char
    , match : Match
    }


type Tile
    = EmptyTile
    | FilledTile Letter


type Word
    = Word (List Tile)


toMatched : List Char -> List Char -> List Letter
toMatched matchStatus chars =
    List.map2
        (\m c ->
            { char = c
            , match =
                case m of
                    'e' ->
                        Exact

                    'p' ->
                        Present

                    _ ->
                        Missing
            }
        )
        matchStatus
        chars


zip a b =
    List.map2 Tuple.pair a b


at : List t -> Int -> Maybe t
at list i =
    List.head (List.drop i (List.take (i + 1) list))


set : List t -> Int -> t -> List t
set list i x =
    if i < List.length list then
        List.take i list ++ [ x ] ++ List.drop (i + 1) list

    else
        list


matchExact : List Char -> List Char -> List Char
matchExact a b =
    List.take (List.length a)
        (List.map2
            (\x y ->
                if x == y then
                    'e'

                else
                    '.'
            )
            a
            b
            ++ List.repeat (List.length a - List.length b) '.'
        )


match : List Char -> List Char -> List Char
match guess solution =
    let
        guessExact : List Char
        guessExact =
            matchExact guess solution

        solutionExact : List Char
        solutionExact =
            matchExact solution guess

        innerLoop i j sol result =
            if j < List.length sol then
                if at sol j /= Just '.' then
                    innerLoop i (j + 1) sol result

                else if at solution j == at guess i then
                    ( set sol j 'p'
                    , set result i 'p'
                    )

                else
                    innerLoop i (j + 1) sol result

            else
                ( sol, result )

        outerLoop i sol result =
            if i < List.length guess then
                if at result i == Just 'e' then
                    outerLoop (i + 1) sol result

                else
                    let
                        ( solInner, resultInner ) =
                            innerLoop i 0 sol result
                    in
                    outerLoop (i + 1) solInner resultInner

            else
                ( sol, result )

        ( solUpdate, resultUpdate ) =
            outerLoop 0 solutionExact guessExact
    in
    resultUpdate


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
            test (g ++ "-" ++ s ++ "=" ++ e) <| \_ -> Expect.equal (match guess solution) expect
    in
    describe "Wordle Match Logic"
        [ describe "MatchExact"
            [ testMatch "FURBA" "BUFFA" "pe.pe"
            , testMatch "BABBA" "CACCA" ".e..e"
            , testMatch "BAA" "CCB" "p.."
            , testMatch "longer" "smal" "p....."
            , testMatch "SMAL" "Soooosoos" "e..."
            , testMatch "LLxxx" "yyLLL" "pp..."
            , testMatch "yyLLL" "LLxxx" "..pp."
            , testMatch "ABB" "AA" "e.."
            , testMatch "AxyA" "zAAw" "p..p"
            ]
        , describe "Lista"
            [ test "3o" <|
                \_ -> Expect.equal (at [ 0, 1, 2, 3, 4 ] 2) (Just 2)
            , test "set 0" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 0 9) [ 9, 1, 2 ]
            , test "set 1" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 1 9) [ 0, 9, 2 ]
            , test "set 2" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 2 9) [ 0, 1, 9 ]
            , test "set 3" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 3 9) [ 0, 1, 2 ]
            ]
        ]
