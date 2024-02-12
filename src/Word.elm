module Word exposing (MatchedChar(..), matchGuess, suite)

import Expect
import Test exposing (Test, describe, test)


type MatchedChar
    = Missing Char --grey
    | Exact Char --green
    | Present Char --yellow


matchGuess : List Char -> List Char -> List MatchedChar
matchGuess guess solution =
    let
        matchStatus =
            match guess solution
    in
    toMatched matchStatus guess


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



-- Prepend element to second element of a tuple
-- (1, [9, 8, 7]) 10 -> (1, [10, 9, 8, 7])


t2prepend : ( t, List t ) -> t -> ( t, List t )
t2prepend ( a, y ) x =
    ( a, x :: y )



-- finder looks for a character inside a list of previously matched chars;
-- returns the state of the match and a new list of matched chars.
-- E.g. 'F' in 'bUffA' -> 'bUFfA' (upper case is a match)


finder : Char -> List MatchedChar -> List Char -> ( MatchedChar, List MatchedChar )
finder soggetto listaStato listaLettereTarget =
    case ( listaStato, listaLettereTarget ) of
        ( [], _ ) ->
            ( Missing soggetto, [] )

        ( _, [] ) ->
            ( Missing soggetto, [] )

        ( (Missing c) :: ls, l :: ll ) ->
            if soggetto == l then
                -- L'abbiamo trovato
                ( Present soggetto, Present soggetto :: ls )

            else
                t2prepend (finder soggetto ls ll) (Missing c)

        -- ( (Exact c) :: ls, l :: ll ) ->
        --     -- Devo saltare questo caso, perché la lettera è già presa
        --     t2prepend (finder soggetto ls ll) (Exact c)
        -- ( (Present c) :: ls, l :: ll ) ->
        --     t2prepend (finder soggetto ls ll) (Present c)
        ( m :: ls, l :: ll ) ->
            t2prepend (finder soggetto ls ll) m


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

        secondStep : Int -> List Char -> List MatchedChar -> List Char -> List MatchedChar
        secondStep i gue stato target =
            case ( gue, List.drop i stato ) of
                ( [], _ ) ->
                    []

                ( _, [] ) ->
                    []

                ( g :: gs, (Exact _) :: ss ) ->
                    -- Se una lettera è sicura significa che l'abbiamo già trovata
                    -- e non ha senso cercare ricorsivamente.
                    Exact g :: secondStep (i + 1) gs stato target

                ( g :: gs, _ :: ss ) ->
                    -- Non sono sicuro, devo cercare
                    case finder g stato target of
                        ( Exact _, nuovoStato ) ->
                            Exact g :: secondStep (i + 1) gs nuovoStato target

                        ( Missing _, nuovoStato ) ->
                            Missing g :: secondStep (i + 1) gs nuovoStato target

                        ( Present _, nuovoStato ) ->
                            Present g :: secondStep (i + 1) gs nuovoStato target
    in
    secondStep 0 guess equalChars solution


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
                    Expect.all
                        [ Expect.equal (match guess solution)
                        , \hint -> Expect.equal (rematch guess solution) (toMatched hint guess)
                        ]
                        expect
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
        , describe "Lista"
            [ test "3o" <|
                \_ -> Expect.equal (at [ 0, 1, 2, 3, 4 ] 2) (Just 2)
            , test "set 0" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 0 9) [ 9, 1, 2 ]
            , test "set 1" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 1 9) [ 0, 9, 2 ]
            , test "set 2" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 2 9) [ 0, 1, 9 ]
            , test "set 3" <| \_ -> Expect.equal (set [ 0, 1, 2 ] 3 9) [ 0, 1, 2 ]
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
