module Word exposing (..)


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


fromCharList chars =
    List.map (\char -> { char = char, match = Unmatched }) chars


guess1 =
    fromCharList [ 'F', 'U', 'R', 'B', 'A' ]


solution =
    fromCharList [ 'B', 'U', 'F', 'F', 'A' ]


zip a b =
    List.map2 Tuple.pair a b


matchExactTile ( a, b ) =
    if a.char == b.char then
        ( { a | match = Exact }, { b | match = Exact } )
        -- default format with a blank line -> error when pasting in elm repl

    else
        ( a, b )


matchExact ( sol, guess ) =
    List.unzip (List.map matchExactTile (zip sol guess))
