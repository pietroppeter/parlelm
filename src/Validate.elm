module Validate exposing (daysSinceStart, getSecretWord, isParola)

import Array
import Debug
import Parole exposing (inizio, parole, segrete)
import Time
import Time.Extra exposing (Interval(..), diff)


daysSinceStart timestamp =
    diff Day Time.utc inizio timestamp


secrets =
    Array.fromList segrete


getSecretWord : Time.Posix -> String
getSecretWord timestamp =
    let
        days =
            daysSinceStart timestamp

        idx =
            modBy (Array.length secrets) days
    in
    String.toUpper <|
        Maybe.withDefault "OSSAP" <|
            Array.get idx secrets


isParola : String -> Bool
isParola w =
    List.any ((==) (String.toLower w)) parole



-- Secret word
