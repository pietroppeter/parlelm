module Validate exposing (getSecretWord, isParola)

import Array
import Debug
import Parole exposing (parole, segrete)


secrets =
    Array.fromList segrete


getSecretWord : String
getSecretWord =
    secret


isParola : String -> Bool
isParola w =
    List.any ((==) (String.toLower w)) parole



-- Secret word


secret =
    String.toUpper <|
        Maybe.withDefault "OSSAP" <|
            Array.get 100 secrets
