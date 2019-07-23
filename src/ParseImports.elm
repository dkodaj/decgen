module ParseImports exposing (Constructors, Expose(..), ExposeList(..), exposedTypes, Import, typeImports)

import Destructuring exposing (bracketed, debracket, decomment, regex, removeNothings, removeStringLiterals)
import List exposing (map)
import Regex exposing (Match)
import String exposing (split, trim)


type Constructors
    = Constructors (List String)
    | DotDot -- unqualified import of a union type, e.g. import MyPackage exposing (MyType(...))


type Expose
    = Simple String
    | Complex String Constructors
    | Operator String


type ExposeList
    = Qualified (List Expose)
    | Unqualified


type alias Import =
    { fullName : String
    , shortName : Maybe String
    , exposes : ExposeList
    }


exposeGrab : List (Maybe String) -> Maybe Expose
exposeGrab submatch =
    case submatch of
        (Just x) :: Nothing :: whatever ->
            case bracketed (trim x) of
                True ->
                    Just <| Operator (debracket <| trim x)

                False ->
                    Just <| (Simple <| trim x)

        (Just x) :: (Just y) :: whatever ->
            let
                words =
                    map trim <| split "," y

                constructors =
                    case words of
                        [ ".." ] ->
                            DotDot

                        _ ->
                            Constructors words
            in
            Just <| Complex (trim x) constructors

        _ ->
            Nothing


exposeList : String -> List Expose
exposeList txt =
    removeNothings <| map exposeGrab <| map .submatches <| regex exposeListRegex txt


exposeListRegex =
    "(.+?)\\s*(?:\\(((?:\\w|\\s|\\.|,)*?)\\)\\s*)?(?:,\\s*|$)"


imports : String -> List Import
imports sourceTxt =
    removeNothings <| map importGrab <| regex importsRegex <| decomment <| removeStringLiterals sourceTxt


importGrab : Match -> Maybe Import
importGrab match =
    case match.submatches of
        (Just "") :: whatever ->
            Nothing

        (Just a) :: b :: Nothing :: cs ->
            Just
                { fullName = a
                , shortName = b
                , exposes = Qualified []
                }

        (Just a) :: b :: (Just "..") :: cs ->
            Just
                { fullName = a
                , shortName = b
                , exposes = Unqualified
                }

        (Just a) :: b :: (Just c) :: ds ->
            Just
                { fullName = a
                , shortName = b
                , exposes = Qualified (exposeList c)
                }

        _ ->
            Nothing


importsRegex =
    --grabs module name, nickname ("as ...") and the things inside "exposing (...)", if any
    "import\\s+((?:\\w|\\.)+)\\s*?(?: as (\\w+)\\s*?)?(?: exposing\\s*\\(((?:.|\\n)+?)\\)\\s*)?(?=(?:\\n$|\\n\\w)|\\s*--|\\s*\\{-)"

isExposedType : Expose -> Bool
isExposedType expose =
    case expose of
        Complex _ _ ->
            True
        
        Simple name ->
            case String.uncons name of
                Nothing ->
                    False
                
                Just (head,rest) ->
                    Char.isUpper head
        
        Operator _ ->
            False
            
exposedTypes : String -> List Expose
exposedTypes txt =
    List.filter isExposedType (exposeList txt)
    
typeImports : String -> List Import
typeImports txt =
    let
        filter imp =
            case imp.exposes of
                Qualified list ->
                    { imp | exposes = Qualified (List.filter isExposedType list) }
                
                Unqualified ->
                    imp
    in
    List.map filter (imports txt)