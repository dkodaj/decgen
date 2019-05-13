module TypeExtract exposing
    ( extractAll
    , extractAllWithDefs
    , grabRawTypes
    , grabTypeDefs
    , typeNick
    )

import AnonymousTypes exposing (grabAnonymousTypes)
import Destructuring exposing (bracketIfSpaced, civilize, clean, debracket, decomment, derecord, detuple, deunion, dropWord, removeColons, removeStringLiterals, singleLine)
import List exposing (filter, map)
import Regex exposing (Match)
import String exposing (dropRight, join, trim, words)
import Types exposing (Field, RawType, Type(..), TypeDef, isRecord)


aliasDefs : List TypeDef -> List (List String)
aliasDefs types =
    let
        def a =
            [ "type alias " ++ a.name ++ " = " ++ typeDescr True a.theType ]
    in
    map def types


anonymousType : Type -> TypeDef
anonymousType a =
    { name = typeNick a, theType = a }


anonymousTypes : Bool -> List TypeDef -> List TypeDef
anonymousTypes encoding typeList =
    map anonymousType <| grabAnonymousTypes encoding typeList


extractAll : Bool -> String -> List TypeDef
extractAll encoding txt =
    let
        declared =
            grabTypeDefs txt

        anonymous =
            anonymousTypes encoding declared
    in
    declared ++ anonymous


extractAllWithDefs : Bool -> String -> ( List TypeDef, List (List String) )
extractAllWithDefs encoding txt =
    let
        declared =
            grabTypeDefs txt

        anonymous =
            anonymousTypes encoding declared
    in
    ( declared ++ anonymous, aliasDefs <| filter isRecord anonymous )


grabTypeDefs : String -> List TypeDef
grabTypeDefs txt =
    let
        toTypeDef a =
            { name = a.name, theType = typeOf True a.def }
    in
    map toTypeDef <| grabRawTypes txt


grabRawType : List (Maybe String) -> Maybe RawType
grabRawType submatches =
    case submatches of
        (Just a) :: (Just b) :: cs ->
            Just { name = trim a, def = trim <| singleLine b }

        _ ->
            Nothing


grabRawTypes : String -> List RawType
grabRawTypes txt =
    clean <| map grabRawType <| map .submatches <| regexIt <| decomment <| removeStringLiterals txt


regexIt : String -> List Match
regexIt txt =
    case Regex.fromString typeRegex of
        Nothing ->
            []

        Just regex ->
            Regex.find regex txt


typeRegex =
    "type\\s+(?:alias )?\\s*(\\w+)\\s*=([\\w(){},|.:_ \\r\\n]+)(?=(?:\\r\\w|\\n\\w)|$)"



--== Recognize types ==--


typeOf : Bool -> String -> Type
typeOf maybeUnion def =
    --  typeOf True "List String" == TypeList TypeString
    --  typeOf False "List String" == TypeList TypeString
    --  typeOf True "MyType | String" == TypeUnion [TypeOpaque "MyType", TypeString]
    --  typeOf True "MyType" == TypeProduct [TypeOpaque "MyType"]
    --  typeOf False "MyType" == TypeOpaque "MyType"
    let
        subType x =
            typeOf False x
    in
    case detuple def of
        a :: bs ->
            TypeTuple <| map subType (a :: bs)

        [] ->
            case derecord def of
                a :: bs ->
                    let
                        makeField ( x, y ) =
                            Field x (subType y)
                    in
                    TypeRecord <| map makeField (a :: bs)

                [] ->
                    case words (debracket def) of
                        [] ->
                            TypeError "Type conversion error: empty string"

                        a :: bs ->
                            case a of
                                "Array" ->
                                    TypeArray (subType <| dropWord a <| debracket def)

                                "Bool" ->
                                    TypeBool

                                "Dict" ->
                                    case deunion (debracket def) of
                                        ( _, c :: d :: es ) :: fs ->
                                            TypeDict ( subType c, subType d )

                                        _ ->
                                            TypeError "Error parsing def as a Dict"

                                "Dict.Dict" ->
                                    case deunion (debracket def) of
                                        ( _, c :: d :: es ) :: fs ->
                                            TypeDict ( subType c, subType d )

                                        _ ->
                                            TypeError "Error parsing def as a Dict"

                                "Float" ->
                                    TypeFloat

                                "Int" ->
                                    TypeInt

                                "List" ->
                                    TypeList (subType <| dropWord a <| debracket def)

                                "Maybe" ->
                                    TypeMaybe (subType <| dropWord a <| debracket def)

                                "String" ->
                                    TypeString

                                _ ->
                                    case maybeUnion of
                                        True ->
                                            let
                                                constructor ( x, y ) =
                                                    case y of
                                                        [ "" ] ->
                                                            ( x, [] )

                                                        _ ->
                                                            ( x, map subType y )
                                            in
                                            case deunion def of
                                                c :: [] ->
                                                    TypeProduct (constructor c)

                                                c :: ds ->
                                                    TypeUnion <| map constructor (c :: ds)

                                                [] ->
                                                    TypeError "Union type conversion error: empty string"

                                        False ->
                                            TypeOpaque (removeColons a)


typeDescr : Bool -> Type -> String
typeDescr bracketIt a =
    --    typeDescr False (TypeList TypeInt) == "List Int"
    --    typeDescr True (TypeList TypeInt) == "(List Int)"
    let
        wrap x =
            if bracketIt then
                "(" ++ x ++ ")"

            else
                x
    in
    case a of
        TypeArray b ->
            wrap <| "Array " ++ typeDescr True b

        TypeBool ->
            "Bool"

        TypeDict ( b, c ) ->
            "Dict " ++ (bracketIfSpaced <| typeDescr False b) ++ " " ++ (bracketIfSpaced <| typeDescr False c)

        TypeError b ->
            b

        TypeFloat ->
            "Float"

        TypeInt ->
            "Int"

        TypeList b ->
            wrap <| "List " ++ typeDescr True b

        TypeMaybe b ->
            wrap <| "Maybe " ++ typeDescr True b

        TypeOpaque b ->
            b

        TypeProduct ( b, c ) ->
            case c of
                [] ->
                    b

                _ ->
                    b ++ " " ++ (String.concat <| map (typeDescr True) c)

        TypeRecord b ->
            let
                fieldString x =
                    x.name ++ ": " ++ typeDescr False x.fieldType ++ ", "

                fields =
                    dropRight 2 <| String.concat <| map fieldString b
            in
            "{" ++ fields ++ "}"

        TypeString ->
            "String"

        TypeTuple bs ->
            "(" ++ (join ", " <| map (typeDescr False) bs) ++ ")"

        TypeUnion b ->
            let
                constructorString ( x, y ) =
                    case y of
                        [] ->
                            x ++ " | "

                        _ ->
                            x ++ " " ++ (String.concat <| map (typeDescr True) y) ++ " | "

                constructors =
                    dropRight 2 <| String.concat <| map constructorString b
            in
            constructors


typeNick : Type -> String
typeNick a =
    let
        tag prefix =
            prefix ++ (civilize <| typeDescr False a)
    in
    case a of
        TypeRecord _ ->
            tag "Record"

        TypeTuple _ ->
            tag "Tuple"

        _ ->
            tag ""
