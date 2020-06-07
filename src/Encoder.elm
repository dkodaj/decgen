module Encoder exposing (encodeMaybe, encoder)

import Destructuring exposing (bracketCommas, bracketIfSpaced, capitalize, quote, removeColons, tab, tabLines)
import List exposing (filter, indexedMap, length, map, map2, range)
import ParseType exposing (typeNick)
import String exposing (contains, dropRight, join, split)
import Types exposing (Type(..), TypeDef, coreType)


encoder : TypeDef -> List String
encoder typeDef =
    let
        encoderBody =
            map (tab 1) <| split "\n" <| encoderHelp True typeDef.name typeDef.theType

        encoderName =
            case typeDef.theType of
                TypeTuple xs ->
                    "encode" ++ name ++ " (" ++ varListComma xs ++ ") ="

                TypeProduct ( b, c ) ->
                    case c of
                        [] ->
                            "encode" ++ name ++ " a ="

                        _ ->
                            "encode" ++ name ++ " (" ++ b ++ " " ++ varList c ++ ") ="

                _ ->
                    "encode" ++ name ++ " a ="

        name =
            removeColons typeDef.name
            --turn "Vec3.vec3" into "Vec3vec3"

        vars a =
            map var <| range 1 (length a)

        varList a =
            join " " (vars a)

        varListComma a =
            join ", " (vars a)
    in
    encoderName :: encoderBody


encoderHelp : Bool -> String -> Type -> String
encoderHelp topLevel rawName a =
    let
        name =
            removeColons rawName

        maybeAppend txt =
            case topLevel of
                True ->
                    txt ++ " a"

                False ->
                    txt

        recurseOn x y =
            x ++ (bracketIfSpaced <| encoderHelp False "" y)
    in
    case a of
        TypeArray b ->
            maybeAppend <| recurseOn "Encode.array " b

        TypeBool ->
            maybeAppend <| "Encode.bool"

        TypeDict ( b, c ) ->
            case topLevel of
                True ->
                    encoderDict name ( b, c )

                False ->
                    case name of
                        "" ->
                            "encode" ++ typeNick a

                        _ ->
                            "encode" ++ name

        TypeError b ->
            maybeAppend <| b

        TypeExtendedRecord b ->
            --same as TypeRecord
            case topLevel of
                True ->
                    encoderRecord b

                False ->
                    case name of
                        "" ->
                            "encode" ++ typeNick a

                        _ ->
                            "encode" ++ name

        TypeExtensible b ->
            maybeAppend <| "<< Extensible records cannot be encoded >>"

        TypeFloat ->
            maybeAppend <| "Encode.float"

        TypeImported b ->
            maybeAppend <| "encode" ++ removeColons b

        TypeInt ->
            maybeAppend <| "Encode.int"

        TypeList b ->
            maybeAppend <| recurseOn "Encode.list " b

        TypeMaybe b ->
            maybeAppend <| recurseOn "encodeMaybe " b

        TypeProduct b ->
            case topLevel of
                True ->
                    encoderProduct True False b

                False ->
                    case name of
                        "" ->
                            "encode" ++ typeNick a

                        _ ->
                            "encode" ++ name

        TypeRecord b ->
            case topLevel of
                True ->
                    encoderRecord b

                False ->
                    case name of
                        "" ->
                            "encode" ++ typeNick a

                        _ ->
                            "encode" ++ name

        TypeString ->
            maybeAppend <| "Encode.string"

        TypeTuple bs ->
            case topLevel of
                True ->
                    encoderTuple bs

                False ->
                    case name of
                        "" ->
                            "encode" ++ typeNick a

                        _ ->
                            "encode" ++ name

        TypeUnion b ->
            case topLevel of
                True ->
                    encoderUnion name b

                False ->
                    case name of
                        "" ->
                            let
                                folder x y =
                                    x ++ ", " ++ y

                                typeStr =
                                    "{ " ++ (List.foldl folder "" <| map Tuple.first b) ++ " }"
                            in
                            "Encoder parse error: unanymous union type: " ++ typeStr

                        _ ->
                            "encode" ++ name


encoderDict : String -> ( Type, Type ) -> String
encoderDict name ( b, c ) =
    let
        subEncoderName =
            "encode" ++ removeColons name ++ "Tuple"
    in
    join "\n" <|
        [ "let"
        , tab 1 <| subEncoderName ++ " (a1,a2) ="
        , tab 2 "Encode.object"
        , tab 3 <| "[ (\"A1\", " ++ encoderHelp False "" b ++ " a1)"
        , tab 3 <| ", (\"A2\", " ++ encoderHelp False "" c ++ " a2) ]"
        , "in"
        , tab 1 <| "Encode.list " ++ subEncoderName ++ " (Dict.toList a)"
        ]


encodeMaybe : List String
encodeMaybe =
    [ "encodeMaybe f a = "
    , tab 1 "case a of"
    , tab 2 "Just b ->"
    , tab 3 "f b"
    , tab 2 "Nothing ->"
    , tab 3 "Encode.null"
    ]


encoderProduct : Bool -> Bool -> ( String, List Type ) -> String
encoderProduct productType addConstructor ( constructor, subTypes ) =
    let
        fieldDefs =
            map2 (\a b -> ( a, b )) vars subTypes

        fieldEncode ( a, b ) =
            "(" ++ (quote <| capitalize a) ++ ", " ++ subEncoder b ++ " " ++ a ++ ")"

        vars =
            map var <| range 1 (length subTypes)

        subEncoder a =
            let
                fullEncoder =
                    dropRight 2 <| encoderHelp True "" a
            in
            case coreType a of
                True ->
                    fullEncoder

                False ->
                    encoderHelp False "" a

        constrEncode =
            case addConstructor of
                False ->
                    []

                True ->
                    [ "(\"Constructor\", Encode.string " ++ quote constructor ++ ")" ]

        defaultEncoder =
            join "\n" <|
                [ "Encode.object" ]
                    ++ (map (tab 1) <| bracketCommas <| constrEncode ++ map fieldEncode fieldDefs)
                    ++ [ tab 1 "]" ]
    in
    case subTypes of
        [] ->
            case addConstructor of
                True ->
                    defaultEncoder

                False ->
                    "Encode.string " ++ quote constructor

        x :: [] ->
            case productType of
                True ->
                    subEncoder x ++ " a1"

                False ->
                    defaultEncoder

        _ ->
            defaultEncoder


encoderRecord : List TypeDef -> String
encoderRecord xs =
    let
        fieldEncode x =
            "(" ++ quote (removeColons x.name) ++ ", " ++ subEncoder x.theType ++ " a." ++ removeColons x.name ++ ")"

        subEncoder x =
            encoderHelp False "" x
    in
    join "\n" <|
        [ "Encode.object" ]
            ++ (map (tab 1) <| bracketCommas <| map fieldEncode xs)
            ++ [ tab 1 "]" ]


encoderTuple : List Type -> String
encoderTuple xs =
    let
        encodeElement ( idx, elem ) =
            "(\"" ++ varUpper (idx + 1) ++ "\", " ++ encoderHelp False "" elem ++ " " ++ var (idx + 1) ++ ")"
    in
    join "\n" <|
        [ "Encode.object" ]
            ++ (map (tab 1) <| bracketCommas <| map encodeElement <| indexedMap Tuple.pair xs)
            ++ [ tab 1 "]" ]


encoderUnion : String -> List ( String, List Type ) -> String
encoderUnion name xs =
    let
        complexConstructor ( a, b ) =
            b /= []

        simpleUnion =
            filter complexConstructor xs == []
    in
    case simpleUnion of
        True ->
            encoderUnionSimple name xs

        False ->
            encoderUnionComplex name xs


--e.g. type Color = Red | Green | Blue
encoderUnionSimple : String -> List ( String, List Type ) -> String
encoderUnionSimple name xs =
    let
        constructor ( a, b ) =
            (a ++ "-> " ++ quote a)
    in
    join "\n" <|        
        [ tab 1 "case a of" ] ++
        ( map (tabLines 2) <| map constructor xs )



--e.g. type Ammo = Bullets Int | Napalm Float
encoderUnionComplex : String -> List ( String, List Type ) -> String
encoderUnionComplex name xs =
    let
        varList a =
            join " " (vars a)

        vars a =
            map var <| range 1 (length a)

        encodeConstructor ( constructor, fields ) =
            constructor ++ " " ++ varList fields ++ " ->\n" ++ (tabLines 1 <| encoderProduct False True ( constructor, fields ))
    in
    join "\n" <|
        ["case a of"]
        ++ (map (tabLines 1) <| map encodeConstructor xs)

var n =
    "a" ++ String.fromInt n


varUpper n =
    "A" ++ String.fromInt n
