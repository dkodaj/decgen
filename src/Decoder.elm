module Decoder exposing (decoder)

import Destructuring exposing (bracket, bracketIfSpaced, capitalize, quote, removeColons, tab, tabLines)
import List exposing (concat, filter, indexedMap, length, map, map2, range)
import ParseType exposing (typeNick)
import String exposing (join, split)
import Types exposing (ExtraPackage(..), Type(..), TypeDef, coreType)


decoder : ExtraPackage -> TypeDef -> List String
decoder extra typeDef =
    let
        decoderBody =
            case typeDef.theType of
                TypeUnion _ ->
                    decoderBodyRaw

                _ ->
                    map (tab 1) decoderBodyRaw

        decoderBodyRaw =
            split "\n" <| decoderHelp True typeDef.name typeDef.theType extra

        decoderName =
            "decode" ++ removeColons typeDef.name ++ " ="
    in
    decoderName :: decoderBody


decoderHelp : Bool -> String -> Type -> ExtraPackage -> String
decoderHelp topLevel rawName a extra =
    let
        recurseOn x y =
            x ++ " " ++ (bracketIfSpaced <| decoderHelp False "" y extra)
            
        name =
            removeColons rawName
    in
    case a of
        TypeArray b ->
            recurseOn "Decode.array" b

        TypeBool ->
            "Decode.bool"

        TypeDict ( b, c ) ->
            case topLevel of
                True ->
                    let
                        subDecoderName =
                            "decode" ++ name ++ "Tuple"

                        subDecoder =
                            decoderHelp True "" (TypeTuple [ b, c ]) extra
                    in
                    join "\n"
                        [ "let"
                        , tab 1 <| subDecoderName ++ " ="
                        , tabLines 2 <| subDecoder
                        , "in"
                        , tab 1 <| "Decode.map Dict.fromList (Decode.list " ++ subDecoderName ++ ")"
                        ]

                False ->
                    case name of
                        "" ->
                            "decode" ++ typeNick a

                        _ ->
                            "decode" ++ name

        TypeError b ->
            b

        TypeExtendedRecord b ->
            case topLevel of
                True ->
                    case name of
                        "" ->
                            decoderRecord (typeNick a) b extra

                        _ ->
                            decoderRecord (name ++ "Extended") b extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ typeNick a

                        _ ->
                            "decode" ++ name ++ "Extended"

        TypeExtensible _ ->
            "<< Extensible records are not decoded. >>"

        TypeFloat ->
            "Decode.float"

        TypeImported b ->
            "decode" ++ removeColons b

        TypeInt ->
            "Decode.int"

        TypeList b ->
            recurseOn "Decode.list" b

        TypeMaybe b ->
            recurseOn "Decode.maybe" b

        TypeProduct b ->
            case topLevel of
                True ->
                    decoderProduct True b extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ typeNick a

                        _ ->
                            "decode" ++ name

        TypeRecord b ->
            case topLevel of
                True ->
                    case name of
                        "" ->
                            decoderRecord (typeNick a) b extra

                        _ ->
                            decoderRecord name b extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ typeNick a

                        _ ->
                            "decode" ++ name

        TypeString ->
            "Decode.string"

        TypeTuple bs ->
            case topLevel of
                True ->
                    decoderTuple bs extra

                False ->
                    case name of
                        "" ->
                            "decode" ++ typeNick a

                        _ ->
                            "decode" ++ name

        TypeUnion b ->
            case topLevel of
                True ->
                    decoderUnion name b extra

                False ->
                    case name of
                        "" ->
                            let
                                folder x y =
                                    x ++ ", " ++ y

                                typeStr =
                                    "{ " ++ (List.foldl folder "" <| map Tuple.first b) ++ " }"
                            in
                            "Decoder parse error: anonymous union type: " ++ typeStr

                        _ ->
                            "decode" ++ name


decoderProduct : Bool -> ( String, List Type ) -> ExtraPackage -> String
decoderProduct productType ( constructor, subTypes ) extra =
    let
        fieldDecode ( a, b ) =
            field varNum (quote <| capitalize a) (subDecoder b) extra

        fieldDefs =
            map2 (\a b -> ( a, b )) vars subTypes

        subDecoder a =
            bracketIfSpaced <| decoderHelp False "" a extra

        vars =
            map var <| range 1 varNum

        varNum =
            length subTypes

        simpleDecoder x =
            "Decode.map " ++ constructor ++ " " ++ subDecoder x

        complexDecoder =
            join "\n" <|
                [ mapper varNum
                , tab 1 constructor
                ]
                    ++ (map (tab 2) <| map fieldDecode fieldDefs)
    in
    case subTypes of
        [] ->
            "Decode.succeed " ++ constructor

        x :: [] ->
            if productType then
                simpleDecoder x

            else
                complexDecoder

        _ ->
            complexDecoder


decoderRecord : String -> List TypeDef -> ExtraPackage -> String
decoderRecord name xs extra =
    let
        fieldDecode x =
            field fieldNum (quote x.name) (subDecoder x.theType) extra

        subDecoder x =
            bracketIfSpaced <| decoderHelp False "" x extra

        fieldNum =
            length xs
    in
    join "\n" <|
        [ mapper fieldNum
        , tab 1 name
        ]
            ++ (map (tab 2) <| map fieldDecode xs)


decoderTuple : List Type -> ExtraPackage -> String
decoderTuple xs extra =
    let
        component ( idx, elem ) =
            tab 2 <| field varNum (quote <| varUpper <| idx + 1) (subDecoder elem) extra

        toTuple =
            "(\\" ++ varList ++ " -> " ++ varListComma ++ ")"

        subDecoder x =
            bracketIfSpaced <| decoderHelp False "" x extra

        vars =
            map var <| range 1 varNum

        varList =
            join " " vars

        varListComma =
            "(" ++ join ", " vars ++ ")"

        varNum =
            length xs
    in
    mapper varNum
        ++ "\n"
        ++ tab 1 toTuple
        ++ "\n"
        ++ (join "\n" <| map component <| indexedMap Tuple.pair xs)


decoderUnion : String -> List ( String, List Type ) -> ExtraPackage -> String
decoderUnion name xs extra =
    let
        complexConstructor ( a, b ) =
            b /= []

        simpleUnion =
            filter complexConstructor xs == []
    in
    case simpleUnion of
        True ->
            decoderUnionSimple name xs

        False ->
            decoderUnionComplex name xs extra



--e.g. type Color = Red | Green | Blue
decoderUnionSimple : String -> List ( String, List Type ) -> String
decoderUnionSimple name xs =
    let
        constructor ( a, b ) =
            (quote a ++ "->\n") ++ (tab 2 <| "Decode.succeed " ++ a)
    in
    join "\n" <|
        map (tab 1) <|
            [ "let", tab 1 "recover x =", tab 2 "case x of" ]
                ++ (map (tabLines 3) <| map constructor xs)
                ++ [ tab 3 "other->", tab 4 "Decode.fail <| \"Unknown constructor for type " ++ name ++ ": \" ++ other" ]
                ++ [ "in", tab 1 "Decode.string |> Decode.andThen recover" ]



--e.g. type Ammo = Bullets Int | Napalm Float
decoderUnionComplex : String -> List ( String, List Type ) -> ExtraPackage -> String
decoderUnionComplex name xs extra =
    let
        decodeConstructor ( constructor, fields ) =
            quote constructor ++ " ->\n" ++ (tabLines 1 <| decoderProduct False ( constructor, fields ) extra)
    in
    join "\n" <|
        [ tab 1 <| "Decode.field \"Constructor\" Decode.string |> Decode.andThen decode" ++ removeColons name ++ "Help" ++ "\n"
        , "decode" ++ removeColons name ++ "Help constructor ="
        , tab 1 "case constructor of"
        ]
            ++ (map (tabLines 2) <| map decodeConstructor xs)
            ++ [ tab 2 "other->"
               , tab 3 <| "Decode.fail <| \"Unknown constructor for type " ++ name ++ ": \" ++ other"
               ]


field : Int -> String -> String -> ExtraPackage -> String
field n name dec extra =
    case n < 9 of
        True ->
            bracket <| " Decode.field " ++ name ++ " " ++ dec ++ " "

        False ->
            case extra of
                Extra ->
                    "|> Extra.andMap Decode.field(" ++ name ++ " " ++ dec ++ ")"

                Pipeline ->
                    "|> Pipeline.required " ++ name ++ " " ++ dec


mapper : Int -> String
mapper n =
    case n < 9 of
        True ->
            let
                suffix =
                    if n == 1 then
                        ""

                    else
                        String.fromInt n
            in
            "Decode.map" ++ suffix

        False ->
            "Decode.succeed"

var : Int -> String
var n =
    "a" ++ String.fromInt n


varOk : String -> String
varOk a =
    a ++ "_"


varUpper n =
    "A" ++ String.fromInt n
