module DecGen.Decoder exposing (decoder, decoderCapitalize)

import DecGen.Destructuring exposing (bracketIfSpaced, capitalize, quote, tab, tabLines)
import DecGen.TypeExtract exposing (typeNick)
import DecGen.Types exposing (coreType, Field, Type(..), TypeDef)
import List exposing (concat, filter, indexedMap, length, map, map2, range)
import String exposing (join, split)

decoder = decoderBase False

decoderCapitalize = decoderBase True

decoderBase: Bool -> TypeDef -> List String
decoderBase capitalFields typeDef =
    let
        decoderBody = 
            case typeDef.theType of
                TypeUnion _->
                    decoderBodyRaw
                _->
                    map (tab 1) decoderBodyRaw
        decoderBodyRaw = split "\n" <| decoderHelp capitalFields True typeDef.name typeDef.theType
        decoderName = "decode" ++ typeDef.name ++ " ="
    in
        decoderName :: decoderBody

decoderHelp: Bool -> Bool-> String -> Type-> String
decoderHelp capitalFields topLevel name a =
    let
        recurseOn x y =
            x ++ " " ++ ( bracketIfSpaced <| decoderHelp False False "" y )
    in
        case a of
            TypeArray b->
                recurseOn "Dec.array" b
            TypeBool->
                "Dec.bool"
            TypeDict (b,c) ->
                case topLevel of
                    True->
                        let
                            subDecoderName = "decode"++name++"Tuple"
                            subDecoder = decoderHelp False True "" (TypeTuple [b,c])
                        in
                            join "\n" [
                              "let"
                            , tab 1 <| subDecoderName ++ " ="
                            , tabLines 2 <| subDecoder
                            , "in"
                            , tab 1 <| "Dec.map Dict.fromList (Dec.list " ++ subDecoderName ++")"  
                            ]
                    False->
                        case name of
                            ""->
                                "decode" ++ typeNick a
                            _->
                                "decode" ++ name
            TypeFloat->
                "Dec.float"
            TypeInt->
                "Dec.int"
            TypeList b->
                recurseOn "Dec.list" b
            TypeMaybe b->
                recurseOn "Dec.nullable" b
            TypeOpaque b->
                "decode"++b
            TypeProduct b->
                case topLevel of
                    True->
                        decoderProduct True b
                    False->
                        case name of
                            ""->
                                "decode" ++ typeNick a
                            _->
                                "decode" ++ name
            TypeRecord b->
                case topLevel of
                    True->
                        case name of
                            ""->
                                decoderRecord capitalFields (typeNick a) b
                            _->
                                decoderRecord capitalFields name b                        
                    False->
                        case name of
                            ""->
                                "decode" ++ typeNick a
                            _->
                                "decode" ++ name
            TypeString->
                "Dec.string"
            
            TypeTuple bs->
                case topLevel of
                    True->
                        decoderTuple bs
                    False->
                        case name of
                            ""->
                                "decode" ++ typeNick a
                            _->
                                "decode" ++ name
            TypeUnion b->
                case topLevel of
                    True->
                        decoderUnion name b
                    False->
                        case name of
                            ""->
                                "Decoder parse error: unanymous union type: " ++ toString b
                            _->
                                "decode" ++ name

decoderProduct: Bool -> (String, List Type) -> String
decoderProduct productType (constructor, subTypes) =
    let
        fieldDecode (a,b) = "|> required " ++ (quote <| capitalize a) ++ " " ++ (subDecoder b)
        fieldDefs = map2 (\a b->(a,b)) vars subTypes
        subDecoder a = bracketIfSpaced <| decoderHelp False False "" a
        vars = map var <| range 1 (length subTypes)
        simpleDecoder x = "Dec.map " ++ constructor ++ " " ++ (subDecoder x)
        complexDecoder = 
            join "\n" <|
                [ "decode"
                , tab 1 constructor
                ] ++
                (map (tab 2) <| map fieldDecode fieldDefs)
    in
         case subTypes of
            []->
                "Dec.succeed " ++ constructor
            x::[]->
                if productType then simpleDecoder x else complexDecoder
            _->
                complexDecoder

decoderRecord: Bool -> String -> List Field -> String
decoderRecord capitalFields name xs =
    let
        fieldDecode x = "|> required " ++ (fieldName x) ++ " " ++ (subDecoder x.fieldType)
        subDecoder x = bracketIfSpaced <| decoderHelp False False "" x
        fieldName x = 
            case capitalFields of
                True->
                    quote <| capitalize x.name
                False->
                    quote x.name
    in
        join "\n" <|
            [ "decode"
            , tab 1 name
            ] ++
            (map (tab 2) <| map fieldDecode xs)

decoderTuple: List Type -> String
decoderTuple xs =
    let
        component (idx,elem) = tab 2 <| "|> required " ++ (quote <| varUpper <| idx+1) ++ " " ++ (subDecoder elem)
        mapper = "(\\"++varList ++ " -> "++varListComma ++ ")"
        subDecoder x = bracketIfSpaced <| decoderHelp False False "" x
        vars = map var <| range 1 (length xs)
        varList = join " " vars
        varListComma = "(" ++ join ", " vars ++ ")"
    in
        "decode" ++ "\n" ++
        (tab 1 mapper) ++ "\n" ++
        (join "\n" <| map component <| indexedMap (,) xs)

decoderUnion: String -> List (String, List Type) -> String
decoderUnion name xs =
    let
        complexConstructor (a,b) = (b /= [])
        simpleUnion = filter complexConstructor xs == []
    in
        case simpleUnion of
            True->
                decoderUnionSimple name xs
            False->
                decoderUnionComplex name xs

decoderUnionSimple: String -> List (String, List Type) -> String
--e.g. type Color = Red | Green | Blue 
decoderUnionSimple name xs =
    let
        constructor (a,b) =
            (quote a ++ "->\n") ++ (tab 2 <| "Dec.succeed " ++ a)
    in
        join "\n" <| map (tab 1) <|
            ["let", tab 1 "recover x =", tab 2 "case x of"] ++
            (map (tabLines 3) <| map constructor xs) ++
            [tab 3 "other->", tab 4 "Dec.fail <| \"Unknown constructor for type "++name++": \" ++ other" ] ++
            ["in", tab 1 "Dec.string |> andThen recover"]

decoderUnionComplex: String -> List (String, List Type) -> String
decoderUnionComplex name xs =
    let
        decodeConstructor (constructor, fields) =
            quote constructor ++  " ->\n" ++ (tabLines 1 <| decoderProduct False (constructor, fields))
    in
        join "\n" <|
            [ tab 1 <| "Dec.field \"Constructor\" Dec.string |> andThen decode" ++ name ++ "Help" ++ "\n"
            , "decode"++name++"Help constructor ="
            , tab 1 "case constructor of"
            ] ++ 
            (map (tabLines 2) <| map decodeConstructor xs) ++
            [ tab 2 "other->"
            , tab 3 <| "Dec.fail <| \"Unknown constructor for type " ++ name ++": \" ++ other"
            ]

var: Int -> String
var n = "a" ++ toString n

varOk: String -> String
varOk a = a++"_"

varUpper n = "A" ++ toString n