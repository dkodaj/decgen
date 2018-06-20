module DecGen.Decoder exposing (decoder)

import DecGen.Destructuring exposing (bracketIfSpaced, capitalize, quote, tab, tabLines)
import DecGen.TypeExtract exposing (typeNick)
import DecGen.Types exposing (coreType, Field, Type(..), TypeDef)
import List exposing (concat, filter, length, map, map2, range)
import String exposing (join, split)

decoder: TypeDef -> List String
decoder typeDef =
    let
        decoderBody = 
            case typeDef.theType of
                TypeUnion _->
                    decoderBodyRaw
                _->
                    map (tab 1) decoderBodyRaw
        decoderBodyRaw = split "\n" <| decoderHelp True typeDef.name typeDef.theType
        decoderName = "decode" ++ typeDef.name ++ " ="
    in
        decoderName :: decoderBody

decoderHelp: Bool-> String -> Type-> String
decoderHelp topLevel name a =
    let
        recurseOn x y =
            x ++ " " ++ ( bracketIfSpaced <| decoderHelp False "" y )
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
                            subDecoder = decoderHelp True "" (TypeTuple (b,c))
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
                        decoderProduct b
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
                                decoderRecord (typeNick a) b
                            _->
                                decoderRecord name b                        
                    False->
                        case name of
                            ""->
                                "decode" ++ typeNick a
                            _->
                                "decode" ++ name
            TypeString->
                "Dec.string"
            
            TypeTuple (b,c)->
                case topLevel of
                    True->
                        let
                            subDecoder x = bracketIfSpaced <| decoderHelp False "" x
                        in
                            "decode" ++ "\n" ++
                            (tab 1 "(\\x y -> (x,y))") ++ "\n" ++
                            (tab 2 <| "|> required ") ++ (quote "First") ++ " " ++ (subDecoder b) ++ "\n" ++
                            (tab 2 <| "|> required ") ++ (quote "Second") ++ " " ++ (subDecoder c)
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

decoderProduct: (String, List Type) -> String
decoderProduct (constructor, subTypes) =
    let
        fieldDecode (a,b) = "|> required " ++ (quote <| capitalize a) ++ " " ++ (subDecoder b)
        fieldDefs = map2 (\a b->(a,b)) vars subTypes
        subDecoder a = bracketIfSpaced <| decoderHelp False "" a
        vars = map var <| range 1 (length subTypes)
        subEncoder a = 
            let
                fullDecoder = decoderHelp True "" a
            in
                case coreType a of
                    True->
                        bracketIfSpaced <| fullDecoder
                    False->
                        bracketIfSpaced <| decoderHelp False "" a   
    in
         case subTypes of
            []->
                "Dec.succeed " ++ constructor
            x::[]->
                "Dec.map " ++ constructor ++ " " ++ subDecoder x
            _->
                join "\n" <|
                    [ "decode"
                    , tab 1 constructor
                    ] ++
                    (map (tab 2) <| map fieldDecode fieldDefs)

decoderRecord: String -> List Field -> String
decoderRecord name xs =
    let
        fieldDecode x = "|> required " ++ (quote <| capitalize x.name) ++ " " ++ (subDecoder x.fieldType)
        subDecoder x = bracketIfSpaced <| decoderHelp False "" x
    in
        join "\n" <|
            [ "decode"
            , tab 1 name
            ] ++
            (map (tab 2) <| map fieldDecode xs)

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
            quote constructor ++  " ->\n" ++ (tabLines 1 <| decoderProduct (constructor, fields))
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