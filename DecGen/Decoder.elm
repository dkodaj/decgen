module DecGen.Decoder exposing (decoder)

import DecGen.Destructuring exposing (..)
import DecGen.Types exposing (..)
import DecGen.TypeExtract exposing (typeNick)

import List exposing (concat, filter, foldl, foldr, map, range, reverse)
import String exposing (contains, dropLeft, dropRight, fromChar, join, indices, left, length, right, split, repeat, toUpper, trim, words)

decoder: TypeDef -> List String
decoder typeDef =
    let
        decoderBody = map (tab 1) <| split "\n" <| decoderHelp True typeDef.name typeDef.theType
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
            TypeAlias b->
                "decode"++b
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
                        decoderUnion b
                    False->
                        case name of
                            ""->
                                "Decoder parse error: unanymous union type: " ++ toString b
                            _->
                                "decode" ++ name

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

decoderUnion: List (String, List Type) -> String
decoderUnion xs =
    let
        complexConstructor (a,b) = (b /= [])
        simpleUnion = filter complexConstructor xs == []
    in
        case simpleUnion of
            True->
                decoderUnionSimple xs
            False->
                decoderUnionUgly xs

decoderUnionSimple: List (String, List Type) -> String
decoderUnionSimple xs =
    let
        constructor (a,b) =
            (tab 3 <| quote a ++ "->\n") ++ (tab 4 <| "Dec.succeed " ++ a)
    in
        join "\n" <|
            ["let", tab 1 "recover x =", tab 2 "case a of"] ++
            (map constructor xs) ++
            [tab 3 "other->", tab 4 "Dec.fail <| \"Invalid constructor field found: \" ++ other" ] ++
            ["in", tab 1 "Dec.string |> andThen recover"]

decoderUnionUgly: List (String, List Type) -> String
decoderUnionUgly xs =
    let
        a = "b"
    in
        join "\n" <|
            [ "let"
            , tab 1 "recover xs ="
            , tab 2 "case xs of"
            , tab 3 "a0::bs->"
            , tab 4 "case decodeValue Dec.string a0 of"
            ] ++
            ( map (tab 5) <| concat <| map unionCase xs) ++
            [tab 5 "Ok other->", tab 6 "Dec.fail <| \"Invalid constructor field found: \" ++ other"] ++
            [tab 5 "Err err->", tab 6 "Dec.fail err"] ++
            [tab 3 "_->", tab 4 "Dec.fail \"Invalid JSON input: empty list\""] ++
            ["in", tab 1 "Dec.list Dec.value |> andThen recover"]   

unionCase: (String, List Type) -> List String
unionCase (constructor, types) =
    case types of
        []->
            [ "Ok " ++ quote constructor ++ "->"
            , tab 1 <| "Dec.succeed " ++ constructor
            ]
        _->
            [ "Ok " ++ quote constructor ++"->"
            , tab 1 "case bs of"
            ] ++
            (map (tab 2) <| unionCaseHelp (constructor, types)) ++
            [ tab 2 "_->"
            , tab 3 <| "Dec.fail <| \"Invalid fields for constructor " ++ constructor ++ ": \" ++ toString bs"
            ] 

unionCaseHelp: (String, List Type) -> List String
unionCaseHelp (constructor, types) =
    let
        caseString = (join "::" vars) ++ "::cs ->"
        okList = join " " <| map varOk vars
        recursionBase = toTuples vars types
        toTuples xs ys = List.map2 (\x y -> (x,y)) xs ys
        vars = map var <| range 1 (List.length types)
    in
        caseString :: map (tab 2) (unionCaseRecursion constructor okList recursionBase)

unionCaseRecursion: String -> String -> List (String, Type) -> List String
unionCaseRecursion constructor okList xs =
    let
        recurseOn ys =
            case ys of
                []->
                    ["Dec.succeed <| "++constructor++" "++ okList]
                _->
                    unionCaseRecursion constructor okList ys
    in
        case xs of
            []->
                []
            (var,currType)::ys->
                [ "case decodeValue " ++ (bracketIfSpaced <| decoderHelp False "" currType) ++ " " ++ var ++ " of"
                , tab 1 <| "Ok " ++ (varOk var) ++"->"
                ] ++
                (map (tab 2) <| recurseOn ys) ++
                [ tab 1 <| "Err err->"
                , tab 2 <| "Dec.fail err"
                ]   

var: Int -> String
var n = "a" ++ toString n

varOk: String -> String
varOk a = a++"_"