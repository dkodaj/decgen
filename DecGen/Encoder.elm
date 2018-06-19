module DecGen.Encoder exposing (encoder)

import DecGen.Destructuring exposing (bracketIfSpaced, capitalize, quote, tab)
import DecGen.TypeExtract exposing (typeNick)
import DecGen.Types exposing (Field, Type(..), TypeDef)
import List exposing (filter, indexedMap, length, map, map2, range)
import String exposing (join, split)


encoder: TypeDef -> List String
encoder typeDef =
    let
        encoderBody = map (tab 1) <| split "\n" <| encoderHelp True typeDef.name typeDef.theType
        encoderName = 
            case typeDef.theType of
                TypeTuple _->
                    "encode" ++ typeDef.name ++ " (a,b) ="
                TypeProduct (b,c)->
                    case c of
                        []->
                            "encode" ++ typeDef.name ++ " a ="
                        x::[]->
                            "encode" ++ typeDef.name ++ " ("++ b++" a) ="
                        _->
                            let
                                vars = map var (range 0 (length c - 1))
                                varList = join " " vars
                            in
                                "encode" ++ typeDef.name ++ " ("++ b++" "++varList ++") ="
                _->
                   "encode" ++ typeDef.name ++ " a =" 
    in
        encoderName :: encoderBody

encoderHelp: Bool-> String -> Type-> String
encoderHelp topLevel name a =
    let
        maybeAppend txt =
            case topLevel of
                True->
                    (bracketIfSpaced txt) ++ " a"
                False->
                    txt
        recurseOn x y z =
            x ++ " << (" ++ y ++ " " ++ ( bracketIfSpaced <| encoderHelp False "" z ) ++ ")"
    in
        case a of            
            TypeArray b->
                maybeAppend <| recurseOn "Enc.array" "Array.map" b
            TypeBool->
                maybeAppend <| "Enc.bool"
            TypeDict (b,c)->
                case topLevel of
                    True->
                        encoderDict name (b,c)
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name
            TypeFloat->
                maybeAppend <| "Enc.float"
            TypeInt->
                maybeAppend <| "Enc.int"
            TypeList b->
                maybeAppend <| recurseOn "Enc.list" "List.map" b
            TypeMaybe b->
                case topLevel of
                    True->
                        encoderMaybe b
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name     
            TypeOpaque b->
                maybeAppend <| "encode"++b
            
            TypeProduct b->
                case topLevel of
                    True->
                        encoderProduct b
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name
            TypeRecord b->
                case topLevel of
                    True->
                        encoderRecord b                        
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name
            TypeString->
                maybeAppend <| "Enc.string"
            
            TypeTuple (b,c)->
                case topLevel of
                    True->
                        encoderTuple (b,c)
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name
            TypeUnion b->
                case topLevel of
                    True->
                        encoderUnion b
                    False->
                        case name of
                            ""->
                                "Encoder parse error: unanymous union type: " ++ toString b
                            _->
                                "encode" ++ name

encoderDict: String -> (Type, Type) -> String
encoderDict name (b,c) =
    let
        subEncoderName = "encode" ++ name ++ "Tuple"
    in
        join "\n" <|
            [ "let"
            , tab 1 <| subEncoderName ++ " (b,c) ="
            , tab 2 "object"
            , tab 3 <| "[ (\"First\", " ++ (bracketIfSpaced <| encoderHelp False "" b) ++ " b)" 
            , tab 3 <| ", (\"Second\", " ++ (bracketIfSpaced <| encoderHelp False "" c) ++ " c) ]" 
            , "in"
            , tab 1 <| "(Enc.list << List.map " ++ subEncoderName ++ ") (Dict.toList a)"
            ]

encoderMaybe: Type -> String
encoderMaybe x =
    join "\n" <|
        [ "case a of"
        , tab 1 "Just b->"
        , tab 2 <| encoderHelp False "" x ++ " b"
        , tab 1 "Nothing->"
        , tab 2 "Enc.null" 
        ]

encoderProduct: (String, List Type) -> String
encoderProduct (constructor, subTypes) =
    let
        fieldEncode (a,b) = "(" ++ (quote <| capitalize a) ++ ", " ++ (subEncoder b) ++ " " ++ a ++ ")"
        fields = map (\(a,b)->(var a,b)) <| indexedMap (,) subTypes
        subEncoder a = bracketIfSpaced <| encoderHelp False "" a
    in
        case subTypes of
            []->
                "Enc.string " ++ constructor
            x::[]->
                subEncoder x ++ " a"
            _->
                join "\n" <|
                    ["object"] ++
                    (map (tab 1) <| bracketCommas <| map fieldEncode fields) ++
                    [ tab 1 "]"]

encoderRecord: List Field -> String
encoderRecord xs =
    let
        fieldEncode x = "(" ++ (quote <| capitalize x.name) ++ ", " ++ (subEncoder x.fieldType) ++ " a." ++ x.name ++ ")"
        subEncoder x = bracketIfSpaced <| encoderHelp False "" x
    in
        join "\n" <|
            ["object"] ++
            (map (tab 1) <| bracketCommas <| map fieldEncode xs) ++
            [ tab 1 "]"]

bracketCommas: List String -> List String
bracketCommas xs = 
    let
        glue a b = a ++ " " ++ b
        separators = "[" :: List.repeat (List.length xs - 1) ","
    in
        map2 glue separators xs

encoderTuple: (Type, Type) -> String
encoderTuple (a,b) =
    join "\n" <|
        [ "object"
        , tab 1 <| "[ (\"First\", " ++ (bracketIfSpaced <| encoderHelp False "" a) ++ " a)" 
        , tab 1 <| ", (\"Second\", " ++ (bracketIfSpaced <| encoderHelp False "" b) ++ " b)" 
        , tab 1 "]"
        ]

encoderUnion: List (String, List Type) -> String
encoderUnion xs =
    let
        complexConstructor (a,b) = (b /= [])
        simpleUnion = filter complexConstructor xs == []
    in
        case simpleUnion of
            True->
                encoderUnionSimple xs
            False->
                encoderUnionUgly xs

encoderUnionSimple: List (String, List Type) -> String
encoderUnionSimple xs =
    "Enc.string <| toString a"

encoderUnionUgly: List (String, List Type) -> String
encoderUnionUgly xs =
    let
        x = 1
    in
        join "\n" <|
            ["case a of"] ++
            ( map unionCase xs )

unionCase: (String, List Type) -> String
unionCase (constructor, types) =
    let
        toTuple a b = (a,b)
        vars = map var <| range 1 (List.length types)
        varList = join " " vars
        varsTypes = map2 toTuple vars types
    in
        join "\n" <|
            [ tab 1 <| constructor ++ " " ++ varList ++ "->"
            , tab 2 "Enc.list"
            , tab 3 <| "[ Enc.string " ++ quote constructor 
            ] ++
            ( map (tab 3) <| map unionCaseHelp varsTypes ) ++
            [ tab 3 "]" ]

unionCaseHelp: (String, Type) -> String
unionCaseHelp (varName, theType) =
    ", " ++ (bracketIfSpaced <| encoderHelp False "" theType) ++ " " ++ varName

var n = "a"++toString n