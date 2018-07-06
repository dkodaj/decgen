module DecGen.Encoder exposing (encoder, encoderCapitalize)

import DecGen.Destructuring exposing (bracketCommas, bracketIfSpaced, capitalize, quote, tab, tabLines)
import DecGen.TypeExtract exposing (typeNick)
import DecGen.Types exposing (coreTypeForEncoding, Field, Type(..), TypeDef)
import List exposing (filter, indexedMap, length, map, map2, range)
import String exposing (contains, dropRight, join, split)

encoder = encoderBase False

encoderCapitalize = encoderBase True

encoderBase: Bool -> TypeDef -> List String
encoderBase capitalFields typeDef =
    let
        encoderBody = map (tab 1) <| split "\n" <| encoderHelp capitalFields True typeDef.name typeDef.theType
        encoderName = 
            case typeDef.theType of
                TypeTuple xs->
                    "encode" ++ typeDef.name ++ " ("++varListComma xs++") ="
                TypeProduct (b,c)->
                    case c of
                        []->
                            "encode" ++ typeDef.name ++ " a ="
                        _->
                            "encode" ++ typeDef.name ++ " ("++ b++" "++varList c ++") ="
                _->
                   "encode" ++ typeDef.name ++ " a =" 
        
        vars a = map var <| range 1 (length a)
        varList a = join " " (vars a)
        varListComma a = join ", " (vars a)
    in
        encoderName :: encoderBody

encoderHelp: Bool -> Bool-> String -> Type-> String
encoderHelp capitalFields topLevel name a =
    let
        maybeAppend txt =
            case topLevel of
                True->
                    (bracketIfSpaced txt) ++ " a"
                False->
                    txt
        recurseOn x y z =
            x ++ " << (" ++ y ++ " " ++ ( bracketIfSpaced <| encoderHelp False False "" z ) ++ ")"
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
                        encoderProduct True False b
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name
            TypeRecord b->
                case topLevel of
                    True->
                        encoderRecord capitalFields b                        
                    False->
                        case name of
                            ""->
                                "encode" ++ typeNick a
                            _->
                                "encode" ++ name
            TypeString->
                maybeAppend <| "Enc.string"
            
            TypeTuple bs->
                case topLevel of
                    True->
                        encoderTuple bs
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
            , tab 1 <| subEncoderName ++ " (a1,a2) ="
            , tab 2 "object"
            , tab 3 <| "[ (\"A1\", " ++ (bracketIfSpaced <| encoderHelp False False "" b) ++ " a1)" 
            , tab 3 <| ", (\"A2\", " ++ (bracketIfSpaced <| encoderHelp False False "" c) ++ " a2) ]" 
            , "in"
            , tab 1 <| "(Enc.list << List.map " ++ subEncoderName ++ ") (Dict.toList a)"
            ]

encoderMaybe: Type -> String
encoderMaybe x =
    join "\n" <|
        [ "case a of"
        , tab 1 "Just b->"
        , tab 2 <| (bracketIfSpaced <| encoderHelp False False "" x) ++ " b"
        , tab 1 "Nothing->"
        , tab 2 "Enc.null" 
        ]

encoderProduct: Bool -> Bool -> (String, List Type) -> String
encoderProduct productType addConstructor (constructor, subTypes) =
    let
        fieldDefs = map2 (\a b -> (a,b)) vars subTypes
        fieldEncode (a,b) = "(" ++ (quote <| capitalize a) ++ ", " ++ (subEncoder b) ++ " " ++ a ++ ")"
        vars = map var <| range 1 (length subTypes)
        subEncoder a = 
            let
                fullEncoder = dropRight 2 <| encoderHelp False True "" a
            in
                case coreTypeForEncoding a of
                    True->
                        fullEncoder
                    False->
                        bracketIfSpaced <| encoderHelp False False "" a                        
        constrEncode =
                case addConstructor of
                    False->
                        []
                    True->
                        ["(\"Constructor\", Enc.string " ++ quote constructor ++")"]
        defaultEncoder = 
            join "\n" <|
                ["object"] ++
                (map (tab 1) <| bracketCommas <| constrEncode ++ map fieldEncode fieldDefs) ++
                [ tab 1 "]"] 
    in
        case subTypes of
            []->
                case addConstructor of
                    True->
                        defaultEncoder
                    False->
                        "Enc.string " ++ quote constructor
            x::[]->
                case productType of
                    True->
                        subEncoder x  ++ " a1"
                    False->
                        defaultEncoder
            _->
                defaultEncoder
                

encoderRecord: Bool -> List Field -> String
encoderRecord capitalFields xs =
    let
        fieldEncode x = "(" ++ (fieldName x) ++ ", " ++ (subEncoder x.fieldType) ++ " a." ++ x.name ++ ")"
        subEncoder x = bracketIfSpaced <| encoderHelp False False "" x
        fieldName x = 
            case capitalFields of
                True->
                    quote <| capitalize x.name
                False->
                    quote x.name
    in
        join "\n" <|
            ["object"] ++
            (map (tab 1) <| bracketCommas <| map fieldEncode xs) ++
            [ tab 1 "]"]

encoderTuple: List Type -> String
encoderTuple xs =
    let
        encodeElement (idx,elem) =
          "(\"" ++ varUpper (idx+1) ++ "\", " ++ (bracketIfSpaced <| encoderHelp False False "" elem) ++ " " ++ var (idx+1) ++ ")"
    in
    join "\n" <|
        [ "object" ] ++
        (map (tab 1) <| bracketCommas <| map encodeElement <| indexedMap (,) xs) ++
        [ tab 1 "]"]

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
                encoderUnionComplex xs

encoderUnionSimple: List (String, List Type) -> String
encoderUnionSimple xs =
    "Enc.string <| toString a"

encoderUnionComplex: List (String, List Type) -> String
encoderUnionComplex xs =
    let
        varList ys = join " " <| map var <| range 1 (length ys)
        encodeConstructor (a, ys) =
            tab 1 (a ++ " " ++ varList ys ++ "->" ++ "\n") ++ tabLines 2 (encoderProduct False True (a,ys))
    in
        join "\n" <|
            ["case a of"] ++
            ( map encodeConstructor xs )

var n = "a" ++ toString n

varUpper n = "A" ++ toString n