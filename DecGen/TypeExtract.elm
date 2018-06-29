module DecGen.TypeExtract exposing (extractAll, grabRawTypes, grabTypeDefs, typeNick)

import DecGen.AnonymousTypes exposing (grabAnonymousTypes)
import DecGen.Destructuring exposing (bracketIfSpaced, civilize, clean, debracket, decomment, derecord, detuple, deunion, dropWord, removeColons, removeStringLiterals, singleLine)
import DecGen.Types exposing (Field, RawType, Type(..), TypeDef)
import List exposing (map)
import Regex exposing (find, HowMany(..), Match, regex)
import String exposing (dropRight, trim, words)


aliasDefs: List TypeDef -> List (List String)
aliasDefs types =
    let
        def a = ["type alias " ++ a.name ++ " = " ++ (typeDescr True a.theType)]
    in
        map def types    

anonymousType: Type -> TypeDef
anonymousType a =
    { name = typeNick a, theType = a }

anonymousTypes: Bool -> List TypeDef -> List TypeDef
anonymousTypes encoding typeList =
    map anonymousType <| grabAnonymousTypes encoding typeList

extractAll: Bool ->String -> List TypeDef
extractAll encoding txt =
    let
        declared = grabTypeDefs txt
        anonymous = anonymousTypes encoding declared
    in
        declared ++ anonymous

extractAllWithDefs: Bool -> String -> ( List TypeDef, List (List String) )
extractAllWithDefs encoding txt =
    let
        declared = grabTypeDefs txt
        anonymous = anonymousTypes encoding declared
    in
        (declared ++ anonymous, aliasDefs anonymous)

grabTypeDefs: String -> List TypeDef
grabTypeDefs txt =
    let
        toTypeDef a =
            { name = a.name, theType = typeOf True a.def }
    in
        map toTypeDef <| grabRawTypes txt 

grabRawType: List (Maybe String) -> Maybe RawType
grabRawType submatches =
    case submatches of
        Just a:: Just b ::cs->
            Just { name = trim a, def = trim <| singleLine b }
        _->
            Nothing

grabRawTypes: String -> List RawType
grabRawTypes txt =
    clean <| map grabRawType <| map .submatches <| regexIt <| decomment <| removeStringLiterals txt

regexIt: String -> List Match
regexIt txt = find All typeRegex txt

typeRegex = regex "type\\s+(?:alias )?\\s*(\\w+)\\s*=([\\w(){},|.:_ \\r\\n]+)(?=(?:\\r\\w|\\n\\w)|$)"


--== Recognize types ==--

typeOf: Bool-> String  -> Type
typeOf maybeUnion def  = 
--  typeOf True "List String" == TypeList TypeString
--  typeOf False "List String" == TypeList TypeString
--  typeOf True "MyType | String" == TypeUnion [TypeOpaque "MyType", TypeString]
--  typeOf True "MyType" == TypeUnion [TypeOpaque "MyType"]
--  typeOf False "MyType" == TypeOpaque "MyType"
    let
        subType a = typeOf False a
    in
        case detuple def of
            Just (a,b)->
                TypeTuple (subType a, subType b)
            Nothing->
                case derecord def of
                    x::xs->
                        let
                            makeField (a,b) = Field a (subType b)
                        in
                            TypeRecord <| map makeField (x::xs)
                    []->
                        case words (debracket def) of
                            []->
                                TypeOpaque "Type conversion error: empty string"
                            x::xs->
                                case x of
                                    "Array"->
                                        TypeArray (subType <| dropWord x <| debracket def)
                                    "Bool"->
                                        TypeBool
                                    "Dict"->
                                        case deunion (debracket def) of
                                            (_,x::y::zs)::vs->
                                                TypeDict (subType x, subType y)
                                            _->
                                                TypeOpaque "Error parsing def as a Dict"
                                    "Dict.Dict"->
                                        case deunion (debracket def) of
                                            (_,x::y::zs)::vs->
                                                TypeDict (subType x, subType y)
                                            _->
                                                TypeOpaque "Error parsing def as a Dict"
                                    "Float"->
                                        TypeFloat
                                    "Int"->
                                        TypeInt
                                    "List"->
                                        TypeList (subType <| dropWord x <| debracket def)
                                    "Maybe"->
                                        TypeMaybe (subType <| dropWord x <| debracket def)
                                    "String"->
                                        TypeString
                                    _->
                                        case maybeUnion of
                                            True->
                                                let
                                                    constructor (a,b) = 
                                                        case b of
                                                            [""]->
                                                                (a, [])
                                                            _->
                                                                (a, map subType b)
                                                in
                                                    case deunion def of
                                                        x::[]->
                                                            TypeProduct (constructor x)
                                                        x::xs->
                                                            TypeUnion <| map constructor (x::xs)
                                                        []->
                                                            TypeOpaque "Union type conversion error: empty string"
                                            False->
                                                TypeOpaque (removeColons x)


typeDescr: Bool -> Type -> String
typeDescr bracketIt a =
--    typeDescr False (TypeList TypeInt) == "List Int"
--    typeDescr True (TypeList TypeInt) == "(List Int)"       
    let
        wrap a =
            if bracketIt
                then "("++a++")"
                else a
    in
        case a of
            TypeArray b->
               wrap <| "Array " ++ typeDescr True b
            TypeBool->
                "Bool"
            TypeDict (b,c)->
                "Dict " ++ (bracketIfSpaced <| typeDescr False b) ++ " " ++ (bracketIfSpaced <| typeDescr False c)
            TypeFloat->
                "Float"
            TypeInt->
                "Int"
            TypeList b->
                wrap <| "List " ++ typeDescr True b
            TypeMaybe b->
                wrap <| "Maybe " ++ typeDescr True b
            TypeOpaque b->
                b
            TypeProduct (b,c)->
                case c of
                    []->
                        b
                    _->
                        b ++ " " ++ (String.concat <| map (typeDescr True) c)
            TypeRecord b->
                let
                    fieldString x = x.name ++ ": " ++ typeDescr False x.fieldType ++ ", "
                    fields = dropRight 2 <| String.concat <| map fieldString b
                in
                    "{" ++ fields ++ "}"
            TypeString->
                "String"
            TypeTuple (b,c)->
                "(" ++ typeDescr False b ++ "," ++ typeDescr False c ++ ")"
            TypeUnion b->
                let
                    constructorString (x,y) =
                        case y of
                            []->
                                x ++ " | "
                            _->
                                x ++ " " ++ (String.concat <| map (typeDescr True) y) ++ " | "
                    constructors = dropRight 2 <| String.concat <| map constructorString b
                in
                    constructors

typeNick: Type -> String
typeNick a =
    let
        tag prefix = prefix ++ ( civilize <| typeDescr False a )
    in
        case a of
            TypeRecord _->
                tag "Record"
            TypeTuple _->
                tag "Tuple"
            _->
                tag ""

