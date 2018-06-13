module DecGen.TypeExtract exposing (extractAll, grabRawTypes, grabTypeDefs, typeNick)

import DecGen.AnonymousTypes exposing (grabAnonymousTypes)
import DecGen.Destructuring exposing (..)
import DecGen.Types exposing (Field, RawType, Type(..), TypeDef)

import List exposing (filter, foldl, foldr, map, range, reverse)
import Regex exposing (find, HowMany(..), Match, regex, replace)
import String exposing (contains, dropLeft, dropRight, fromChar, join, indices, left, length, right, split, repeat, toUpper, trim, words)



aliasDefs: List TypeDef -> List (List String)
aliasDefs types =
    let
        def a = ["type alias " ++ a.name ++ " = " ++ (typeDescr True a.theType)]
    in
        map def types    

anonymousType: Type -> TypeDef
anonymousType a =
    { name = typeNick a, theType = a }

anonymousTypes: List TypeDef -> List TypeDef
anonymousTypes typeList =
    map anonymousType <| grabAnonymousTypes typeList

extractAll: String -> ( List TypeDef, List (List String) )
extractAll txt =
    let
        declared = grabTypeDefs txt
        anonymous = anonymousTypes declared
    in
        (declared ++ anonymous, aliasDefs anonymous)

grabTypeDefs: String -> List TypeDef
grabTypeDefs txt =
    let
        toTypeDef a =
            { name = a.name, theType = typeOf True a.typeDef }
    in
        map toTypeDef <| grabRawTypes txt 

grabRawType: List (Maybe String) -> Maybe RawType
grabRawType submatches =
    case submatches of
        Just a:: Just b ::cs->
            Just { name = trim a, typeDef = trim <| singleLine b }
        _->
            Nothing

grabRawTypes: String -> List RawType
grabRawTypes txt =
    clean <| map grabRawType <| map .submatches <| regexIt <| decomment txt

regexIt: String -> List Match
regexIt txt = find All typeRegex txt

typeRegex = regex "type\\s+(?:alias )?\\s*(\\w+)\\s*=([\\w(){},|.:_ \\r\\n]+)(?=(?:\\r\\w|\\n\\w)|$)"


--== Recognize types ==--

typeOf: Bool -> String -> Type
typeOf checkUnion def = 
--  typeOf x "List String" == TypeList TypeString
--  typeOf x "(String, Int)" == TypeTuple (TypeString, TypeInt)
--  typeOf True "A String | B" == TypeUnion [ ("A", [TypeString] ), ("B", []) ]
--  tyoeOf False "A String | B" == TypeAlias "A String | B"
--  typeOf True "A" == TypeUnion [ ("A", []) ]
--  typeOf False "A" == TypeAlias "A"
    case detuple def of
        Just (a,b)->
            TypeTuple (typeOf False a, typeOf False b)
        Nothing->
            case derecord def of
                x::xs->
                    let
                        makeField (a,b) = Field a (typeOf False b)
                    in
                        TypeRecord <| map makeField (x::xs)
                []->
                    case words (debracket def) of
                        []->
                            TypeAlias "Type conversion error: empty string"
                        x::xs->
                            case x of
                                "Array"->
                                    TypeArray (typeOf False <| dropWord x <| debracket def)
                                "Bool"->
                                    TypeBool
                                "Dict"->
                                    case deunion (debracket def) of
                                        (_,x::y::zs)::vs->
                                            TypeDict (typeOf False x, typeOf False y)
                                        _->
                                            TypeAlias "Error parsing def as a Dict"
                                "Dict.Dict"->
                                    case deunion (debracket def) of
                                        (_,x::y::zs)::vs->
                                            TypeDict (typeOf False x, typeOf False y)
                                        _->
                                            TypeAlias "Error parsing def as a Dict"
                                "Float"->
                                    TypeFloat
                                "Int"->
                                    TypeInt
                                "List"->
                                    TypeList (typeOf False <| dropWord x <| debracket def)
                                "Maybe"->
                                    TypeMaybe (typeOf False <| dropWord x <| debracket def)
                                "String"->
                                    TypeString
                                _->
                                    case checkUnion of
                                        True->
                                            case deunion def of
                                                x::xs->
                                                    let
                                                        constructor (a,b) = 
                                                            case b of
                                                                [""]->
                                                                    (a, [])
                                                                _->
                                                                    (a, map (typeOf False) b)
                                                    in
                                                        TypeUnion <| map constructor (x::xs)
                                                []->
                                                    TypeAlias "Union type conversion error: empty string"
                                        False->
                                            TypeAlias (removeColons x)

typeDescr: Bool -> Type -> String
typeDescr bracketIt a =
--    typeDescr False <| TypeList TypeInt == "List Int"
--    typeDescr True <| TypeList TypeInt == "(List Int)"       
    let
        wrap a =
            if bracketIt
                then "("++a++")"
                else a
    in
        case a of
            TypeAlias b->
                b
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

