module Types exposing (..)

import Destructuring exposing (bracketIfSpaced)


type Type
    = TypeArray Type
    | TypeBool
    | TypeDict ( Type, Type )
    | TypeError String --parse error
    | TypeExtendedRecord (List TypeDef) --record defined using an extensible one
    | TypeExtensible (List TypeDef) --extensible record
    | TypeFloat
    | TypeImported String --type not core and not defined in the input
    | TypeInt
    | TypeList Type
    | TypeMaybe Type
    | TypeProduct ( String, List Type )
    | TypeRecord (List TypeDef)
    | TypeString
    | TypeTuple (List Type)
    | TypeUnion (List ( String, List Type ))

--options for decoding large record types
type ExtraPackage
    = Extra  --Json.Decode.Extra
    | Pipeline --Json.Decode.Pipeline

type alias RawType =
    { name : String
    , def : String
    , extensible : Bool
    }

type alias TypeDef =
    { name : Maybe String
    , generatedName : String
    , theType : Type
    }


simpleType this =
    case this of
        TypeBool ->
            True

        TypeFloat ->
            True

        TypeInt ->
            True

        TypeString ->
            True

        _ ->
            False


coreType : Type -> Bool
coreType this =
    case this of
        TypeArray a ->
            simpleType a

        TypeList a ->
            simpleType a

        TypeMaybe a ->
            simpleType a

        _ ->
            simpleType this


isEmptyRecord : TypeDef -> Bool
isEmptyRecord this =
    case this.theType of
        TypeRecord [] ->
            True
        
        _ ->
            False

isNonemptyExtended : TypeDef -> Bool
isNonemptyExtended this =
    case this.theType of
        TypeExtendedRecord [] ->
            False
            
        TypeExtendedRecord _ ->
            True    
            
        _ ->
            False


isExtensible : TypeDef -> Bool
isExtensible this =
    case this.theType of
        TypeExtensible _ ->
            True

        _ ->
            False


isRecord : TypeDef -> Bool
isRecord this =
    case this.theType of
        TypeRecord _ ->
            True

        _ ->
            False


toString : TypeDef -> Maybe String
toString a =
    case a.name of
        Just name ->
            Just name

        Nothing ->
            typeToString a.theType


typeToString : Type -> Maybe String
typeToString a =
    let        
        f : String -> String -> String
        f x y =
            x ++ " " ++ y

        g : Type -> Maybe String
        g x =
            Maybe.map bracketIfSpaced (typeToString x)

        h : String -> Maybe String -> Maybe String
        h x y =
            Maybe.map (f x) y
    in
    case a of
        TypeArray b ->
            h "Array" (g b)
        
        TypeBool ->
            Just "Bool"

        TypeDict (b,c) ->
            h "Dict" (Maybe.map2 f (g b) (g c))
        
        TypeError _ ->
            Nothing
        
        TypeExtendedRecord _ ->
            Nothing
        
        TypeExtensible _ ->
            Nothing
        
        TypeFloat ->
            Just "Float"
        
        TypeImported name ->
            Just name
        
        TypeInt ->
            Just "Int"
        
        TypeList b ->
            h "List" (g b)

        TypeMaybe b ->
            h "Maybe" (g b)

        TypeProduct ( name, _ ) ->
            Just name

        TypeRecord _ ->
            Nothing

        TypeString ->
            Just "String"

        TypeTuple list ->
            let
                typeFold : Type -> Maybe (List String) -> Maybe (List String)
                typeFold x result =
                    case result of
                        Nothing ->
                            Nothing

                        Just ys ->
                            case g x of
                                Just y -> Just (y :: ys)
                                Nothing -> Nothing
            in
            case List.foldr typeFold (Just []) list of
                Nothing ->
                    Nothing

                Just [] ->
                    Nothing

                Just strings ->
                    Just <| "(" ++ String.join ", " strings ++ ")"

        TypeUnion _ ->
            Nothing