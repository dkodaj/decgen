module Types exposing (..)


type Type
    = TypeArray Type
    | TypeBool
    | TypeDict ( Type, Type )
    | TypeError String --parse error
    | TypeFloat
    | TypeInt
    | TypeList Type
    | TypeMaybe Type
    | TypeOpaque String --type not core and not defined in the input
    | TypeProduct ( String, List Type )
    | TypeRecord (List Field)
    | TypeString
    | TypeTuple (List Type)
    | TypeUnion (List ( String, List Type ))

--options for decoding large record types
type ExtraPackage
    = Extra  --Json.Decode.Extra
    | Pipeline --Json.Decode.Pipeline

type alias Field =
    { name : String
    , fieldType : Type
    }


type alias RawType =
    { name : String
    , def : String
    }


type alias TypeDef =
    { name : String
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


coreTypeForEncoding : Type -> Bool
coreTypeForEncoding this =
    case this of
        TypeMaybe _ ->
            False

        _ ->
            coreType this


isRecord : TypeDef -> Bool
isRecord this =
    case this.theType of
        TypeRecord _ ->
            True

        _ ->
            False
