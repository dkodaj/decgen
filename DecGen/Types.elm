module DecGen.Types exposing (..)

type Type = 
      TypeArray Type
    | TypeBool
    | TypeDict (Type, Type)
    | TypeFloat
    | TypeInt
    | TypeList Type
    | TypeMaybe Type
    | TypeOpaque String --a type not defined in the source
    | TypeProduct (String, List Type)
    | TypeRecord (List Field)
    | TypeString
    | TypeTuple (List Type)
    | TypeUnion ( List (String, List Type) )

type alias Field = { 
      name: String
    , fieldType: Type 
    }

type alias RawType = {
      name: String
    , def: String
    }

type alias TypeDef = {
      name: String
    , theType: Type  
    }

simpleType this =
    case this of
        TypeBool->
            True
        TypeFloat->
            True
        TypeInt->
            True
        TypeString->
            True
        _->
            False

coreType: Type -> Bool
coreType this =
    case this of
        TypeArray a->
            simpleType a
        TypeList a->
            simpleType a
        TypeMaybe a->
            simpleType a
        _->
            simpleType this

coreTypeForEncoding: Type -> Bool
coreTypeForEncoding this =
    case this of
        TypeMaybe _->
            False
        _->
            coreType this

isRecord: TypeDef -> Bool
isRecord this =
    case this.theType of
        TypeRecord _->
            True
        _->
            False