module DecGen.Types exposing (..)

type Type = 
      TypeAlias String
    | TypeArray Type
    | TypeBool
    | TypeDict (Type, Type)
    | TypeFloat
    | TypeInt
    | TypeList Type
    | TypeMaybe Type
    | TypeRecord (List Field)
    | TypeString
    | TypeTuple (Type, Type)
    | TypeUnion ( List (String, List Type) )

type alias Field = { 
      name: String
    , fieldType: Type 
    }

type alias RawType = {
      name: String
    , typeDef: String  
    }

type alias TypeDef = {
      name: String
    , theType: Type  
    }

