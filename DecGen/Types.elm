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
    , def: String
    , isAlias: Bool  
    }

type alias TypeDef = {
      name: String
    , theType: Type  
    }

