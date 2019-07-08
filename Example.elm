module Example exposing (Record, Union, Dict)
import Dict

type alias Dict = Dict.Dict Int Record
type alias Record = {
    primitive : String
  , union : Union
  }

type Union = A | B String
