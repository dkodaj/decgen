module DecGen exposing (both, decoders, encoders, imports)

import DecGen.Decoder exposing (decoder)
import DecGen.Encoder exposing (encoder)
import DecGen.TypeExtract exposing (extractAll)
import List exposing (concat, map, sortBy)
import String exposing (contains, join)

both: String -> String
both txt =
    (decoders txt) ++ "\n\n" ++ (encoders txt)

decoders: String -> String
decoders txt =
    let
        allTypes = extractAll False txt
    in
        stringify <|
            (map decoder <| sortBy .name allTypes )

encoders: String -> String
encoders txt =
    let
        allTypes = extractAll True txt
    in
        stringify <|
            (map encoder <| sortBy .name allTypes )

imports output =
    let
        maybe txt keyWord =
            case contains keyWord output of
                True->
                    [txt]
                False->
                    []
        importDict = maybe "import Dict exposing (Dict)" "Dict."
        importDec = maybe "import Json.Decode as Dec exposing (andThen, decodeValue)" " Dec."
        importDecPipe = maybe "import Json.Decode.Pipeline exposing (decode, required)" "|> required" 
        importEnc = maybe "import Json.Encode as Enc exposing (object)" " Enc."
        importList = maybe "import List" " List."
    in
        join "\n" <|
            concat [
              importDict
            , importDec
            , importDecPipe
            , importEnc
            , importList
            , ["\n"]
            ]
            

stringify: List (List String) -> String
stringify xs =
    join "\n\n" <| map (join "\n") xs




