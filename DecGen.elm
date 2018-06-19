module DecGen exposing (both, decoders, encoders, imports)

import DecGen.Decoder exposing (decoder)
import DecGen.Encoder exposing (encoder)
import DecGen.TypeExtract exposing (extractAll)
import List exposing (concat, map, sortBy)
import String exposing (contains, join)

both: String -> String
both txt =
    let
        (allTypes, anonymousDefs) = extractAll txt
        sortedTypes = sortBy .name allTypes
    in
        stringify <|
            (anonymousDefs)
            ++
            (map decoder <| sortedTypes )
            ++
            (map encoder <| sortedTypes )

decoders: String -> String
decoders txt =
    let
        (allTypes, anonymousDefs) = extractAll txt
    in
        stringify <|
            (anonymousDefs)
            ++
            (map decoder <| sortBy .name allTypes )

encoders: String -> String
encoders txt =
    let
        (allTypes, anonymousDefs) = extractAll txt
    in
        stringify <|
            (anonymousDefs)
            ++
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




