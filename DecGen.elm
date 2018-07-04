module DecGen exposing (..)

import DecGen.Decoder exposing (decoder, decoderCapitalize)
import DecGen.Destructuring exposing (bracket, stringify)
import DecGen.Encoder exposing (encoder, encoderCapitalize)
import DecGen.TypeExtract exposing (extractAll)
import List exposing (concat,filter, map, sortBy)
import String exposing (contains, join, trim)

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


--== Record field names capitalized ==--

bothCapitalize: String -> String
bothCapitalize txt =
    (decodersCapitalize txt) ++ "\n\n" ++ (encodersCapitalize txt)

decodersCapitalize: String -> String
decodersCapitalize txt =
    let
        allTypes = extractAll False txt
    in
        stringify <|
            (map decoderCapitalize <| sortBy .name allTypes )

encodersCapitalize: String -> String
encodersCapitalize txt =
    let
        allTypes = extractAll True txt
    in
        stringify <|
            (map encoderCapitalize <| sortBy .name allTypes )


--== Decoder/encoder imports ==--

imports output =
    let
        inOutput a = contains a output
        exposeTxt xs =
            case xs of
                []->
                    ""
                _->
                    " exposing " ++ (bracket <| join ", " <| map trim xs)
        maybe modName maybeNick funcs =
            let
                expose = exposeTxt <| filter inOutput funcs
                modRef = 
                    case maybeNick of
                        Nothing->
                            modName
                        Just nick->
                            nick
                relevant = inOutput (modRef++".") || expose /= ""
            in
                case relevant of
                    True->
                        case maybeNick of
                            Nothing->
                                [ "import " ++ modName ++ expose ]
                            Just nick->
                                [ "import " ++ modName ++ " as " ++ nick ++ expose]
                    False->
                        []
        importDict = maybe "Dict" Nothing []
        importDec = maybe "Json.Decode" (Just "Dec") [" andThen "]
        importDecPipe = maybe "Json.Decode.Pipeline" Nothing [" decode\n", " required "] 
        importEnc = maybe "Json.Encode" (Just "Enc") [" object\n"]
        importList = maybe "List" Nothing []
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
            





