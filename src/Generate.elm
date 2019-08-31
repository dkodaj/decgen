module Generate exposing (both, bothWithImports, decoders, encoders, imports)

import Decoder exposing (decoder)
import Destructuring exposing (bracket, stringify)
import Encoder exposing (encoder)
import List exposing (concat, filter, map, sortBy)
import ParseModules
import ParseType exposing (extractAll, extractAllWithDefs)
import String exposing (contains, join, trim)
import Types exposing (ExtraPackage)

--== Generate encoders/decoders from a list of .elm files ==--
    -- the head of the list is the base module, the rest are its depencies
    -- types defined in the tail are only decoded/encoded if they are
    -- imported by the head, or by something imported by the head etc.




bothWithImports : ExtraPackage -> List String -> String
bothWithImports extra txt =
    decodersWithImports extra txt ++ "\n\n" ++ encodersWithImports txt


decodersWithImports : ExtraPackage -> List String -> String
decodersWithImports extra sources =
    stringify <|
        ( map (decoder extra)
            <| sortBy .name
                <| ParseModules.parseAll False sources 
        )

encodersWithImports : List String -> String
encodersWithImports sources =
    stringify <|
        ( map encoder
            <| sortBy .name
                <| ParseModules.parseAll True sources 
        )


--== Generate encoders/decoders from a single .elm file or a bunch of type definitions ==--

{-| Generete encoders/decoders for some source code

    import Types exposing (ExtraPackage(..))

    both Pipeline "type alias TunaOrTofu = Tuna | Tofu"
    --> """decodeTunaOrTofu =
    -->         let
    -->            recover x =
    -->               case x of
    -->                  \"Tuna\"->
    -->                     Decode.succeed Tuna
    -->                  \"Tofu\"->
    -->                     Decode.succeed Tofu
    -->                  other->
    -->                     Decode.fail <| \"Unknown constructor for type TunaOrTofu: \" ++ other
    -->         in
    -->            Decode.string |> Decode.andThen recover\n
    -->     encodeTunaOrTofu a =
    -->         case a of
    -->            Tuna ->
    -->               Encode.string \"Tuna\"
    -->            Tofu ->
    -->               Encode.string \"Tofu\""""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting

    both Pipeline "type alias QualifiedExample = SomeModule.SomeType"

    --> """decodeQualifiedExample =
    -->         SomeModule.decodeSomeType\n
    -->    encodeQualifiedExample a =
    -->         SomeModule.encodeSomeType a"""
    -->         |> String.replace "                     " "" -- adjust to formatting
    -->         |> String.replace "                    " "" -- adjust to formatting
    -->         |> String.replace "                   " "" -- adjust to formatting

Note that the last two lines are to make the expected outcome more readable
-}
both : ExtraPackage -> String -> String
both extra txt =
    decoders extra txt ++ "\n\n" ++ encoders txt


decoders : ExtraPackage -> String -> String
decoders extra txt =
    let
        ( allTypes, anonymousDefs ) =
            extractAllWithDefs False txt
    in
    stringify <|
        anonymousDefs
            ++ (map (decoder extra)
                <| sortBy .name allTypes)


encoders : String -> String
encoders txt =
    let
        allTypes =
            extractAll True txt
    in
    stringify <|
        (map encoder <| sortBy .name allTypes)



--== Decoder/encoder imports ==--


imports output =
    let
        inOutput a =
            contains a output

        maybe modName maybeNick =
            let
                modRef =
                    case maybeNick of
                        Nothing ->
                            modName

                        Just nick ->
                            nick

                relevant =
                    inOutput (modRef ++ ".")
            in
            case relevant of
                True ->
                    case maybeNick of
                        Nothing ->
                            [ "import " ++ modName ]

                        Just nick ->
                            [ "import " ++ modName ++ " as " ++ nick ]

                False ->
                    []

        importDict =
            maybe "Dict" Nothing

        importDec =
            maybe "Json.Decode" (Just "Decode")

        importDecExtra =
            maybe "Json.Decode.Extra" (Just "Extra")

        importDecPipe =
            maybe "Json.Decode.Pipeline" (Just "Pipeline")

        importEnc =
            maybe "Json.Encode" (Just "Encode")
    in
    join "\n" <|
        concat
            [ importDict
            , importDec
            , importDecExtra
            , importDecPipe
            , importEnc
            , [ "\n" ]
            ]
