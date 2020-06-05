port module Main exposing (main)

import Browser
import Generate
import Html exposing (Html, node, text)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Dec
import Types exposing (ExtraPackage(..))


type alias Model =
    { input : String
    , output : String
    , showImport : Bool
    , extra : ExtraPackage
    }


type Msg
    = Both
    | Copy
    | Decode
    | Encode
    | InputChange String
    | ToggleExtraPackage ExtraPackage
    | ToggleShowImport


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , output = ""
      , showImport = False
      , extra = Pipeline
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Both ->
            ( { model | output = Generate.both model.extra model.input }, Cmd.none )

        Copy ->
            ( model, copyText (output model) )

        Decode ->
            ( { model | output = Generate.decoders model.extra model.input }, Cmd.none )

        Encode ->
            ( { model | output = Generate.encoders model.input }, Cmd.none )

        InputChange txt ->
            ( { model | input = txt }, Cmd.none )

        ToggleExtraPackage package ->
            ( { model | extra = package }, Cmd.none )

        ToggleShowImport ->
            ( { model | showImport = not model.showImport }, Cmd.none )


view : Model -> Html Msg
view model =
    node "div"
        []
        [ banner
        , break
        , columns
            [ column1 "is-5 marg" <|
                field
                    [ text "Input" ]
                    [ textArea
                        "Copy/paste or type code"
                        "codeInput is-normal"
                        [ Events.onInput InputChange ]
                        17
                        model.input
                    ]
            , column "has-text-centered" (controlPanel model)
            , column1 "is-5 marg" <|
                field
                    [ level "is-mobile"
                        [ text "Output" ]
                        []
                        [ button "is-small is-warning" "Copy" [ Events.onClick Copy ] ]
                    ]
                    [ textArea
                        ""
                        "codeInput is-normal"
                        [ Attr.readonly True ]
                        17
                        (output model)
                    ]
            ]
        , div1 "has-text-centered" <|
            Html.a
                [ Attr.href "https://github.com/dkodaj/decgen" ]
                [ text "browse"
                , fa "" "fab fa-github"
                , text "source"
                ]
        ]



--== View helpers ==--


banner =
    node "section"
        [ Attr.class "hero is-warning myHero" ]
        [ div1
            "hero-body"
            (node "h1"
                [ Attr.class "title" ]
                [ text "Elm decoder generator" ]
            )
        ]


break =
    Html.br [] []


button clss txt attrs =
    node "label"
        (Attr.class ("button " ++ clss) :: attrs)
        [ text txt
        ]


buttonWithIcon clss txt faClss faId attrs =
    node "label"
        (Attr.class ("button " ++ clss) :: attrs)
        [ text (txt ++ nbsp ++ nbsp ++ nbsp)
        , fa faClss faId -- Font Awesome icon
        ]


buttons =
    [ buttonWithIcon "is-success is-outlined myButton" "Decode" "" "fas fa-forward" [ Events.onClick Decode ]
    , buttonWithIcon "is-warning is-outlined myButton" "Encode" "" "fas fa-forward" [ Events.onClick Encode ]
    , buttonWithIcon "is-danger is-outlined myButton" (nbsp ++ nbsp ++ "Both") "" "fas fa-forward" [ Events.onClick Both ]
    ]


checkbox txt check attr =
    node "label"
        [ Attr.class "checkbox" ]
        [ node "input"
            [ attr
            , Attr.type_ "checkbox"
            , Attr.checked check
            ]
            []
        , text txt
        ]


column clss children =
    div ("column " ++ clss) children


column1 clss child =
    div ("column " ++ clss) [ child ]


columns children =
    div "columns" children


controlPanel model =
    [ break, break, break ]
        ++ buttons
        ++ [ break
           , break
           , break
           , break
           , checkbox " Show imports" model.showImport (Events.onClick ToggleShowImport)
           , break
           , break
           , div1 "" (text "For large records, use")
           , select
                "is-success is-small"
                "mono"
                [ "Json.Decode.Pipeline", "Json.Extra" ]
                decodeSelect
           ]


decodeSelect =
    let
        recover x =
            case x of
                "Json.Decode.Pipeline" ->
                    Dec.succeed Pipeline

                "Json.Extra" ->
                    Dec.succeed Extra

                other ->
                    Dec.fail <| "Unknown constructor for type ExtraPackage: " ++ other
    in
    Dec.map ToggleExtraPackage
        (Dec.at [ "target", "value" ] Dec.string |> Dec.andThen recover)


div clss children =
    node "div" [ Attr.class clss ] children


div1 clss child =
    node "div" [ Attr.class clss ] [ child ]


fa clss id = --Font Awesome icon
    Html.span
        [ Attr.class ("icon " ++ clss) ]
        [ node "i"
            [ Attr.class id ]
            []
        ]


field lbl cntrl =
    div
        "field"
        [ node "label"
            [ Attr.class "label" ]
            lbl
        , div "control" cntrl
        ]


level clss left center right =
    let
        mapper x =
            div1 "level-item" x
    in
    node "nav"
        [ Attr.class <| "level " ++ clss ]
        ([ div "level-left" <| List.map mapper left
         , div "level-right" <| List.map mapper right
         ]
            ++ List.map mapper center
        )


nbsp = --non-breaking space
    String.fromChar <| Char.fromCode 160 --> decimal unicode


output model =
    case model.showImport of
        True ->
            Generate.imports model.output ++ model.output

        False ->
            model.output


select clss1 clss2 options decMsg =
    let
        option x =
            node "option"
                [ Attr.class clss2 ]
                [ text x ]
    in
    div
        ("select center " ++ clss1)
        [ node "select"
            [ Attr.class clss2
            , Events.on "change" decMsg
            ]
            (List.map option options)
        ]


textArea plc clss attrs numRows val =
    node "textarea"
        (attrs
            ++ [ Attr.value val
               , Attr.class <| "textarea " ++ clss
               , Attr.placeholder plc
               , Attr.rows numRows
               ]
        )
        []



--== Ports ==--
--from Elm to JavaScript

port copyText : String -> Cmd msg
