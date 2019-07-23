port module Cli exposing (run)
import Platform
import Types
import Generate

type alias Flags = SourceCodes

type alias SourceCodes = List String

type alias Model = {}

type Msg = NoOp

main : Platform.Program Flags Model Msg
main = Platform.worker {
    init = run
  , update = always (always ({}, Cmd.none))
  , subscriptions = always Sub.none
  }

run : SourceCodes -> (Model, Cmd Msg)
run sourceCodes =
  let
    coders = Generate.bothWithImports Types.Extra sourceCodes
  in
    ({}, done coders)

port done : String -> Cmd msg
