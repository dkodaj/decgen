port module Cli exposing (run)
import Platform
import Types
import Generate

type alias Flags = SourceCode
type alias SourceCode = String

type alias Model = {}
type Msg = NoOp

main : Platform.Program Flags Model Msg
main = Platform.worker {
    init = run
  , update = always (always ({}, Cmd.none))
  , subscriptions = always Sub.none
  }

run : SourceCode -> (Model, Cmd Msg)
run sourceCode =
  let
    coders = Generate.both Types.Extra sourceCode
  in
    ({}, done coders)

port done : String -> Cmd msg
