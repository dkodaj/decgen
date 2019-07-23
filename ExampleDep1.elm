module ExampleDep1 exposing (Union)

import ExampleDep2 exposing (..)

type Union = A ExampleDep2.AnotherRecord | B String
