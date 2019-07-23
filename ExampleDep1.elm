module ExampleDep1 exposing (Union)

import ExampleDep2 exposing (AnotherRecord)

type Union = A AnotherRecord | B String
