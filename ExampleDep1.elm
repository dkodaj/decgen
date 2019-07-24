module ExampleDep1 exposing (Union)

import ExampleDep2

type Union = A ExampleDep2.AnotherRecord | B String
