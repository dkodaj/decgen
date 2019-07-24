module ExampleDep1 exposing (Union)

import ExampleDep2 as Dep2

type Union = A Dep2.AnotherRecord | B String
