#!/usr/bin/env node

const Module = require('../src/Cli.js')
const fs = require('fs')
const process = require('process')

filename = process.argv[2]

const fileContents = fs.readFileSync(filename).toString()

/*const someCode = `
module X exposing (T, X)
type alias T = {a : String, B : Union}
type Union = X String | Y
`
*/
run(fileContents)

function run (code) {
  const flags = code
  const generate = Module.Elm.Cli.init
  const process = generate({flags})
  process.ports.done.subscribe(x => console.log(x))
}
