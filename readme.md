# Elm decoder generator

Builds JSON decoders and encoders for your Elm types.

## Usage: in the browser

Copy/paste your type definitions into the [live version](https://dkodaj.github.io/decgen).

## Usage: command line

Clone the repo and build Cli.elm:

```
$ git clone https://github.com/dkodaj/decgen
$ cd decgen
$ elm make src/Cli.elm --output src/Cli.js --optimize
```

Generate decoders for Example.elm:

```
$ node src/decgen.js Example.elm
```