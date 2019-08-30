cli:
	elm make src/Cli.elm --output=src/Cli.js
test:
	elm-verify-examples && elm-app test
