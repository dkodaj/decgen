# Elm decoder generator

Builds JSON decoders and encoders for your Elm types. You must pass in the source code as text.

```elm
import DecGen

sampleCode = "type alias MyType = {x: String, y: Int}"

DecGen.decoders sampleCode == 
	"""
	decodeMyType =
	   decode
	      MyType
	         |> required "X" Dec.string
	         |> required "Y" Dec.int
	"""

DecGen.encoders sampleCode == 
	"""
	encodeMyType a =
	   object
	      [ ("X", Enc.string a.x)
	      , ("Y", Enc.int a.y)
	      ]
	"""
```

## Use it from the browser

Copy/paste your code into the [live version](https://dkodaj.github.io/decgen).