# Simplex-noise
This is a library to generate simplex noise in Elm.

Check out this [interactive demo](https://herteby.github.io/simplex-noise/preview/)!

This is what basic simplex noise looks like:
![simplex](https://herteby.github.io/simplex-noise/simplexExample.png)

Simplex noise can be combined in multiple layers to create fractal noise:
![fractal](https://herteby.github.io/simplex-noise/fractalExample.png)

This is what fractal noise looks like passed through a threshold filter:
![fractal threshold](https://herteby.github.io/simplex-noise/fractalThresholdExample.png)
It looks a bit like coastlines from a satellite photo!

## Example usage

```
import Simplex exposing (PermutationTable)

--Create a permutation table, using 42 as the seed
permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42

-- Create a function for 2D fractal noise
noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 4.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable

-- Create a 100x100 matrix of fractal noise. 
image : List (List Float)
image =
    List.range 0 100
        |> List.map
            (\x ->
                List.range 0 100
                    |> List.map
                        (\y ->
                            noise (toFloat x) (toFloat y)
                        )
            )


```

Original version written by [Andreas Köberle](https://github.com/eskimoblood), who ported the [simplex-noise.js](https://github.com/jwagner/simplex-noise.js) library written by by Jonas Wagner.