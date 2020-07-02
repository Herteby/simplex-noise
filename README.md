# Simplex-noise
This is a library to generate [simplex noise](https://en.wikipedia.org/wiki/Simplex_noise) in Elm.

[Interactive demo](https://herteby.github.io/simplex-noise/preview/)

![simplex](https://herteby.github.io/simplex-noise/simplexExample.png)

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
    List.range 0 99
        |> List.map
            (\x ->
                List.range 0 99
                    |> List.map
                        (\y ->
                            noise (toFloat x) (toFloat y)
                        )
            )


```

Original version written by [Andreas Köberle](https://github.com/eskimoblood), who ported the [simplex-noise.js](https://github.com/jwagner/simplex-noise.js) library written by by Jonas Wagner.