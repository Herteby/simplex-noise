# Simplex-noise
This is a library to generate simplex noise in Elm.


This is an Elm 0.19 port of eskimoblood/elm-simplex-noise, which itself was a port of [simplex-noise.js](https://github.com/jwagner/simplex-noise.js) by Jonas Wagner.

## Example usage

    (perm, newSeed) = permutationTable (initialSeed 42) -- generate the permutation table
    noiseValue = noise3d perm 1 1 1
