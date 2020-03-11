# Simplex-noise
This is a library to generate simplex noise in Elm.


This is an Elm 0.19 port of eskimoblood/elm-simplex-noise, which itself was a port of [simplex-noise.js](https://github.com/jwagner/simplex-noise.js) by Jonas Wagner.


```
import Simplex exposing (PermutationTable)

permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42


```