module Main exposing (main)

import Browser
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Image exposing (Image)
import Image.Color
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Simplex exposing (PermutationTable)


type alias Model =
    { resolution : Int
    , scale : Float
    , steps : Int
    , stepSize : Float
    , persistence : Float
    , threshold : Maybe Float
    , smooth : Bool
    }


initialModel : Model
initialModel =
    { resolution = 300
    , scale = 1
    , steps = 4
    , stepSize = 2
    , persistence = 2
    , threshold = Nothing
    , smooth = True
    }


type Msg
    = SetResolution Int
    | SetScale Float
    | SetSteps Int
    | SetStepSize Float
    | SetPersistence Float
    | ToggleThreshold
    | SetThreshold Float
    | ToggleSmooth
    | Generate


update : Msg -> ( Model, Model ) -> ( Model, Model )
update msg ( model, model2 ) =
    case msg of
        SetResolution int ->
            ( { model | resolution = int }, model2 )

        SetScale float ->
            ( { model | scale = float }, model2 )

        SetSteps int ->
            ( { model | steps = int }, model2 )

        SetStepSize float ->
            ( { model | stepSize = float }, model2 )

        SetPersistence float ->
            ( { model | persistence = float }, model2 )

        ToggleThreshold ->
            ( { model
                | threshold =
                    if model.threshold == Nothing then
                        Just 0

                    else
                        Nothing
              }
            , model2
            )

        SetThreshold int ->
            ( { model | threshold = Just int }, model2 )

        ToggleSmooth ->
            ( { model | smooth = not model.smooth }, model2 )

        Generate ->
            ( model, model )


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42


image : Model -> Html msg
image { resolution, scale, steps, stepSize, persistence, threshold } =
    let
        config =
            { steps = steps, stepSize = stepSize, persistence = persistence, scale = scale }

        url =
            List.range 0 resolution
                |> List.map
                    (\x ->
                        List.range 0 resolution
                            |> List.map
                                (\y ->
                                    let
                                        val =
                                            fractal2d config permTable (toFloat x) (toFloat y)

                                        val_ =
                                            case threshold of
                                                Just t ->
                                                    if val > t then
                                                        1

                                                    else
                                                        0

                                                Nothing ->
                                                    (val + 1) / 2 |> clamp 0 1
                                    in
                                    Color.rgb val_ val_ val_
                                )
                    )
                |> Image.Color.fromList2d
                |> Image.toPngUrl
    in
    div [ class "image", style "background-image" ("url(" ++ url ++ ")") ] []


view : ( Model, Model ) -> Html Msg
view ( model, model2 ) =
    div [ class "main", classList [ ( "pixelated", not model.smooth ) ] ]
        [ div [ class "header" ]
            [ h1 []
                [ text "Noise preview for "
                , a [ href "https://package.elm-lang.org/packages/Herteby/simplex-noise/latest/" ] [ text "Herteby/simplex-noise" ]
                ]
            ]
        , lazy image model2
        , Html.form [ onSubmit Generate ]
            [ fieldset []
                [ legend [] [ text "Noise settings" ]
                , floatField "Scale" model.scale SetScale "This scales all the noise, making it smoother/blurrier"
                , intField "Steps" model.steps SetSteps "The number of noise layers to combine. The more layers, the larger the features will be. Increases processing time."
                , floatField "Step size" model.stepSize SetStepSize "A value of 2 means that each noise layer is twice as large as the previous one."
                , floatField "Persistence" model.persistence SetPersistence "A higher persistence means that the larger noise layers are weighed more heavily, while a lower one weighs the smaller layers more."
                ]
            , fieldset []
                [ legend [] [ text "Display settings" ]
                , intField "Resolution" model.resolution SetResolution "Width/height resolution of the generated image"
                , label [ title "Toggle between greyscale and a hard black/white threshold" ]
                    [ div [] [ text "Threshold" ]
                    , input
                        [ type_ "checkbox"
                        , checked (model.threshold /= Nothing)
                        , onClick ToggleThreshold
                        ]
                        []
                    ]
                , case model.threshold of
                    Just t ->
                        floatField "-1 to 1" t SetThreshold "The noise is generated in a range between -1 and 1."

                    Nothing ->
                        text ""
                , label [ title "Toggle the browser's image scaling mode. Turning off smooth scaling is not supported by all browsers." ]
                    [ div [] [ text "Smooth scaling" ]
                    , input
                        [ type_ "checkbox"
                        , checked model.smooth
                        , onClick ToggleSmooth
                        ]
                        []
                    ]
                ]
            , div [ class "hint" ] [ text "Hover over the settings to see helpful tooltips" ]
            , button [] [ text "Generate" ]
            ]
        , node "style" [] [ text css ]
        ]


intField : String -> Int -> (Int -> msg) -> String -> Html msg
intField name val msg tooltip =
    label [ title tooltip ]
        [ div [] [ text name ]
        , input
            [ type_ "number"
            , value (String.fromInt val)
            , on "input" <| Decode.at [ "target", "value" ] <| Decode.map msg Decode.parseInt
            ]
            []
        ]


floatField : String -> Float -> (Float -> msg) -> String -> Html msg
floatField name val msg tooltip =
    label [ title tooltip ]
        [ div [] [ text name ]
        , input
            [ type_ "number"
            , step "0.1"
            , value (String.fromFloat val)
            , on "input" <| Decode.at [ "target", "value" ] <| Decode.map msg Decode.parseFloat
            ]
            []
        ]


main : Program () ( Model, Model ) Msg
main =
    Browser.sandbox
        { init = ( initialModel, initialModel )
        , view = view
        , update = update
        }


fractal2d : { steps : Int, stepSize : Float, persistence : Float, scale : Float } -> PermutationTable -> Float -> Float -> Float
fractal2d { steps, stepSize, persistence, scale } table x y =
    List.range 0 (steps - 1)
        |> List.map toFloat
        |> List.foldl
            (\step ( noise, max ) ->
                let
                    freq =
                        (stepSize ^ step) * scale

                    amp =
                        persistence ^ step
                in
                ( noise + (amp * Simplex.noise2d table (x / freq) (y / freq))
                , max + amp
                )
            )
            ( 0, 0 )
        |> (\( noise, max ) -> noise / max)


css =
    """
html {
  height:100%;
}
body {
  height:100%;
  font-family:sans-serif;
  font-size:14px;
  display:flex;
  margin:0;
}
h1,h2,h3,h4,h5,h6,h7{
  font-weight:normal;
}
.header {
  background:#333;
  color:white;
  padding: 1px;
  text-align:center;
}
.header a {
  color:white;
}
.main {
  display:flex;
  flex-direction:column;
  flex-grow:1;
}
.image {
  background-size:cover;
  background-position:center;
  flex-grow:1;
}
.pixelated .image {
  image-rendering:pixelated;
}
form {
  background:#eee;
  display:inline-block;
  padding:10px;
  border-radius:5px;
  position:absolute;
  top:100px;
  left:10px;
}
fieldset {
  margin-bottom:10px;
}
label {
  display:flex;
  align-items:center;
}
label:not(:first-of-type){
  margin-top:10px;
}
label div {
  width:130px;
}
input[type="number"]{
  width:50px;
}
button {
  font-size:16px;
  padding:5px;
  width:100%;
}
.hint {
  width:1px;
  min-width:100%;
  font-size:12px;
  margin-bottom:10px;
}
"""
