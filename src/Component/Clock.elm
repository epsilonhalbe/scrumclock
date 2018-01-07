module Component.Clock exposing (..)

import Material
import Material.Button as Button exposing (..)
import Material.Card as Card
import Material.Chip as Chip exposing (..)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Helpers exposing (map1st, map2nd)
import Material.Icon as Icon
import Material.List as MList
import Material.Options as Options exposing (Style, css)
import Material.Slider as Slider
import Material.Snackbar as Snackbar
import Material.Textfield as Textfield

import Time exposing (Time, every, second, inSeconds)
import String.Utils exposing (initials)
import String exposing (join)
import List exposing (map)
import Html exposing (Html, span, div, programWithFlags)
import Svg exposing (svg, text, circle, line, path)
import Svg.Attributes exposing ( style, writingMode, textAnchor, fill, x, y
                               , fillOpacity, strokeWidth, stroke, d )

type alias Model = { sec         : Int
                   , maxMinutes  : Float
                   , running     : Bool
                   , mdl         : Material.Model
                   }

type alias HasClock m = {m | clock : Model}

init : Model
init = { sec = 0
       , maxMinutes = 15
       , running = False
       , mdl = Material.model }


type Msg = SetMinutes Float
         | Start
         | Stop
         | Tick Int
         | Mdl (Material.Msg Msg)

-- UPDATE

update : (Msg -> msg) -> Msg -> HasClock m -> (HasClock m, Cmd msg)
update lift msg model =
  let (clock, cmd) = update_ msg model.clock
  in ({model | clock = clock}, Cmd.map lift cmd)

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ action model =
  case action of
    SetMinutes maxMinutes -> ({model | maxMinutes = maxMinutes }, Cmd.none)
    Start -> ({model | running = True }, Cmd.none)
    Stop  -> ({model | running = False, sec = 0 }, Cmd.none)
    Tick sec -> ({model | sec = sec}, Cmd.none)
    Mdl msg -> Material.update Mdl msg model


clock : Float -> Int -> Int -> String ->  List (Svg.Svg msg)
clock sec_ n k str =
    let maxR = 250
        minR = 50
        area = (maxR^2 - minR^2) * pi
        n_ = toFloat (n+1)
        k_ = toFloat (k-1)
        inneR =sqrt <| ((n_-k_-1)*area)/(n_*pi) - minR^2
        outeR =sqrt <| ((n_-k_)*area)/(n_*pi) - minR^2
    in [ Svg.path [ d <| arc inneR outeR (clamp k_ (k_+0.999) sec_) (k_+0.999)
                  , stroke "#FF4081"
                  , strokeWidth "2"
                  , fill "#3F51B5"
                  , fillOpacity <| toString <| 1.00 - k_/(3*n_)
                  ] []
       , Svg.text_ [ x <|toString <| (-(outeR + inneR))/2
                   , y "0", fill "#FFFFFF"
                   , textAnchor "middle"
                   , writingMode "vertical-rl"
                   , style "text-orientation: upright"
                   ] [Svg.text <| initials str]
       ]

arc : Float -> Float -> Float -> Float -> String
arc inneR outeR fromDeg toDeg =
  let xCoord r phi = r * cos (turns phi)
      yCoord r phi = negate (r * sin (turns phi))
      startInner = String.join " " <| map toString [xCoord inneR fromDeg, yCoord inneR fromDeg]
      startOuter = String.join " " <| map toString [xCoord outeR fromDeg, yCoord outeR fromDeg]
      endInner   = String.join " " <| map toString [xCoord inneR toDeg  , yCoord inneR toDeg]
      endOuter   = String.join " " <| map toString [xCoord outeR toDeg  , yCoord outeR toDeg]
      largeArcFlag = if abs (toDeg - fromDeg) <= 0.5 then "0" else "1"
  in String.join " " <| List.map (String.join " ")
                    [["M", startOuter]
                    ,["A", toString outeR, toString outeR, "0", largeArcFlag, "0", endOuter ]
                    ,["L", endInner]
                    ,["A", toString inneR, toString inneR, "0", largeArcFlag, "1", startInner ]
                    ,["L", startOuter, "Z"]
                    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = every second <| \t ->
    if model.running && model.sec < round (60 * model.maxMinutes)
      then Tick (model.sec + 1)
      else Stop
