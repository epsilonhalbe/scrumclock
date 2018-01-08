module Component.Clock exposing ( Model, Msg(..), update, subscriptions
                                , init, clock, slider, timeFormat)

import Material
import Material.Button  as Button
import Material.Icon    as Icon
import Material.Options as Options
import Material.Slider  as Slider

import Time exposing (Time, every, second, inSeconds)
import Set exposing (Set, size, insert, empty)
import String exposing (join)
import List exposing (map)
import Html exposing (Html, span, div)
import Svg exposing (svg, text, circle, line, path)
import Svg.Attributes exposing ( style, writingMode, textAnchor, fill, x, y
                               , fillOpacity, strokeWidth, stroke, d, width 
                               , viewBox, height)

import String.Utils exposing (initials)

import Component.Team as Team

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

slider : Team.Model -> Model -> Html Msg
slider team model =
    Html.div []
             [ Button.render Mdl [0] model.mdl
                 ([ Button.fab
                  , Button.colored
                  , Options.onClick <| if model.running then Stop else Start
                  ] ++ if Set.isEmpty team.currentTeam
                          then [Button.disabled]
                          else []
                  )
                 [ Icon.i <| if model.running then "stop" else "play_arrow"]
             , Slider.view <|
                 [ Slider.onChange SetMinutes
                 , Slider.value <| model.maxMinutes
                 , Slider.max 30
                 , Slider.min 1
                 , Slider.step 1
                 ] ++ if model.running then [Slider.disabled] else []
             , Html.p [] [Html.text <| timeFormat (round (model.maxMinutes * 60) - model.sec)]
             ]

clock : Team.Model -> Model -> Html Msg
clock team model =
      Html.span []
        [ svg [ style "border:1px white solid"
              , if Set.isEmpty team.currentTeam
                   then height "0"
                   else viewBox "-260 -260 520 520"
              , width "100%"
              ]
              (List.concat (List.map2 (clock_ team model)
                                      (List.range 1 (Set.size team.currentTeam))
                                      (Set.toList team.currentTeam)))
        ]

clock_ : Team.Model -> Model -> Int -> String -> List (Svg.Svg msg)
clock_ team model k str =
    let sec_ = (toFloat (model.sec * (Set.size team.currentTeam))) / (60 * model.maxMinutes)
        n    = Set.size team.currentTeam
        maxR = 250
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

timeFormat : Int -> String
timeFormat secs = let mins_ = String.padLeft 2 '0' <| toString <| secs // 60
                      secs_ = String.padLeft 2 '0' <| toString <| secs % 60
                   in mins_ ++ ":" ++ secs_

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = every second <| \t ->
    if model.running && model.sec < round (60 * model.maxMinutes)
      then Tick (model.sec + 1)
      else Stop
