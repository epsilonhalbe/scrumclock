module Main exposing (main)

import Material
import Material.Grid as Grid
import Material.Snackbar as Snackbar

import Html exposing (Html, div, programWithFlags)
import Svg.Attributes exposing (style)

import Component.Clock as Clock
import Component.Team as Team

main = programWithFlags
     { init          = initialize
     , view          = view
     , update        = update
     , subscriptions = subscriptions
     }

-- MODEL

type alias Model = { clock       : Clock.Model
                   , team        : Team.Model
                   , mdl         : Material.Model
                   , cacheError  : Snackbar.Model ()
                   }

initialize : Maybe (List (List String)) -> (Model, Cmd Msg)
initialize teams =
    ({init | team = Team.initialize teams}
    , Cmd.none)

init : Model
init = { clock = Clock.init
       , team = Team.init
       , cacheError = Snackbar.model
       , mdl = Material.model }

-- UPDATE

type Msg = Team Team.Msg
         | Clock Clock.Msg
         | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Team  act -> Team.update  Team  act model
    Clock act -> Clock.update Clock act model
    Mdl   act -> Material.update Mdl act model

-- VIEW

view : Model -> Html Msg
view model = div [style "margin:auto;width:90%;text-align:center;"]
  [ Html.h1 [] [Html.text <| if model.clock.running
                                then Clock.timeFormat (round (model.clock.maxMinutes * 60) - model.clock.sec)
                                else "Scrum clock"]
  , Grid.grid [Grid.align Grid.Top]
     [ Grid.cell [ Grid.size Grid.All  12
                 , Grid.size Grid.Desktop  6
                 , Grid.offset Grid.Desktop  3
                 , Grid.order Grid.Desktop 1
                 , Grid.stretch
                 ]
        [ Html.map Clock <| Clock.slider model.team model.clock ]
     , Grid.cell [ Grid.size Grid.All  12
                 , Grid.size Grid.Desktop  6
                 , Grid.order Grid.Desktop 3
                 ]
        [ Html.map Clock <| Clock.clock model.team model.clock ]
     , Grid.cell [ Grid.size Grid.All  12
                 , Grid.size Grid.Desktop  6
                 , Grid.order Grid.Desktop 2
                 ]
        [ Html.map Team <| Team.view model.clock.running model.team]
     ]
  ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map Clock <| Clock.subscriptions model.clock
