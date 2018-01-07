module Main exposing (main)

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

import Html exposing (Html, span, div, programWithFlags)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (map)
import Platform.Sub exposing (none)
import Set exposing (Set, size, insert, empty)
import String.Utils exposing (initials)
import String exposing (join)
import Svg exposing (svg, text, circle, line, path)
import Svg.Attributes exposing (..)
import Task exposing (Task)
import Time exposing (Time, every, second, inSeconds)

import Component.Clock as Clock exposing (Msg(..), clock)
import Component.Team as Team exposing (Msg(..), memberChip)

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
         | KeyUp Int
         | Mdl (Material.Msg Msg)
         | None

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Team act  -> let (team_, msg_) = Team.update act model.team
                  in ({ model | team = team_}, Cmd.map Team msg_ )
    Clock act -> let (clock_, msg_) = Clock.update act model.clock
                  in ({ model | clock = clock_}, Cmd.map Clock msg_ )
    KeyUp keycode -> if keycode == 13 {- ENTER -}
                       then update (Team <| Add model.team.name) model
                       else update None model
    Mdl msg -> Material.update Mdl msg model
    None -> (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = let sec_ = (toFloat (model.clock.sec * (Set.size model.team.currentTeam))) / (60 * model.clock.maxMinutes)
              in div [style "margin:auto;width:90%;text-align:center;"]
                [ Html.h1 [] [Html.text <| if model.clock.running
                                              then timeFormat (round (model.clock.maxMinutes * 60) - model.clock.sec)
                                              else "Scrum clock"]
                , Grid.grid []
                   [ Grid.cell [ Grid.size Grid.Tablet 12, Grid.size Grid.Desktop 6, Grid.size Grid.Phone 12
                               , Grid.order Grid.Desktop 1 ]
                     [ Html.span []
                          [svg [ style "border:1px white solid"
                               , viewBox "-260 -260 520 520", width "100%"]
                               (List.concat (List.map2 (Clock.clock sec_ (Set.size model.team.currentTeam)) (List.range 1 (Set.size model.team.currentTeam)) (Set.toList model.team.currentTeam)))
                          ]
                     ]
                   , Grid.cell [ Grid.size Grid.Tablet 12, Grid.size Grid.Desktop 6, Grid.size Grid.Phone 12]
                     [ Button.render Mdl [0] model.mdl
                           ([ Button.fab
                            , Button.colored
                            , Options.onClick <| if model.clock.running then Clock Stop else Clock Start
                            ] ++ if Set.isEmpty model.team.currentTeam then [Button.disabled] else []
                            )
                           [ Icon.i <| if model.clock.running then "stop" else "play_arrow"]
                     , Slider.view <|
                           [ Slider.onChange (Clock << SetMinutes)
                           , Slider.value <| model.clock.maxMinutes
                           , Slider.max 30
                           , Slider.min 1
                           , Slider.step 1
                           ] ++ if model.clock.running then [Slider.disabled] else []
                     , Html.p [] [Html.text <| timeFormat (round (model.clock.maxMinutes * 60) - model.clock.sec)]
                     , Html.p [] [Html.text "Start by adding new members to your scrum team by entering some names (separated by semicolons)."]
                     , Button.render Mdl [0] model.mdl
                         ([ Button.fab
                          , Button.colored
                          , Options.onClick (Team <| Add model.team.name)
                          ] ++ if model.clock.running || String.isEmpty model.team.name
                                                || Set.member model.team.name model.team.currentTeam
                                    then [Button.disabled]
                                    else []
                         )
                         [ Icon.i "add"]
                     , Html.span [style "margin-left: 1em"]
                          [Textfield.render Mdl [2] model.mdl
                         ([ Textfield.label "new member"
                          , Textfield.floatingLabel
                          , Textfield.value model.team.name
                          , Options.onInput (Team << Name)
                          , Options.on "keyup" (Decode.map KeyUp <| Decode.at ["keyCode"] Decode.int)
                          ] ++ if model.clock.running then [Textfield.disabled] else [])
                         []]
                     , div [style "text-align:left;"] <| List.map (Html.map Team << memberChip model.clock.running) <| Set.toList model.team.currentTeam
                     , div [style "padding-top: 1em"]
                        [ Button.render Mdl [0] model.mdl
                           ([ Button.raised
                            , Button.colored
                            , Options.onClick (Team Clear)
                            ]
                                ++ if model.clock.running || Set.isEmpty model.team.currentTeam
                                      then [Button.disabled] else [])
                           [ Html.text "Clear" ]
                        ]
                     , div [style "padding-top: 1em"]
                        [ Button.render Mdl [0] model.mdl
                           ([ Button.raised
                            , Button.colored
                            , Options.onClick (Team SaveTeam)
                            ]
                                ++ if model.clock.running || Set.isEmpty model.team.currentTeam
                                                    || List.member model.team.currentTeam model.team.savedTeams
                                      then [Button.disabled] else [])
                           [ Html.text "Save new Team" ]
                        ]
                     , div [style "padding-top: 1em"]
                        [ Html.map Team <| Grid.grid [] <| List.map (Team.teamCard model.team) model.team.savedTeams
                        ]
                     ]
                   ]
                ]

timeFormat : Int -> String
timeFormat secs = let mins_ = String.padLeft 2 '0' <| toString <| secs // 60
                      secs_ = String.padLeft 2 '0' <| toString <| secs % 60
                   in mins_ ++ ":" ++ secs_

subscriptions : Model -> Sub Msg
subscriptions model = Sub.map Clock <| Clock.subscriptions model.clock
