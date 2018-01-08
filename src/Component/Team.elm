port module Component.Team exposing ( Model, Msg(..), init, update, view
                                    , initialize)

import Set exposing (Set, size, insert, empty)
import String

import Material
import Material.Button    as Button
import Material.Card      as Card
import Material.Chip      as Chip
import Material.Color     as Color
import Material.Elevation as Elevation
import Material.Grid      as Grid
import Material.Icon      as Icon
import Material.List      as MList
import Material.Options   as Options exposing (css)
import Material.Textfield as Textfield

import Html exposing (Html, span, div)
import Svg.Attributes exposing (style)
import Json.Decode as Decode

import String.Utils exposing (initials)

type alias Team = Set String

type alias Model = { name        : String
                   , currentTeam : Set String
                   , savedTeams  : List Team
                   , mdl         : Material.Model }

type alias HasTeam m = {m | team : Model}

init : Model
init = { name = ""
       , currentTeam = Set.empty
       , savedTeams = []
       , mdl = Material.model }

initialize : Maybe (List (List String)) -> Model
initialize teams = {init | savedTeams = List.map Set.fromList
                                     <| Maybe.withDefault [] teams}

type Msg = Add String
         | Clear
         | Delete String
         | RemoveTeam (Set String)
         | SaveTeam
         | SetTeam (Set String)
         | Name String
         | KeyUp Int
         | None
         | Mdl (Material.Msg Msg)

update : (Msg -> msg) -> Msg -> HasTeam m -> (HasTeam m, Cmd msg)
update lift msg model =
  let (team, cmd) = update_ msg model.team
  in ({model | team = team}, Cmd.map lift cmd)

update_ : Msg -> Model -> (Model, Cmd Msg)
update_ action model =
  case action of
    Add members ->
           let newMembers = Set.fromList <| List.filter (not << String.isEmpty)
                                         <| List.map String.trim
                                         <| String.split ";" members
            in ({model | currentTeam = Set.union model.currentTeam newMembers
                       , name = "" }
               , Cmd.none)
    Delete member -> ( {model | currentTeam = Set.remove member model.currentTeam}
                     , Cmd.none)
    RemoveTeam xs -> let newTeams = List.filter ((/=) xs) model.savedTeams
                      in ( {model | savedTeams = newTeams}
                         , saveTeams <| List.map Set.toList newTeams )
    SaveTeam -> let newTeams = model.currentTeam :: model.savedTeams
                 in ( {model | savedTeams  = newTeams
                             , currentTeam = Set.empty}
                    , saveTeams <| List.map Set.toList newTeams )
    SetTeam xs -> ( {model | currentTeam = xs       }, Cmd.none)
    Clear      -> ( {model | currentTeam = Set.empty}, Cmd.none)
    Name name  -> ( {model | name        = name     }, Cmd.none)
    KeyUp keycode -> if keycode == 13 {- ENTER -}
                       then update_ (Add model.name) model
                       else update_ None model
    None -> (model, Cmd.none)
    Mdl msg -> Material.update Mdl msg model


memberChip : Bool -> String -> Html Msg
memberChip running str =
  Chip.span
    (if running
       then [ Chip.deleteIcon "lens"]
       else [ Chip.deleteClick (Delete str)
            , Chip.deleteIcon "cancel"
            ])
    [ Chip.contact Html.span
        [ Color.background Color.primary
        , Color.text Color.white
        , Options.onClick None
        ]
        [ Html.text <| initials str ]
    , Chip.content []
        [ Html.text str ]
    ]

teamCard : Model -> Set String -> Grid.Cell Msg
teamCard model xs =
  Grid.cell [ Grid.size Grid.Desktop 6 ]
   [ Card.view
      [ css "width" "100%"
      , css "text-align" "left"
      , Elevation.e8
      , Color.background (Color.color Color.Pink Color.S300)
      , Options.onClick (SetTeam xs) ]
      [ Card.title [ ]
          [ Card.head [ Color.text Color.white ]
                      [ Html.text "Team" ]
          , Card.subhead [ Color.text Color.white ]
                         [ Html.text "click to load team" ]
          ]
      , Card.menu []
           [ Button.render Mdl [0,0] model.mdl
               [ Button.icon
               , Button.ripple
               , Color.text Color.white
               , Options.onClick (RemoveTeam xs)]
               [ Icon.i "close" ]
           ]
      , Card.text []
                  [ MList.ul [] (List.map (\x -> MList.li [] [Html.text x]) <| Set.toList xs) ]
      ]
   ]

view : Bool -> Model -> Html Msg
view isRunning model = Html.div []
            [ Html.p [] [Html.text "Start by adding new members to your scrum team by entering some names (separated by semicolons)."]
            , Button.render Mdl [0] model.mdl
                ([ Button.fab
                 , Button.colored
                 , Options.onClick (Add model.name)
                 ] ++ if isRunning || String.isEmpty model.name
                                   || Set.member model.name model.currentTeam
                        then [Button.disabled]
                        else []
                )
                [ Icon.i "add"]
            , Html.span [style "margin-left: 1em"]
                 [Textfield.render Mdl [2] model.mdl
                ([ Textfield.label "new member"
                 , Textfield.floatingLabel
                 , Textfield.value model.name
                 , Options.onInput Name
                 , Options.on "keyup" (Decode.map KeyUp <| Decode.at ["keyCode"] Decode.int)
                 ] ++ if isRunning then [Textfield.disabled] else [])
                []]
            , div [style "text-align:left;"] <| List.map (memberChip isRunning) <| Set.toList model.currentTeam
            , div [style "padding-top: 1em"]
               [ Button.render Mdl [0] model.mdl
                  ([ Button.raised
                   , Button.colored
                   , Options.onClick Clear
                   ] ++ if isRunning || Set.isEmpty model.currentTeam
                          then [Button.disabled] else [])
                  [ Html.text "Clear" ]
               ]
            , div [style "padding-top: 1em"]
               [ Button.render Mdl [0] model.mdl
                  ([ Button.raised
                   , Button.colored
                   , Options.onClick SaveTeam
                   ] ++ if isRunning || Set.isEmpty model.currentTeam
                                     || List.member model.currentTeam model.savedTeams
                          then [Button.disabled] else [])
                  [ Html.text "Save new Team" ]
               ]
            , div [style "padding-top: 1em"]
               [ Grid.grid [] <| List.map (teamCard model) model.savedTeams ]
            ]

-- PORTS

port saveTeams : List (List String) -> Cmd msg
