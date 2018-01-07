port module Component.Team exposing (..)

import Set exposing (Set, size, insert, empty)
import String

import Material
import Material.Button    as Button exposing (..)
import Material.Card      as Card
import Material.Chip      as Chip exposing (..)
import Material.Color     as Color
import Material.Elevation as Elevation
import Material.Grid      as Grid
import Material.Icon      as Icon
import Material.List      as MList
import Material.Options   as Options exposing (Style, css)
import Material.Slider    as Slider
import Material.Snackbar  as Snackbar
import Material.Textfield as Textfield
import Material.Helpers exposing (map1st, map2nd)

import Html exposing (Html, span, div, programWithFlags)

import String.Utils exposing (initials)



type alias Team = Set String

type alias Model = { name        : String
                   , currentTeam : Set String
                   , savedTeams  : List Team
                   , mdl         : Material.Model }

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
         | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
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
--      , Options.onClick None
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


-- PORTS

port saveTeams : List (List String) -> Cmd msg
