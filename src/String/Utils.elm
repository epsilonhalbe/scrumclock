module String.Utils exposing (initials)

import String exposing (..)
import List

initials : String -> String
initials = toUpper << concat << List.map (left 1) << split " "

