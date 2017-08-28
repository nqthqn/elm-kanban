module Main exposing (main)

import Html
import App exposing (update, view, model)


main =
    Html.program
        { init = ( model, Cmd.none )
        , update = updatea
        , view = view
        , subscriptions = always Sub.none
        }
