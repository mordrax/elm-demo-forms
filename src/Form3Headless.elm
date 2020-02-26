module Form3Headless exposing (..)

import Html exposing (Html)
import Task exposing (Task)


type alias Form3Headless =
    {}


type Msg
    = Msg


init : ( Form3Headless, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Form3Headless -> ( Form3Headless, Cmd msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


save : Form3Headless -> Task String ()
save model =
    Task.succeed ()
