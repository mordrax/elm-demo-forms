module Forms.Headless exposing (..)

import Html exposing (Html)
import Task exposing (Task)


type alias Headless =
    {}


type Msg
    = Msg


init : Headless
init =
    {}


update : Msg -> Headless -> ( Headless, Cmd msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


save : Headless -> Result (List String) (Task () String)
save model =
    case validate model of
        [] ->
            Result.Ok (Task.succeed "Data")

        errs ->
            Result.Err errs


validate : Headless -> List String
validate model =
    []
