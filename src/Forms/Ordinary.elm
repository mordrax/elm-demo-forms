module Forms.Ordinary exposing (..)

import Html exposing (Html)
import Task exposing (Task)


type alias Ordinary =
    {}


type Msg
    = Msg


init : Ordinary
init =
    {}


update : Msg -> Ordinary -> ( Ordinary, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Ordinary -> Html Msg
view model =
    Html.text ""


save : Ordinary -> Result (List String) (Task () (Cmd Msg))
save model =
    case validate model of
        [] ->
            Result.Ok (Task.succeed Cmd.none)

        errs ->
            Result.Err errs


validate : Ordinary -> List String
validate model =
    []
