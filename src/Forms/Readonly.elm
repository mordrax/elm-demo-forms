module Forms.Readonly exposing (..)

import Html exposing (Html)


type alias Readonly =
    {}


type Msg
    = Msg


init : Readonly
init =
    {}


update : Msg -> Readonly -> ( Readonly, Cmd msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Readonly -> Html Msg
view model =
    Html.text ""
