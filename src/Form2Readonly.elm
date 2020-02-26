module Form2Readonly exposing (..)

import Html exposing (Html)


type alias Form2Readonly =
    {}


type Msg
    = Msg


init : ( Form2Readonly, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Form2Readonly -> ( Form2Readonly, Cmd msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Form2Readonly -> Html Msg
view model =
    Html.text ""
