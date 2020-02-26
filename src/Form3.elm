module Form2 exposing (..)

import Html exposing (Html)


type alias Form2 =
    {}


type Msg
    = Msg


init : ( Form2, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Form2 -> ( Form2, Cmd msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Form2 -> Html Msg
view model =
    Html.text ""
