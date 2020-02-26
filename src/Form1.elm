module Form1 exposing (..)

import Html exposing (Html)


type alias Form1 =
    {}


type Msg
    = Msg


init : Form1
init =
    {}


update : Msg -> Form1 -> ( Form1, Cmd msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )


view : Form1 -> Html Msg
view model =
    Html.text ""


save : Form1 -> ( Form1, Cmd Msg )
save model =
    ( model, Cmd.none )


validate : Form1 -> List String
validate model =
    []
