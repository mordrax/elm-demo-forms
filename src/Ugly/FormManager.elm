module Ugly.FormManager exposing (..)

import Form1 exposing (Form1)
import Html exposing (Html)
import Html.Events as HE


type Msg
    = FormMsg FormMsg
    | Open OpenForm
    | Save


type alias FormManager =
    { formState : FormState
    }


type OpenForm
    = OpenForm1 String


type FormState
    = NoForm
    | Form1State Form1


type FormMsg
    = Form1Msg Form1.Msg


update : Msg -> FormManager -> ( FormManager, Cmd Msg )
update msg model =
    case msg of
        Open openFormMsg ->
            openForm openFormMsg model

        FormMsg formMsg ->
            updateForm formMsg model

        Save ->
            saveForm model


openForm : OpenForm -> FormManager -> ( FormManager, Cmd Msg )
openForm msg model =
    case msg of
        OpenForm1 _ ->
            ( { model | formState = Form1State Form1.init }, Cmd.none )


mapStateMsg : (a -> FormState) -> (b -> FormMsg) -> FormManager -> ( a, Cmd b ) -> ( FormManager, Cmd Msg )
mapStateMsg toFormState toFormMsg model ( formState, formCmd ) =
    ( { model | formState = toFormState formState }
    , Cmd.map (toFormMsg >> FormMsg) formCmd
    )


updateForm : FormMsg -> FormManager -> ( FormManager, Cmd Msg )
updateForm formMsg model =
    case ( formMsg, model.formState ) of
        ( Form1Msg msg, Form1State state ) ->
            Form1.update msg state
                |> mapStateMsg Form1State Form1Msg model


saveForm : FormManager -> ( FormManager, Cmd Msg )
saveForm model =
    case model.formState of
        NoForm ->
            ( model, Cmd.none )

        Form1State formState ->
            Form1.save formState |> mapStateMsg Form1State Form1Msg model


view : FormManager -> Html Msg
view model =
    case model.formState of
        NoForm ->
            Html.text ""

        Form1State formState ->
            Html.div []
                [ Html.button [ HE.onClick Save ] [ Html.text "Save" ]
                , Form1.view formState |> Html.map (Form1Msg >> FormMsg)
                ]
