module HomogeneousForm exposing (..)

import Form1 exposing (Form1)
import Forms.Headless exposing (Headless)
import Forms.Ordinary exposing (Ordinary)
import Forms.Readonly exposing (Readonly)
import Html exposing (Html)
import Html.Events as HE
import Task exposing (Task)


type Msg
    = FormMsg FormMsg
    | Open OpenForm
    | Save
    | SaveResponse
    | Close


type alias HomogeneousForm =
    { formState : FormState
    , validation : List String
    }


type OpenForm
    = OpenForm1
    | OpenForm2
    | OpenForm3


type FormState
    = NoForm
    | Form1State Ordinary
    | Form2State Readonly
    | Form3State Headless


type FormMsg
    = Form1Msg Forms.Ordinary.Msg
    | Form2Msg Forms.Readonly.Msg
    | Form3Msg Forms.Headless.Msg


update : Msg -> HomogeneousForm -> ( HomogeneousForm, Cmd Msg )
update msg model =
    case msg of
        Open openFormMsg ->
            openForm openFormMsg model

        FormMsg formMsg ->
            updateForm formMsg model

        Save ->
            saveForm model

        Close ->
            ( { model | formState = NoForm }
            , Cmd.none
            )


openForm : OpenForm -> HomogeneousForm -> ( HomogeneousForm, Cmd Msg )
openForm msg model =
    case msg of
        OpenForm1 ->
            ( { model | formState = Form1State Forms.Ordinary.init }, Cmd.none )

        OpenForm2 ->
            ( { model | formState = Form2State Forms.Readonly.init }, Cmd.none )

        OpenForm3 ->
            ( { model | formState = Form3State Forms.Headless.init }, Cmd.none )


mapStateMsg : (a -> FormState) -> (b -> FormMsg) -> HomogeneousForm -> ( a, Cmd b ) -> ( HomogeneousForm, Cmd Msg )
mapStateMsg toFormState toFormMsg model ( formState, formCmd ) =
    ( { model | formState = toFormState formState }
    , Cmd.map (toFormMsg >> FormMsg) formCmd
    )


updateForm : FormMsg -> HomogeneousForm -> ( HomogeneousForm, Cmd Msg )
updateForm formMsg model =
    case ( formMsg, model.formState ) of
        ( Form1Msg msg, Form1State state ) ->
            Forms.Ordinary.update msg state
                |> mapStateMsg Form1State Form1Msg model

        ( Form2Msg msg, Form2State state ) ->
            Forms.Readonly.update msg state
                |> mapStateMsg Form2State Form2Msg model

        ( Form3Msg msg, Form3State state ) ->
            Forms.Headless.update msg state
                |> mapStateMsg Form3State Form3Msg model


saveForm : HomogeneousForm -> Result (List String) (Task () (Cmd Msg))
saveForm model =
    case model.formState of
        NoForm ->
            Result.Err [ "Cannot save the form, no form exists." ]

        Form1State formState ->
            Forms.Ordinary.save formState
                -- shitmozles...
                |> Result.map (Task.map (Cmd.map (Form1Msg >> FormMsg)))


view : HomogeneousForm -> Html Msg
view model =
    let
        formHeader =
            Html.div []
                [ Html.button [ HE.onClick Save ] [ Html.text "Save" ]
                , Html.button [ HE.onClick Close ] [ Html.text "Close" ]
                ]
    in
    case model.formState of
        NoForm ->
            Html.text "Page Data"

        Form1State formState ->
            Html.div []
                [ formHeader
                , Forms.Ordinary.view formState |> Html.map (Form1Msg >> FormMsg)
                ]

        Form2State formState ->
            Html.div []
                [ formHeader
                , Forms.Readonly.view formState |> Html.map (Form2Msg >> FormMsg)
                ]

        Form3State formState ->
            Html.div []
                [ formHeader
                , Forms.Headless.view formState |> Html.map (Form3Msg >> FormMsg)
                ]