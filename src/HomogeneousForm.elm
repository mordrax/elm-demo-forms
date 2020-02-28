module HomogeneousForm exposing (..)

import Forms.Headless exposing (Headless)
import Forms.Ordinary exposing (Ordinary)
import Forms.Readonly exposing (Readonly)
import Html exposing (Html)
import Html.Events as HE
import Task exposing (Task)


{-| Form state and controls nested under a 'form manager' parent which does not necessarily have to be any particular page.
Parent still handles all form based logistics, but form itself handles it's own Msg, state updates

    Pros:
    - No coupling between a form and a page ( can live as siblings )
    - Form handling logic and behaviour is implemented once

    Cons:
    - All forms must conform to the same behaviour and therefore the same output type
      Any deviation from the above behaviour becomes very messy to manage

-}
type alias HomogeneousForm =
    { formState : FormState
    , validation : List String
    }


type FormState
    = NoForm
    | Form1State Ordinary
    | Form2State Readonly
    | Form3State Headless


type Msg
    = FormMsg FormMsg
    | Open OpenForm
    | Save
    | SaveResponse
    | Close


type OpenForm
    = OpenForm1
    | OpenForm2
    | OpenForm3


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
            case saveForm model of
                Result.Err err ->
                    ( { model | validation = err }, Cmd.none )

                Result.Ok task ->
                    ( model, Task.attempt (always SaveResponse) task )

        SaveResponse ->
            --update the model, close the form
            ( { model
                | formState = NoForm
                , validation = []
              }
            , Cmd.none
            )

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

        _ ->
            ( model, Cmd.none )


saveForm : HomogeneousForm -> Result (List String) (Task () String)
saveForm model =
    case model.formState of
        NoForm ->
            Result.Err [ "Cannot save the form, no form exists." ]

        Form1State formState ->
            Forms.Ordinary.save formState

        Form2State formState ->
            -- can't save as form is readonly
            --Forms.Readonly.save formState
            Result.Err [ "Cannot save the form, does not support saving." ]

        Form3State formState ->
            Forms.Headless.save formState


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

                -- can't show this, as the form has no view
                --, Forms.Headless.view formState |> Html.map (Form3Msg >> FormMsg)
                ]
