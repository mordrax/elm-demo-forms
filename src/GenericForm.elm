module GenericForm exposing
    ( update
    , view
    )

import Forms.Ordinary
import Forms.Readonly
import Html exposing (Html)
import Html.Events as HE
import Http
import Task exposing (Task)


type alias SaveableForm formState formMsg payload =
    { view : formState -> Html formMsg
    , update : formMsg -> formState -> ( formState, Cmd formMsg )
    , formState : formState

    -- alternative save for handling success/error elsewhere
    , save : formState -> Result (List String) (Task () payload)
    , validate : formState -> List String

    --, save : formState -> Cmd formMsg
    }


type alias NonSavingForm formState formMsg =
    { view : formState -> Html formMsg
    , update : formMsg -> formState -> ( formState, Cmd formMsg )
    , formState : formState
    }


initSaveableForm :
    (formState -> Html formMsg)
    -> (formMsg -> formState -> ( formState, Cmd formMsg ))
    -> formState
    -> (formState -> Result (List String) (Task () payload))
    -> (formState -> List String)
    -> SaveableForm formState formMsg payload
initSaveableForm =
    SaveableForm


initNonSavingForm :
    (formState -> Html formMsg)
    -> (formMsg -> formState -> ( formState, Cmd formMsg ))
    -> formState
    -> NonSavingForm formState formMsg
initNonSavingForm =
    NonSavingForm


type alias GenericForm formState formMsg payload =
    { formType : FormType formState formMsg payload
    , formState : formState
    , validation : List String
    }


type Msg formMsg
    = Save
    | Close
    | SaveResponse
    | FormMsg formMsg



--type FormMsg
--    = OrdinaryMsg Forms.Ordinary.Msg
--    | ReadonlyMsg Forms.Readonly.Msg


type FormType s m p
    = Saveable (SaveableForm s m p)
    | Readonly (NonSavingForm s m)


update :
    Msg formMsg
    -> GenericForm formState formMsg formPayload
    -> ( GenericForm formState formMsg formPayload, Cmd (Msg formMsg) )
update msg model =
    case ( msg, model.formType ) of
        ( Save, Saveable form ) ->
            case form.validate form.formState of
                [] ->
                    case form.save form.formState of
                        Result.Ok httpRequest ->
                            ( { model
                                | validation = []
                              }
                            , Task.attempt (always SaveResponse) httpRequest
                            )

                        Result.Err validationErrors ->
                            ( { model | validation = validationErrors }
                            , Cmd.none
                            )

        ( Close, _ ) ->
            ( { model | validation = [] }
            , Cmd.none
            )

        ( SaveResponse, _ ) ->
            -- update the model { model | ... } andThen
            update Close model

        ( FormMsg formMsg, Saveable form ) ->
            let
                ( newFormState, formJob ) =
                    form.update formMsg form.formState
            in
            ( { model | formType = Saveable { form | formState = newFormState } }
            , Cmd.map FormMsg formJob
            )

        ( FormMsg formMsg, Readonly form ) ->
            let
                ( newFormState, formJob ) =
                    form.update formMsg form.formState
            in
            ( { model | formType = Readonly { form | formState = newFormState } }
            , Cmd.map FormMsg formJob
            )


view : GenericForm formState formMsg formPayload -> formState -> Html (Msg formMsg)
view model formState =
    case model.formType of
        Saveable form ->
            viewSaveable form formState

        Readonly form ->
            viewReadonly form formState


viewSaveable : SaveableForm formState formMsg payload -> formState -> Html (Msg formMsg)
viewSaveable form state =
    let
        formHeader =
            Html.div []
                [ Html.button [ HE.onClick Save ] [ Html.text "Save" ]
                , Html.button [ HE.onClick Close ] [ Html.text "Close" ]
                ]

        formBody =
            form.view state
    in
    Html.div []
        [ formHeader
        , formBody |> Html.map FormMsg
        ]


viewReadonly : NonSavingForm formState formMsg -> formState -> Html (Msg formMsg)
viewReadonly a b =
    Html.text ""
