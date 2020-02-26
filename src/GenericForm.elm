module GenericForm exposing
    ( initOrdinaryForm
    , initReadonlyForm
    , update
    , view
    )

import Html exposing (Html)
import Html.Events as HE
import Http
import Task exposing (Task)


type alias SaveableForm formState formMsg payload =
    { view : formState -> Html formMsg
    , update : formMsg -> formState -> ( formState, Cmd formMsg )

    -- alternative save for handling success/error elsewhere
    , save : formState -> Result (List String) (Task () payload)
    , validate : formState -> List String

    --, save : formState -> Cmd formMsg
    }


type alias ReadonlyForm formState formMsg =
    { view : formState -> Html formMsg
    , update : formMsg -> formState -> ( formState, Cmd formMsg )
    }


initSaveableForm :
    (formState -> Html formMsg)
    -> (formMsg -> formState -> ( formState, Cmd formMsg ))
    -> (formState -> Result (List String) (Task () payload))
    -> (formState -> List String)
    -> SaveableForm formState formMsg payload
initSaveableForm =
    SaveableForm


initReadonlyForm :
    (formState -> Html formMsg)
    -> (formMsg -> formState -> ( formState, Cmd formMsg ))
    -> ReadonlyForm formState formMsg
initReadonlyForm =
    ReadonlyForm


type GenericForm s m p
    = Saveable (SaveableForm s m p)
    | Readonly (ReadonlyForm s m)


type Msg formMsg
    = Save
    | Close
    | SaveResponse
    | FormMsg formMsg


update :
    Msg formMsg
    -> GenericForm formState formMsg formPayload
    -> ( GenericForm formState formMsg formPayload, Cmd (Msg formMsg) )
update msg model =
    case ( msg, model ) of
        ( Save, Saveable form ) ->
            case form.validate form of
                Result.Ok httpRequest ->
                    ( { model | saveState = Saving, validationErrors = [] }
                    , httpRequest
                    , { saveSucceeded = False }
                    )

                Result.Err validationErrors ->
                    ( { model | validationErrors = validationErrors }
                    , Job.init
                    , { saveSucceeded = False }
                    )

        --SaveResponseFail ->
        --    ( { model | saveState = NotSaving }, Job.init, { saveSucceeded = False } )
        --
        --SaveResponseSuccess onSaveJob ->
        --    ( model
        --    , Job.map FormMsg onSaveJob
        --    , { saveSucceeded = True }
        --    )
        FormMsg formMsg ->
            let
                ( newFormState, formJob ) =
                    model.update formMsg model.formState
            in
            ( { model | formState = newFormState }, Cmd.map FormMsg formJob, { saveSucceeded = False } )


saveForm :
    SaveableForm s m r
    -> Result (List String) (Task () (Msg m))
saveForm store model =
    let
        mapToJob =
            Result.map
                (Job.fromHttpTaskWithErrorHandling
                    (model.config.onSave model.formState store >> SaveResponseSuccess)
                    SaveResponseFail
                )
    in
    model.save store model.formState |> mapToJob


view : GenericForm formState formMsg formPayload -> formState -> Html Msg
view model formState =
    case model of
        Saveable form ->
            viewSaveable form formState

        Readonly form ->
            form.view formState


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
