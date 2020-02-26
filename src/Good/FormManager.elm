module Good.FormManager exposing
    ( initOrdinaryForm
    , initReadonlyForm
    , update
    , view
    )

import Html exposing (Html)
import Http
import Task exposing (Task)


type alias OrdinaryForm formState formMsg response =
    { view : formState -> Html formMsg
    , update : formMsg -> formState -> ( formState, Cmd formMsg )

    -- alternative save for handling success/error elsewhere
    , save : formState -> Result (List String) (Task () response)

    --, save : formState -> Cmd formMsg
    }


type alias ReadonlyForm formState formMsg =
    { view : formState -> Html formMsg
    , update : formMsg -> formState -> ( formState, Cmd formMsg )
    }


initOrdinaryForm :
    (formState -> Html formMsg)
    -> (formMsg -> formState -> ( formState, Cmd formMsg ))
    -> (formState -> Result (List String) (Task () response))
    -> OrdinaryForm formState formMsg response
initOrdinaryForm a b c =
    OrdinaryForm a b c


initReadonlyForm :
    (formState -> Html formMsg)
    -> (formMsg -> formState -> ( formState, Cmd formMsg ))
    -> ReadonlyForm formState formMsg
initReadonlyForm a b =
    ReadonlyForm a b


type FormManager s m p
    = Ordinary (OrdinaryForm s m p)
    | Readonly (ReadonlyForm s m)


type Msg formMsg
    = SaveRequest
    | SaveResponse
    | FormMsg formMsg


update :
    Msg formMsg
    -> FormManager formState formMsg formPayload
    -> ( FormManager formState formMsg formPayload, Cmd (Msg formMsg) )
update msg model =
    case msg of
        SaveRequest ->
            case model of
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
    OrdinaryForm s m r
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


view : FormManager formState formMsg formPayload -> formState -> Html Msg
view model formState =
    case model of
        Ordinary form ->
            form.view formState

        Readonly form ->
            form.view formState
