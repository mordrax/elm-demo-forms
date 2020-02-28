module EmbeddedForm exposing (..)

import Html exposing (Html)
import Html.Events as HE
import Task


{-| Form state and controls embedded in the parent page.
Parent handles all form based logistics.

    Pros:
    - Ultimate flexibility
    - Everything in one place
    - Easiest solution

    Cons:
    - Repetition of mundane mechanics ( closing, validation )
    - Repetition of logic and behaviour ( error handling, saving )
    - Coupling

-}
type alias EmbeddedForm =
    { -- own data
      title : String
    , data : Int

    -- form data
    , formState : FormState
    , validation : List String
    }


type FormState
    = NotOpen
    | Open { formData : Int }


type Msg
    = -- bunch of page msgs
      OwnMsgs
      -- form handling msg
    | FormClose
    | FormSave
    | FormSaveResponse
      -- bunch of form msgs
    | FormMsgs


init : ( EmbeddedForm, Cmd Msg )
init =
    ( { title = ""
      , data = 0
      , formState = NotOpen
      , validation = []
      }
    , Cmd.none
    )


update : Msg -> EmbeddedForm -> ( EmbeddedForm, Cmd Msg )
update msg model =
    case msg of
        OwnMsgs ->
            ( model, Cmd.none )

        FormMsgs ->
            case model.formState of
                NotOpen ->
                    -- we have a invalid state, do some error handling
                    ( model, Cmd.none )

                Open formData ->
                    -- perform some update to the form
                    ( { model | formState = Open { formData | formData = formData.formData + 1 } }
                    , Cmd.none
                    )

        FormSave ->
            case validate model.formState of
                [] ->
                    ( model, Task.perform (always FormSaveResponse) (Task.succeed ()) )

                errs ->
                    ( { model | validation = errs }, Cmd.none )

        FormSaveResponse ->
            ( model, Cmd.none )

        FormClose ->
            ( { model | formState = NotOpen }, Cmd.none )


validate : FormState -> List String
validate formState =
    case formState of
        NotOpen ->
            []

        Open data ->
            -- do some validations
            []


view : EmbeddedForm -> Html Msg
view model =
    let
        pageView =
            Html.text "Page Data"
    in
    case model.formState of
        Open formData ->
            Html.div []
                [ viewForm formData model.validation
                , pageView
                ]

        NotOpen ->
            pageView


viewForm : { formData : Int } -> List String -> Html Msg
viewForm formState validation =
    let
        formHeader =
            Html.div []
                [ Html.button [ HE.onClick FormSave ] [ Html.text "Save" ]
                , Html.button [ HE.onClick FormClose ] [ Html.text "Close" ]
                ]

        formBody =
            Html.div []
                [ Html.div []
                    (List.map Html.text validation)

                -- display form data
                ]
    in
    Html.div [] [ formHeader, formBody ]
