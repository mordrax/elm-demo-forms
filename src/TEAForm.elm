module TEAForm exposing (..)

import Forms.Ordinary exposing (Ordinary)
import Html exposing (Html)
import Html.Events as HE
import Task


{-| Form state and controls no longer embedded in the parent page but rather nested under the parent.
Parent still handles all form based logistics, but form itself handles it's own Msg, state updates

    Pros:
    - Ultimate flexibility
    - Explicit coupling by segregation of responsibilities

    Cons:
    - Repetition of mundane mechanics ( closing, validation )
    - Repetition of logic and behaviour ( error handling, saving )
    - Unncecessary dependency of state ( form state depends on the page existing )

-}
type alias TEAForm =
    { -- own data
      title : String
    , data : Int

    -- form data
    , formState : Maybe Ordinary
    , validation : List String
    }


type Msg
    = -- page msg
      OwnMsg
      -- form handling msg
    | FormClose
    | FormSave
    | FormSaveResponse
      -- form msg
    | FormOrdinaryMsg Forms.Ordinary.Msg


init : ( TEAForm, Cmd Msg )
init =
    ( { title = ""
      , data = 0
      , formState = Nothing
      , validation = []
      }
    , Cmd.none
    )


update : Msg -> TEAForm -> ( TEAForm, Cmd Msg )
update msg model =
    case ( msg, model.formState ) of
        ( OwnMsg, _ ) ->
            -- do something on the page
            ( model, Cmd.none )

        ( FormSave, Just formState ) ->
            case Forms.Ordinary.save formState of
                Result.Ok httpRequest ->
                    ( model, Task.attempt (always FormSaveResponse) httpRequest )

                Result.Err errs ->
                    ( { model | validation = errs }, Cmd.none )

        ( FormSave, Nothing ) ->
            -- invalid state, log error
            ( model, Cmd.none )

        ( FormSaveResponse, _ ) ->
            ( { model | formState = Nothing }
            , Cmd.none
            )

        ( FormClose, _ ) ->
            ( { model | formState = Nothing }, Cmd.none )

        ( FormOrdinaryMsg formMsg, Just formState ) ->
            let
                ( newFormState, formCmd ) =
                    Forms.Ordinary.update formMsg formState
            in
            ( { model | formState = Just newFormState }, Cmd.map FormOrdinaryMsg formCmd )

        ( FormOrdinaryMsg _, Nothing ) ->
            -- log error about msgs coming when no form exists
            ( model, Cmd.none )


view : TEAForm -> Html Msg
view model =
    let
        formHeader =
            Html.div []
                [ Html.button [ HE.onClick FormSave ] [ Html.text "Save" ]
                , Html.button [ HE.onClick FormClose ] [ Html.text "Close" ]
                ]

        formBody formState =
            formState
                |> (Forms.Ordinary.view >> Html.map FormOrdinaryMsg)

        form formState =
            Html.div [] [ formHeader, formBody formState ]

        pageView =
            Html.text "Page Data"
    in
    case model.formState of
        Just formState ->
            Html.div [] [ form formState, pageView ]

        Nothing ->
            pageView


toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)
