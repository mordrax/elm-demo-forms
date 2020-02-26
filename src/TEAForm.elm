module TAEForm exposing (..)

import Forms.Ordinary exposing (Ordinary)
import Html exposing (Html)
import Html.Events as HE
import Task


{-| Form state embedded in the parent page. Parent handles all form based logistics.
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
      -- form msg
    | FormClose
    | FormSave
    | FormSaveResponse
    | Form1Msg Forms.Ordinary.Msg


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
                    ( model, Task.perform (always FormSaveResponse) httpRequest )

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
                |> (Forms.Ordinary.view >> Html.map Form1Msg)

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
