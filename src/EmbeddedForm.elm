module EmbeddedForm exposing (..)

import Html exposing (Html)
import Html.Events as HE
import Task


{-| Form state embedded in the parent page. Parent handles all form based logistics.
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
    | Open Int


type Msg
    = -- page msg
      OwnMsg
      -- form handling msg
    | FormClose
    | FormSave
    | FormSaveResponse
      -- form msg
    | Inc


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
        OwnMsg ->
            ( model, Cmd.none )

        Inc ->
            case model.formState of
                NotOpen ->
                    -- we have a invalid state, do some error handling
                    ( model, Cmd.none )

                Open x ->
                    ( { model | formState = Open (x + 1) }, Cmd.none )

        FormSave ->
            case validate model of
                [] ->
                    ( model, toCmd FormSaveResponse )

                errs ->
                    ( { model | validation = errs }, Cmd.none )

        FormSaveResponse ->
            ( model, Cmd.none )

        FormClose ->
            ( { model | formState = NotOpen }, Cmd.none )


validate : EmbeddedForm -> List String
validate model =
    []


view : EmbeddedForm -> Html Msg
view model =
    let
        formHeader =
            Html.div []
                [ Html.button [ HE.onClick FormSave ] [ Html.text "Save" ]
                , Html.button [ HE.onClick FormClose ] [ Html.text "Close" ]
                ]

        formBody =
            Html.div []
                [ Html.div []
                    (List.map Html.text model.validation)
                , Html.button [ HE.onClick Inc ] [ Html.text "Inc" ]
                ]

        form =
            Html.div [] [ formHeader, formBody ]

        pageView =
            Html.text "Page Data"
    in
    case model.formState of
        Open data ->
            Html.div [] [ form, pageView ]

        NotOpen ->
            pageView


toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)
