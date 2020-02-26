module EmbeddedForm exposing (..)

import Html exposing (Html)
import Html.Events as HE
import Task


{-| Form embedded in SomePage.
-}
type alias EmbeddedForm =
    { -- own data
      title : String
    , data : Int
    , -- form data
      isOpen : Bool
    , formState : FormState
    }


type alias FormState =
    Int


type Msg
    = -- page msg
      OwnMsg
      -- form msg
    | FormClose
    | FormSave
    | FormSaveResponse


init : ( EmbeddedForm, Cmd Msg )
init =
    ( { title = ""
      , data = 0
      , isOpen = False
      , formState = 0
      }
    , Cmd.none
    )


update : Msg -> EmbeddedForm -> ( EmbeddedForm, Cmd Msg )
update msg model =
    case msg of
        OwnMsg ->
            ( model, Cmd.none )

        FormSave ->
            case validate model of
                [] ->
                    ( model, toCmd FormSaveResponse )

                _ ->
                    ( model, Cmd.none )

        FormSaveResponse ->
            ( model, Cmd.none )

        FormClose ->
            ( { model | isOpen = False }, Cmd.none )


validate : EmbeddedForm -> List String
validate model =
    []


view : EmbeddedForm -> Html Msg
view model =
    let
        header =
            Html.div []
                [ Html.button [ HE.onClick FormSave ] [ Html.text "Save" ]
                , Html.button [ HE.onClick FormClose ] [ Html.text "Close" ]
                ]

        body =
            Html.text "Form body"

        form =
            Html.div [] [ header, body ]

        pageView =
            Html.text "Page Data"
    in
    case model.isOpen of
        True ->
            Html.div [] [ form, pageView ]

        False ->
            pageView


toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)
