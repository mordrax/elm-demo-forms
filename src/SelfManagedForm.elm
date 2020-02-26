module SelfManagedForm exposing (..)

import Html exposing (Html)
import Task


type alias SelfManagedForm =
    { isOpen : Bool
    }


type Msg
    = Msg
    | Close
    | Save
    | SaveResponse


init : ( SelfManagedForm, Cmd Msg )
init =
    ( { isOpen = False }
    , Cmd.none
    )


update : Msg -> SelfManagedForm -> ( SelfManagedForm, Cmd Msg )
update msg model =
    case msg of
        Msg ->
            ( model, Cmd.none )

        Save ->
            case validate model of
                [] ->
                    ( model, toCmd SaveResponse )

                _ ->
                    ( model, Cmd.none )

        SaveResponse ->
            ( model, Cmd.none )

        Close ->
            ( { model | isOpen = False }, Cmd.none )


validate : SelfManagedForm -> List String
validate model =
    []


view : SelfManagedForm -> Html Msg
view model =
    Html.text "SelfManagedForm"


toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)
