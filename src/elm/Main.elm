port module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (..)
import String

import SSE exposing (..)

main =
    App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions}

sseEndpoint = "http://localhost:8080/events"

-- model

type alias Model =
    { custom: Maybe String
    , generic: Maybe String
    , sse: SseAccess Msg
    }

init: (Model, Cmd Msg)
init =
    let
        sseAccess = SSE.create sseEndpoint UnknownEventType
    in
        ( Model Nothing Nothing sseAccess , Cmd.none )

customEvents: EventType
customEvents = "custom"

genericEvents: EventType
genericEvents = "message"

-- Update

type Msg = CustomEvent String
    | GenericEvent String
    | ListenForCustomEvents
    | ListenForGenericEvents
    | NoMoreCustomEvents
    | NoMoreGenericEvents
    | UnknownEventType

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        CustomEvent text -> ({ model | custom = Just text }, Cmd.none)

        GenericEvent text -> ({ model | generic = Just text }, Cmd.none)

        ListenForCustomEvents ->
            let
                (sse, cmd) = model.sse |> addListener customEvents (\ev -> CustomEvent ev.data)
            in
                ( {model | sse = sse }
                , cmd
                )

        ListenForGenericEvents ->
            let
                (sse, cmd) = model.sse |> addListener genericEvents (\ev -> GenericEvent ev.data)
            in
                ( {model | sse = sse }
                , cmd
                )

        NoMoreCustomEvents ->
            let
                (sse, cmd) = model.sse |> removeListener customEvents
            in
                ( {model | sse = sse }
                , cmd
                )

        NoMoreGenericEvents ->
            let
                (sse, cmd) = model.sse |> removeListener genericEvents
            in
                ( {model | sse = sse }
                , cmd
                )

        UnknownEventType -> (model, Cmd.none) -- Silently drop events we don't know about. Can't happen anyway.

listeningFor: Model -> EventType -> Bool
listeningFor model eventType =
    SSE.hasListenerFor eventType model.sse

-- View

view: Model -> Html Msg
view model =
    div []
        [ p [] [text <| Maybe.withDefault "No custom event yet" (Maybe.map (String.append "Last custom event: ") model.custom) ]
        , p [] [text <| Maybe.withDefault "No generic event yet" (Maybe.map (String.append "Last generic event: ") model.generic) ]
        , p [] [eventListeningToggleButton (listeningFor model customEvents) "Listen for custom events" "Stop listening for custom events" ListenForCustomEvents NoMoreCustomEvents]
        , p [] [eventListeningToggleButton (listeningFor model genericEvents) "Listen for generic events" "Stop listening for generic events" ListenForGenericEvents NoMoreGenericEvents]
        ]

eventListeningToggleButton: Bool -> String -> String -> Msg -> Msg -> Html Msg
eventListeningToggleButton on offText onText offMsg onMsg =
    let
        (txt, msg) =
            if on then
                (onText, onMsg)
            else
                (offText, offMsg)
    in
        button [onClick msg] [text txt]

-- Subscriptions

subscriptions: Model -> Sub Msg
subscriptions model =
    serverSideEvents model.sse
