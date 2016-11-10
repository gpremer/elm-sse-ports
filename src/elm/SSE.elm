port module SSE exposing ( SsEvent
                         , EventType
                         , Endpoint
                         , SseAccess
                         , SseEventDecoder
                         , hasListenerFor
                         , create
                         , addListener
                         , removeListener
                         , serverSideEvents
                         )

import Set exposing (..)
import Dict exposing (..)

type alias SsEvent =
    { data : String
    , eventType : String
    , id : Maybe String
    }

{-| A function that takes an SseEvent, of a specific type, and converts it to a domain-specific class.
-}
type alias SseEventDecoder msg = SsEvent -> msg

type alias Endpoint = String

type alias EventType = String

type alias SseAccess msg =
    { endpoint: Endpoint
    , decoders : Dict String (SseEventDecoder msg)
    , defaultMsg : msg
    }

hasListenerFor: EventType -> SseAccess msg -> Bool
hasListenerFor eventType sseAccess =
    Dict.member eventType sseAccess.decoders

{-| Create a new SseAccess instance. This is instance is not yet listening for SSE events. That only happens when the
first listener is attached.
-}
create: Endpoint -> msg -> SseAccess msg
create endpoint defaultMsg =
    SseAccess endpoint Dict.empty defaultMsg

addListener: EventType -> SseEventDecoder msg -> SseAccess msg -> (SseAccess msg, Cmd msg)
addListener eventType eventDecoder sseAccess =
    let
        hasAnyListenersAlready = not <| Dict.isEmpty sseAccess.decoders
        alreadyHasThisListener = Dict.member eventType sseAccess.decoders

        cmd =
            if hasAnyListenersAlready then
                if alreadyHasThisListener then
                    Cmd.none
                else
                    addListenerJS (sseAccess.endpoint, eventType)
            else
                createEventSourceAndAddListenerJS (sseAccess.endpoint, eventType)

    in
        ( { sseAccess | decoders = Dict.insert eventType eventDecoder sseAccess.decoders }
        , cmd
        )

removeListener: String -> SseAccess msg -> (SseAccess msg, Cmd msg)
removeListener eventType sseAccess =
    let
        withoutListener = { sseAccess | decoders = Dict.remove eventType sseAccess.decoders }

        hasAnyListenersLeft = not <| Dict.isEmpty withoutListener.decoders
        actuallyHasThisListener = Dict.member eventType sseAccess.decoders

        cmd =
            if hasAnyListenersLeft then
                removeListenerJS (sseAccess.endpoint, eventType)
            else
                deleteEventSourceJS sseAccess.endpoint

    in
        ( withoutListener
        , cmd
        )

port addListenerJS: (Endpoint, EventType) -> Cmd msg

port removeListenerJS: (Endpoint, EventType) -> Cmd msg

port createEventSourceJS: Endpoint -> Cmd msg

port deleteEventSourceJS: Endpoint -> Cmd msg

-- Needed because Cmd.batch is not ordered
port createEventSourceAndAddListenerJS: (Endpoint, EventType) -> Cmd msg

serverSideEvents: SseAccess msg -> Sub msg
serverSideEvents sseAccess =
    ssEventsJS <| decodersToEventMapper sseAccess

decodersToEventMapper: SseAccess msg -> SsEvent -> msg
decodersToEventMapper sseAccess event =
        let
            maybeMsg = maybeMap (Dict.get event.eventType sseAccess.decoders) event
        in
            Maybe.withDefault sseAccess.defaultMsg maybeMsg

maybeMap: Maybe (a -> b) -> a -> Maybe b
maybeMap maybeF a =
    case maybeF of
        Just f  -> Just (f a)
        Nothing -> Nothing

port ssEventsJS: (SsEvent -> msg) -> Sub msg
