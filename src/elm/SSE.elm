port module SSE exposing ( SsEvent
                         , EventType
                         , Endpoint
                         , SseAccess
                         , SseEventDecoder
                         , hasListenerFor
                         , create
                         , withoutAnyListener
                         , withListener
                         , withUntypedListener
                         , withoutListener
                         , withoutUntypedListener
                         , serverSideEvents
                         )

import Dict exposing (..)
import Maybe.Extra

type alias SsEvent =
    { data : String
    , eventType : String
    , id : Maybe String
    }

type alias UntypedSsEvent =
    { data : String
    , id : Maybe String
    }

{-
| A function that takes an SseEvent, of a specific type, and converts it to a domain-specific class.
-}
type alias SseEventDecoder msg = SsEvent -> msg

type alias UntypedSseEventDecoder msg = UntypedSsEvent -> msg

type alias Endpoint = String

type alias EventType = String

type alias SseAccess msg =
    { endpoint: Endpoint
    , noopMessage: msg
    , decoders : Dict String (SseEventDecoder msg)
    , untypedDecoder : Maybe (UntypedSseEventDecoder msg)
    }

hasListenerFor: EventType -> SseAccess msg -> Bool
hasListenerFor eventType sseAccess =
    Dict.member eventType sseAccess.decoders

{-| Create a new SseAccess instance. This is instance is not yet listening for SSE events. That only happens when the
first listener is attached.
-}
create: Endpoint -> msg -> SseAccess msg
create endpoint noop =
    SseAccess endpoint noop Dict.empty Nothing

{-| Stopping a subscription is not enough. Although that does free Elm from having to handle SSE event, we also need
 to release the resource for the browser and the server or the SSE socket remains open. Calling this method also
 clears all event handlers. If this sse instance is used again, listeners need to be added again.
-}
withoutAnyListener: SseAccess msg -> (SseAccess msg, Cmd msg)
withoutAnyListener sseAccess =
    ( { sseAccess | decoders = Dict.empty, untypedDecoder = Nothing }
    , if hasListeners sseAccess then deleteEventSourceJS sseAccess.endpoint else Cmd.none
    )

{-| Adds a listener for SSE events that don't have a type. This is very much like calling withListener with an event
type of "message". In fact in the versions of Firefox and Chromium I tested against, it is completely the same at the
Javascript level, but I don't see this behaviour mentioned in the spec. On the Elm side, this function has the benefit
 of calling eventually calling a decoder that doesn't need to handle the event type.
-}
withUntypedListener: UntypedSseEventDecoder msg -> SseAccess msg -> (SseAccess msg, Cmd msg)
withUntypedListener eventDecoder sseAccess =
    let
        cmd =
            if hasListeners sseAccess then
                addListenerJS (sseAccess.endpoint, Nothing)
             else
                createEventSourceAndAddListenerJS (sseAccess.endpoint, Nothing)
    in
    ( { sseAccess | untypedDecoder = Just eventDecoder }
    , cmd
    )

{-| Adds a listener for a specific event type. Whenever such an event is received, it is passed as an SsEvent to the
provided decoder function and the result of decoding is then pumped out of the subscription. If there is already a
listener for the event type, it is replaced by the new one.
-}
withListener: EventType -> SseEventDecoder msg -> SseAccess msg -> (SseAccess msg, Cmd msg)
withListener eventType eventDecoder sseAccess =
    let
        cmd =
            if hasListeners sseAccess then
                if Dict.member eventType sseAccess.decoders then
                    Cmd.none -- All JS listener are the same, no need to set again
                else
                    addListenerJS (sseAccess.endpoint, Just eventType)
            else
                createEventSourceAndAddListenerJS (sseAccess.endpoint, Just eventType)

    in
        ( { sseAccess | decoders = Dict.insert eventType eventDecoder sseAccess.decoders }
        , cmd
        )

{-| Stops listening for untyped events. If no more listeners are active, also release the underlying SSE resource.
-}
withoutUntypedListener: SseAccess msg -> (SseAccess msg, Cmd msg)
withoutUntypedListener sseAccess =
    let
        sseAccessWithoutUntypedListener = { sseAccess | untypedDecoder = Nothing }
        cmd =
            if hasListeners sseAccessWithoutUntypedListener then
                removeListenerJS (sseAccess.endpoint, Nothing)
            else
                deleteEventSourceJS sseAccess.endpoint
    in
        ( sseAccessWithoutUntypedListener
        , cmd
        )

{-| Stops listening for events of the given type. If no more listeners are active, also release the underlying SSE
resource.
-}
withoutListener: String -> SseAccess msg -> (SseAccess msg, Cmd msg)
withoutListener eventType sseAccess =
    let
        sseAccessWithoutListener = { sseAccess | decoders = Dict.remove eventType sseAccess.decoders }
        cmd =
            if hasListeners sseAccessWithoutListener then
                removeListenerJS (sseAccess.endpoint, Just eventType)
            else
                deleteEventSourceJS sseAccess.endpoint
    in
        ( sseAccessWithoutListener
        , cmd
        )

hasListeners: SseAccess msg -> Bool
hasListeners sseAccess =
    (Maybe.Extra.isJust sseAccess.untypedDecoder) || not ( Dict.isEmpty sseAccess.decoders )

port addListenerJS: (Endpoint, Maybe EventType) -> Cmd msg

port removeListenerJS: (Endpoint, Maybe EventType) -> Cmd msg

port createEventSourceJS: Endpoint -> Cmd msg

port deleteEventSourceJS: Endpoint -> Cmd msg

-- Needed because Cmd.batch is not ordered
port createEventSourceAndAddListenerJS: (Endpoint, Maybe EventType) -> Cmd msg

serverSideEvents: SseAccess msg -> Sub msg
serverSideEvents sseAccess =
    Sub.batch
        [ ssEventsJS <| decodersToEventMapper sseAccess
        , Maybe.withDefault Sub.none (Maybe.map ssUntypedEventsJS sseAccess.untypedDecoder)
        ]

decodersToEventMapper: SseAccess msg -> SsEvent -> msg
decodersToEventMapper sseAccess event =
        let
            maybeMsg = maybeMap (Dict.get event.eventType sseAccess.decoders) event -- by design we'll always find a decoder
        in
            Maybe.withDefault (sseAccess.noopMessage) maybeMsg

maybeMap: Maybe (a -> b) -> a -> Maybe b
maybeMap maybeF a = Maybe.map (\f -> f a) maybeF

port ssEventsJS: (SsEvent -> msg) -> Sub msg

port ssUntypedEventsJS: (UntypedSsEvent -> msg) -> Sub msg
