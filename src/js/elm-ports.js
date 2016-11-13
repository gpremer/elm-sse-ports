/*jslint browser: true*/
/*global EventSource*/
/*exported initElmPorts*/
function initElmPorts(app) {
    var sources = {};

    function sendEventToElm(event) {
        app.ports.ssEventsJS.send({
            data: event.data, // Can't be null according to spec
            eventType: event.type, // Can't be because we listen for this event type
            id: event.id || null
        });
    }

    // We could have one function for typed and untyped, but then need to either expose Maybe to users or make
    // superfluous copy.
    function sendUntypedEventToElm(event) {
        app.ports.ssUntypedEventsJS.send({
            data: event.data, // Can't be null according to spec
            eventType: null,
            id: event.id || null
        });
    }

    function createNewEventSource(address) {
        sources[address] = new EventSource(address); // we only call if there isn't one yet

        return sources[address];
    }

    app.ports.createEventSourceJS.subscribe(createNewEventSource);

    app.ports.addListenerJS.subscribe(function (addressAndEventType) {
        var address = addressAndEventType[0];
        var eventType = addressAndEventType[1];

        var eventSource = sources[address]; // we only call if it exists
        if ( eventType )
            eventSource.addEventListener(eventType, sendEventToElm);
        else
            eventSource.onmessage = sendUntypedEventToElm;
    });

    app.ports.createEventSourceAndAddListenerJS.subscribe(function (addressAndEventType) {
        var address = addressAndEventType[0];
        var eventType = addressAndEventType[1];

        var eventSource = createNewEventSource(address);
        if ( eventType )
            eventSource.addEventListener(eventType, sendEventToElm);
        else
            eventSource.onmessage = sendUntypedEventToElm;
    });

    app.ports.removeListenerJS.subscribe(function (addressAndEventType) {
        var address = addressAndEventType[0];
        var eventType = addressAndEventType[1];

        var eventSource = sources[address]; // we only call if it exists
        eventSource.removeEventListener(eventType, sendEventToElm);
    });

    app.ports.deleteEventSourceJS.subscribe(function (address) {
        sources[address].close(); // we only call if it exists
        delete sources[address]; // we only call if it exists
    });
}
