/*jslint browser: true*/
/*global EventSource*/
/*exported initElmPorts*/
function initElmPorts(app) {
    var sources = {};

    function sendEventToElm(event) {
        app.ports.ssEventsJS.send({
            data: event.data || '', // TODO make this a Maybe?
            eventType: event.type,
            id: event.id || null
        });

    }

    app.ports.createEventSourceJS.subscribe(function (address) {
        sources[address] = new EventSource(address); // we only call if there isn't one yet
    });

    app.ports.addListenerJS.subscribe(function (addressAndEventType) {
        var address = addressAndEventType[0];
        var eventType = addressAndEventType[1];

        var eventSource = sources[address]; // we only call if it exists
        eventSource.addEventListener(eventType, sendEventToElm);
    });

    app.ports.createEventSourceAndAddListenerJS.subscribe(function (addressAndEventType) {
        var address = addressAndEventType[0];
        var eventType = addressAndEventType[1];

        sources[address] = new EventSource(address); // we only call if there isn't one yet
        var eventSource = sources[address];
        eventSource.addEventListener(eventType, sendEventToElm);
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
