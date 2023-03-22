
function appendChild(data) {
    let scroller = document.querySelector('#scroller');
    let anchor = document.querySelector('#anchor');
    let msg = document.createElement('span');
    msg.className = 'tr';
    let msg2 = document.createElement('span');
    msg2.className = 'th';
    // TODO: Figure out how to annotate each line with the right markup
    msg.innerText = data;
    scroller.insertBefore(msg, anchor);
  }

function ws_client(api_path) {
    const ws_path = api_path.replace("http", "ws");
    // console.log("ws_client: " + ws_path);

    var ws_log = new WebSocket (ws_path);

    ws_log.onmessage = function (event) {
        var logs = document.getElementById("scroller");
        var logs_data = event.data;
        // console.log("ONMESSAGE: ", logs_data);
        appendChild(logs_data);
    };
}

// Usage:  ensure element is visible
ws_client(location.origin + "/ws" + location.pathname);
