
window.onload = function() {
  var table_root = document.getElementById("table");
  // table -> thead
  head = table_root.firstChild;
  // table -> tbody
  body = table_root.lastChild;

  var charts = charts_init();
  charts_poll(location.origin + "/api" + location.pathname, charts);
}
