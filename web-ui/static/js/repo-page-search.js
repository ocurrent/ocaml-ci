
function title_comparator(a, b) {
  var title_a = a.getElementsByClassName("repo-title")[0].textContent.toLowerCase()
  var title_b = b.getElementsByClassName("repo-title")[0].textContent.toLowerCase()
  if (title_a < title_b) return -1
  if (title_a > title_b) return 1
  return 0
}

function time_comparator(a, b) {
  var ts_a = parseFloat(a.getAttribute("data-timestamp"))
  var ts_b = parseFloat(b.getAttribute("data-timestamp"))
  if (ts_a < ts_b) return -1
  if (ts_a > ts_b) return 1
  return 0
}

function sort(select) {
  var children = Array.from(body.children)
  if (select === "alpha") {
    children = children.sort(title_comparator)
  } else if (select === "recent") {
    children = children.sort(time_comparator)
  }
  for (i = 0; i < children.length; ++i) {
    body.appendChild(children[i]);
  }
}

function search(target) {
  var children = Array.from(body.children)

  function has_substr(child, ss) {
    var title = child.getElementsByClassName("repo-title")[0].textContent.toLowerCase()
    return title.indexOf(ss.toLowerCase()) !== -1
  }

  for (i = 0; i < children.length; ++i) {
    if (has_substr(children[i], target)) {
      children[i].style.display = "";
    } else {
      children[i].style.display = "none";
    }
  }
}

var body = null

window.onload = function() {
  var table_root = document.getElementById("table")
  // table -> tbody
  body = table_root.lastChild
  sort("alpha")
}
