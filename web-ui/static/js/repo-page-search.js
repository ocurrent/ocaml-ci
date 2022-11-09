

function title_comparator(a, b) {
  console.log("A", title_a)
  console.log("B", title_b)
  var title_a = a.getElementsByClassName("repo-title")[0].textContent
  var title_b = b.getElementsByClassName("repo-title")[0].textContent
  if (title_a < title_b) return -1
  if (title_a > title_b) return 1
  return 0
}

function time_comparator(a, b) {
  var ts_a = a.getAttribute("data-timestamp")
  var ts_b = b.getAttribute("data-timestamp")
  if (ts_a < ts_b) return -1
  if (ts_a > ts_b) return 1
  return 0
}

function sort(select) {
  var children = Array.from(body.children)
  if (select === "alpha") {
    children = children.sort(title_comparator)
  } else if (select === "recent") {
    children = children.sort(title_comparator)
  }
  // for (i = 0; i < itemsArr.length; ++i) {
  //   list.appendChild(itemsArr[i]);
  // }
  for (i = 0; i < children.length; ++i) {
    body.appendChild(children[i]);
  }
}

var body = null
// var children = null
var head = null

window.onload = function() {
  var initial = document.getElementById("table")
  // table -> thead
  head = initial.firstChild
  // table -> tbody
  body = initial.lastChild
  // children = Array.from(initial.lastChild.children)
  // sort()
}
