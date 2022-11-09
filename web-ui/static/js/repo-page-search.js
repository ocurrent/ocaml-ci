
<<<<<<< HEAD
function title_comparator(a, b) {
  var title_a = a.getElementsByClassName("repo-title")[0].textContent.toLowerCase()
  var title_b = b.getElementsByClassName("repo-title")[0].textContent.toLowerCase()
=======

function title_comparator(a, b) {
  console.log("A", title_a)
  console.log("B", title_b)
  var title_a = a.getElementsByClassName("repo-title")[0].textContent
  var title_b = b.getElementsByClassName("repo-title")[0].textContent
>>>>>>> 9c78ef6 (Progress with JS for repo sorting)
  if (title_a < title_b) return -1
  if (title_a > title_b) return 1
  return 0
}

function time_comparator(a, b) {
<<<<<<< HEAD
  var ts_a = parseFloat(a.getAttribute("data-timestamp"))
  var ts_b = parseFloat(b.getAttribute("data-timestamp"))
  if (ts_a < ts_b) return -1
  if (ts_a > ts_b) return 1
  // Fallback to title comparison for consistency when switching
  // between the two; we don't want rows swapping unnecessarily
  return title_comparator(a, b)
=======
  var ts_a = a.getAttribute("data-timestamp")
  var ts_b = b.getAttribute("data-timestamp")
  if (ts_a < ts_b) return -1
  if (ts_a > ts_b) return 1
  return 0
>>>>>>> 9c78ef6 (Progress with JS for repo sorting)
}

function sort(select) {
  var children = Array.from(body.children)
  if (select === "alpha") {
    children = children.sort(title_comparator)
  } else if (select === "recent") {
<<<<<<< HEAD
    children = children.sort(time_comparator)
  }
=======
    children = children.sort(title_comparator)
  }
  // for (i = 0; i < itemsArr.length; ++i) {
  //   list.appendChild(itemsArr[i]);
  // }
>>>>>>> 9c78ef6 (Progress with JS for repo sorting)
  for (i = 0; i < children.length; ++i) {
    body.appendChild(children[i]);
  }
}

<<<<<<< HEAD
function search(target) {
  var children = Array.from(body.children)

  function has_substr(child, ss) {
    var title = child.getElementsByClassName("repo-title")[0].textContent.toLowerCase()
    return title.indexOf(ss.toLowerCase()) !== -1
  }
  var n_visible = 0
  for (i = 0; i < children.length; ++i) {
    if (has_substr(children[i], target)) {
      children[i].style.display = "";
      n_visible++
    } else {
      children[i].style.display = "none";
    }
  }
  var n_repositories_header = head.firstChild.firstChild.firstChild
  n_repositories_header.textContent = `Repositories (${n_visible})`
}

var head = null
var body = null

window.onload = function() {
  var table_root = document.getElementById("table")
  // table -> thead
  head = table_root.firstChild
  // table -> tbody
  body = table_root.lastChild
=======
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
>>>>>>> 9c78ef6 (Progress with JS for repo sorting)
}
