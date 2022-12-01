
function title_comparator(a, b) {
  var title_a =
    a
    .getElementsByClassName("repo-title")[0]
    .textContent
    .toLowerCase();
  var title_b =
    b
    .getElementsByClassName("repo-title")[0]
    .textContent
    .toLowerCase();
  if (title_a < title_b) return -1;
  if (title_a > title_b) return 1;
  return 0;
}

function time_comparator(a, b) {
  var ts_a = parseFloat(a.getAttribute("data-timestamp"));
  var ts_b = parseFloat(b.getAttribute("data-timestamp"));
  if (ts_a < ts_b) return -1;
  if (ts_a > ts_b) return 1;
  // Fallback to title comparison for consistency when switching
  // between the two; we don't want rows swapping unnecessarily
  return title_comparator(a, b);
}

function sort(select) {
  var children = Array.from(body.children);
  if (select === "alpha") {
    children = children.sort(title_comparator);
  } else if (select === "recent") {
    children = children.sort(time_comparator);
  }
  for (i = 0; i < children.length; ++i) {
    body.appendChild(children[i]);
  }
}

function search(target) {
  var children = Array.from(body.children);

  function has_substr(child, ss) {
    var title =
      child
      .getElementsByClassName("repo-title")[0]
      .textContent
      .toLowerCase()
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
  var n_repositories_header = head.firstChild.firstChild.firstChild;
  n_repositories_header.textContent = `Repositories (${n_visible})`;
}

var head = null;
var body = null;
