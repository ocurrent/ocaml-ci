function title_comparator(a, b) {
  var title_a = a
    .getElementsByClassName("org-title")[0]
    .textContent.toLowerCase();
  var title_b = b
    .getElementsByClassName("org-title")[0]
    .textContent.toLowerCase();
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
  var selector = document.getElementsByTagName("select");
  var gitForgeSelected = null;
  if (selector.length > 0) {
    if (selector[0].value.localeCompare("all") !== 0) {
      gitForgeSelected = selector[0].value;
    }
  }

  function has_substr(child, ss) {
    var elts = child.getElementsByClassName("org-title");
    if (elts.length > 0) {
      var title = elts[0].textContent.toLowerCase();
      return title.indexOf(ss.toLowerCase()) !== -1;
    } else {
      return false;
    }
  }

  for (i = 0; i < children.length; ++i) {
    if (has_substr(children[i], target)) {
      if (gitForgeSelected) {
        var elts = children[i].getElementsByClassName("data-info");
        if (elts.length > 0) {
          var dataInfo = elts[0].textContent.toLowerCase();
          if (dataInfo.localeCompare(gitForgeSelected) == 0) {
            children[i].style.display = "";
          }
        }
      } else {
        children[i].style.display = "";
      }
    } else {
      children[i].style.display = "none";
    }
  }
}

function filter(target) {
  var children = Array.from(body.children);

  function has_target(child, ss) {
    var elts = child.getElementsByClassName("data-info");
    if (elts.length > 0) {
      var title = elts[0].textContent.toLowerCase();
      return title.indexOf(ss.toLowerCase()) !== -1;
    } else {
      return false;
    }
  }

  if (target.toLowerCase().localeCompare("all") == 0) {
    // make everything visible
    for (i = 0; i < children.length; ++i) {
      children[i].style.display = "";
    }
  } else {
    for (i = 0; i < children.length; ++i) {
      if (has_target(children[i], target)) {
        children[i].style.display = "";
      } else {
        children[i].style.display = "none";
      }
    }
  }
}

window.onload = function () {
  body = document.getElementById("table");
  sort("alpha"); // Sort everything by default

  // apply git-forge filter
  var selector = document.getElementsByTagName("select");
  if (selector.length > 0) {
    filter(selector[0].value)
  }

  document.getElementsByTagName("input")[0].focus()
};
