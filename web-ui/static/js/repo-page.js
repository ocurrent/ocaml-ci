// Search

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


// Charts

function clickHandler(evt, els, _chart) {
  if (els.length == 0 || evt.type !== "click") {
    return;
  }
  var i = els[0].index;
  // id = "chart_[repo]"
  var repo = evt.native.target.id.substring(6);
  var commit_link = chart_links[repo][i];
  window.location = commit_link;
  // Prevents the main repo link from being followed
  evt.preventDefault();
}

const chartOptions = {
  maintainAspectRatio: false,
  onClick: clickHandler,
  plugins: {
    legend: {
      display: false,
    },
    tooltip: {
      enabled: false,
    }
  },
  elements: {
    point: {
      radius: 0,
    },
  },
  scales: {
    x: {
      grid: {
        display: false,
        drawTicks: false,
      },
      ticks: {
        display: false,
      },
    },
    y: {
      grid: {
        display: false,
        drawTicks: false,
      },
      ticks: {
        display: false,
      },
    },
  },
};

function getRepoNames() {
  var children = Array.from(body.children);
  var names = [];
  for (const child of children) {
    names.push(child
      .getElementsByClassName("repo-title")[0]
      .textContent);
  }
  return names;
}

// Chart data is defined with inline <script> tags in TyXML
function charts_init() {
  var repos = getRepoNames();
  charts = {};
  for (const repo of repos) {
    var data = chart_data[repo];
    if (data.length < 15) {
      var padding = Array.from({
          length: (15 - data.length)
        },
        (_v, _i) => 0);
      data = padding.concat(data);
    }
    var colours = chart_colours[repo];
    if (colours.length < 15) {
      var padding = Array.from({
          length: (15 - colours.length)
        },
        (_v, _i) => "rgba(226, 232, 240, 1)");
      colours = padding.concat(colours);
    }
    var ctx = document.getElementById("chart_" + repo).getContext("2d");
    charts[repo] =
      new Chart(
        ctx, {
          type: "bar",
          data: {
            barThickness: "flex",
            labels: chart_labels,
            datasets: [{
              data: data,
              backgroundColor: colours
            }]
          },
          options: chartOptions
        }
      );
  }
  return charts
}


// onload

window.onload = function() {
  var table_root = document.getElementById("table");
  // table -> thead
  head = table_root.firstChild;
  // table -> tbody
  body = table_root.lastChild;

  charts_init();
}
