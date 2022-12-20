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
  var commit_link = chart_links[repo][i + 1];
  window.location = commit_link;
}

function tooltipHandler(context) {
  // Tooltip Element
  let tooltipEl = document.getElementById('chartjs-tooltip');

  // Create element on first render
  if (!tooltipEl) {
    tooltipEl = document.createElement('div');
    tooltipEl.id = 'chartjs-tooltip';
    tooltipEl.innerHTML = '<div class=\"border \
      border-gray-200 dark:border-gray-400 border-t-0 \
      rounded-lg w-full min-w-0 \
      bg-white dark:bg-gray-850 \
      px-3 py-1 \
      shadow-sm \
      \"></div>';
    document.body.appendChild(tooltipEl);
  }

  // Hide if no tooltip
  const tooltipModel = context.tooltip;
  if (tooltipModel.opacity === 0) {
    tooltipEl.style.opacity = 0;
    return;
  }

  // Set caret Position
  tooltipEl.classList.remove('above', 'below', 'no-transform');
  if (tooltipModel.yAlign) {
    tooltipEl.classList.add(tooltipModel.yAlign);
  } else {
    tooltipEl.classList.add('no-transform');
  }

  function getBody(bodyItem) {
    return bodyItem.lines;
  }

  function textOfSpeed(speed) {
    const s = parseFloat(speed);
    if (s >= 3600.0) {
      return (s / 3600.0) + '<span class="pl-0.5">hr</span>'
    } else if (s >= 60.0) {
      return (s / 60.0) + '<span class="pl-0.5">min</span>'
    }
    return s + '<span class="pl-0.5">sec</span>'
  }

  // Set Text
  if (tooltipModel.body) {
    const titleLines = tooltipModel.title || [];
    const bodyLines = tooltipModel.body.map(getBody);
    const repo = context.chart.canvas.id.substring(6);

    const commit_hash = chart_links[repo][parseInt(titleLines[0])].split("/")[5];
    const short_hash = commit_hash.substring(0, 6);

    let innerHtml = '<div class="font-medium text-sm bg-gray-50 dark:bg-gray-900">' + short_hash + '</div>'

    const speed = bodyLines[0];
    innerHtml += '<div class="text-xs py-1">' + textOfSpeed(speed) + '</div>'

    tooltipEl.firstChild.innerHTML = innerHtml;
  }

  const position = context.chart.canvas.getBoundingClientRect();
  const bodyFont = Chart.helpers.toFont(tooltipModel.options.bodyFont);

  // Display, position, and set styles for font
  tooltipEl.style.opacity = 1;
  tooltipEl.style.position = 'absolute';
  tooltipEl.style.left = position.left + window.pageXOffset + tooltipModel.caretX + 'px';
  tooltipEl.style.top = position.top + window.pageYOffset + tooltipModel.caretY + 'px';
  tooltipEl.style.font = bodyFont.string;
  tooltipEl.style.padding = tooltipModel.padding + 'px ' + tooltipModel.padding + 'px';
  tooltipEl.style.pointerEvents = 'none';
}

const chartOptions = {
  maintainAspectRatio: false,
  onClick: clickHandler,
  plugins: {
    legend: {
      display: false,
    },
    tooltip: {
      // Disable the on-canvas tooltip
      enabled: false,
      external: tooltipHandler,
    },
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
