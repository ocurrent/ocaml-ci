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

const chartOptions = {
  maintainAspectRatio: false,
  legend: {
    display: false
  },
  tooltips: {
    // Hide the on-canvas tooltip
    enabled: false,
    custom: function(tooltipModel) {
      // Tooltip Element
      var tooltipEl = document.getElementById('chartjs-tooltip');

      // Create element on first render
      if (!tooltipEl) {
          tooltipEl = document.createElement('div');
          tooltipEl.id = 'chartjs-tooltip';
          tooltipEl.innerHTML = "";
          // tooltipEl.innerHTML = '<table></table>';
          document.body.appendChild(tooltipEl);
      }

      // Hide if no tooltip
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

      // Set Text
      if (tooltipModel.body) {
          // var titleLines = tooltipModel.title || [];
          var bodyLines = tooltipModel.body.map(getBody);


          // console.log(titleLines);
          // console.log(bodyLines);

          // var innerHtml = '<thead>';

          // titleLines.forEach(function(title) {
          //     innerHtml += '<tr><th>' + title + '</th></tr>';
          // });
          // innerHtml += '</thead><tbody>';

          // bodyLines.forEach(function(body, i) {
          //     var colors = tooltipModel.labelColors[i];
          //     var style = 'background:' + colors.backgroundColor;
          //     style += '; border-color:' + colors.borderColor;
          //     style += '; border-width: 2px';
          //     var span = '<span style="' + style + '"></span>';
          //     innerHtml += '<tr><td>' + span + body + '</td></tr>';
          // });
          // innerHtml += '</tbody>';

          innerHtml = "<p>" + bodyLines[0][0] + "</p>";

          // var tableRoot = tooltipEl.querySelector('table');
          tooltipEl.innerHTML = innerHtml;
      }

      // `this` will be the overall tooltip
      var position = this._chart.canvas.getBoundingClientRect();

      // Display, position, and set styles for font
      tooltipEl.style.opacity = 1;
      tooltipEl.style.position = 'absolute';
      tooltipEl.style.left = position.left + window.pageXOffset + tooltipModel.caretX + 'px';
      tooltipEl.style.top = position.top + window.pageYOffset + tooltipModel.caretY + 'px';
      tooltipEl.style.fontFamily = tooltipModel._bodyFontFamily;
      tooltipEl.style.fontSize = tooltipModel.bodyFontSize + 'px';
      tooltipEl.style.fontStyle = tooltipModel._bodyFontStyle;
      tooltipEl.style.padding = tooltipModel.yPadding + 'px ' + tooltipModel.xPadding + 'px';
      tooltipEl.style.pointerEvents = 'none';
  },
  },
  elements: {
    point: {
      radius: 0
    }
  },
  scales: {
    xAxes: [{
      gridLines: false,
      scaleLabel: false,
      ticks: {
        display: false
      }
    }],
    yAxes: [{
      gridLines: false,
      scaleLabel: false,
      ticks: {
        display: false,
        suggestedMin: 0,
        suggestedMax: 10
      },
    }],
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
