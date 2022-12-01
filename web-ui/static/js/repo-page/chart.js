
const chartOptions = {
  maintainAspectRatio: false,
  legend: {
    display: false
  },
  tooltips: {
    enabled: false
  },
  elements: {
    point: {
      radius: 0
    }
  },
  scales: {
    xAxes: [
      {
        gridLines: false,
        scaleLabel: false,
        ticks: {
          display: false
        }
      }
    ],
    yAxes: [
      {
        gridLines: false,
        scaleLabel: false,
        ticks: {
          display: false,
          suggestedMin: 0,
          suggestedMax: 10
        }
      }
    ]
  },
  animation: {
    duration: 0
  }
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

const green = "rgba(18, 183, 106, 1)"
const red = "rgba(217, 45, 32, 1)"
const grey = "rgba(226, 232, 240, 1)"

function statusToColor(status) {
  if (status.startsWith("passed")) {
    return green
  } else if (status.startsWith("failed")) {
    return red
  }
  return grey
}

function charts_init() {
  var repos = getRepoNames();
  charts = {};
  for (const repo of repos) {
    var ctx = document.getElementById("chart_" + repo).getContext("2d");
    charts[repo] =
      new Chart(
        ctx,
        {
          type: "bar",
          data: {
            barThickness: "flex",
            labels: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
            datasets: [
              {
                data: Array(15).fill(15),
                backgroundColor: Array(15).fill(grey)
              }
            ]
          },
          options: chartOptions
        }
      )
  }
  return charts
}

function parseDataSet(history) {
  var data = [];
  var backgroundColor = [];
  for (const v of history) {
    data.push(v["ran_for"]);
    backgroundColor.push(statusToColor(v["status"]));
  }
  return [
    { data: data, backgroundColor: backgroundColor }
  ]
}

/*

JSON format:
{
  chart: {
    repo: [
      { ran_for, status },
      { ran_for, status },
      { ran_for, status },
      { ran_for, status },
      { ran_for, status },
      ...
    ]
    ...
  }
}
*/

function charts_poll(api_path, charts) {
  var endTime = Number(new Date()) + (2700000); // 45min timeout
  var interval = 60000; // 60s

  var checkCondition = function (resolve, reject) {
    // If the condition is met, we're done!
    fetch(api_path)
      .then((response) => response.json())
      .then((data) => {
        // data = [
        //     name: "cavalry",
        //     history: [
        //       { ran_for: 10, status: "passed" },
        //       { ran_for: 5, status: "passed" },
        //       { ran_for: 10, status: "passed" },
        //       { ran_for: 15, status: "passed" },
        //       { ran_for: 10, status: "passed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 2, status: "failed" },
        //       { ran_for: 12, status: "" },
        //     ]
        //   ]
        // };

        for (const repo of data) {
          var chart = charts[repo["name"]];
          chart.data.datasets = parseDataSet(repo["history"]);
          chart.update();
        }
        if (Number(new Date()) < endTime) {
          setTimeout(checkCondition, interval, resolve, reject);
        } else {
          reject(new Error("timed out for " + api_path + ": " + arguments));
        }
      });
  };

  return new Promise(checkCondition);
}

// Usage:  ensure element is visible
// poll(location.origin + "/api" + location.pathname);
