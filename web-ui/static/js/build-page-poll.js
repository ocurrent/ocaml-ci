function poll(api_path, timeout, interval) {
  var endTime = Number(new Date()) + (timeout || 2700000); // 45min timeout
  interval = interval || 10000; // 10s

  const iconSuccess = `
          <div class="icon-status icon-status--success">
              <svg class="h-4 w-4" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="#12B76A">
                  <path fill-rule="evenodd" d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z" clip-rule="evenodd"></path>
              </svg>
          </div>`;

  const iconFailed = `
          <div class="icon-status icon-status--failed">
              <svg xmlns="http://www.w3.org/2000/svg" class="h-3 w-3" viewBox="0 0 20 20" fill="#D92D20">
                  <path fill-rule="evenodd" d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z" clip-rule="evenodd"></path>
              </svg>
          </div>`;

  const iconActive = `
          <div class="icon-status icon-status--active">
            <div></div>
          </div>
        `;

  const iconQueued = `
          <div class="icon-status icon-status--default">
            <div></div>
          </div>
        `;

  function icon_from(status) {
    if (status.startsWith("not started")) {
      return iconQueued;
    } else if (status.startsWith("active")) {
      return iconActive;
    } else if (status.startsWith("passed")) {
      return iconSuccess;
    } else {
      return iconFailed;
    }
  }

  var checkCondition = function (resolve, reject) {
    // If the build has finished, we're done.
    fetch(api_path)
      .then((response) => response.json())
      .then((data) => {
        const build_created_at = document.getElementById("build-created-at");
        build_created_at.textContent = data["first_created_at"];
        const build_ran_for = document.getElementById("build-ran-for");
        build_ran_for.textContent = data["ran_for"];
        const build_total_run_time = document.getElementById("build-total-run-time")
        build_total_run_time.textContent = "Total build run time " + data["total_ran_for"]
        const build_status = document.getElementById("build-status");
        build_status.innerHTML = icon_from(data["status"]);

        const right_arrow = `
              <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="h-5 w-5 -rotate-90" fill="none" viewBox="0 0 24 24" stroke="currentColor" stroke-width="2px"><path stroke-linecap="round" stroke-linejoin="round" d="M19 9l-7 7-7-7"></path></svg>
              `;

        function build_step_row(step, step_route_prefix) {
          const row = document.createElement("a");
          row.setAttribute("class", "table-row");
          row.setAttribute("id", step["variant"]);

          row.setAttribute("href", step_route_prefix + "/" + step["variant"]);

          const main = document.createElement("div"); // status, title, timestamps etc.
          const step_status = document.createElement("div"); // icon-status
          const step_info = document.createElement("div"); //title, timestamp etc.
          const step_title = document.createElement("div"); // title
          const step_timestamps_durations = document.createElement("div"); // timestamps
          const step_created_at = document.createElement("div");
          const hyphen = document.createElement("div");
          const step_queued_for = document.createElement("div");
          const right_matter = document.createElement("div"); // ran_for, right_arrow
          const step_ran_for = document.createElement("div"); // ran_for
          const right_arrow_elt = document.createElement("div");

          step_title.textContent = step["variant"];
          step_title.setAttribute("class", "text-gray-900 text-sm font-medium");

          step_created_at.textContent = "Created at " + step["created_at"];
          hyphen.textContent = "-";
          step_queued_for.textContent = step["queued_for"] + " in queue";

          step_timestamps_durations.appendChild(step_created_at);
          step_timestamps_durations.appendChild(hyphen);
          step_timestamps_durations.appendChild(step_queued_for);
          step_timestamps_durations.setAttribute(
            "class",
            "flex text-sm space-x-2"
          );

          step_info.appendChild(step_title);
          step_info.appendChild(step_timestamps_durations);
          step_info.setAttribute("class", "flex flex-col");

          step_status.innerHTML = icon_from(step["status"]);
          step_status.setAttribute("id", step["variant"] + "-status");

          main.appendChild(step_status);
          main.appendChild(step_info);
          main.setAttribute("class", "flex items-center space-x-3");

          step_ran_for.textContent = "Ran for " + step["ran_for"];

          right_arrow_elt.innerHTML = right_arrow;
          right_matter.appendChild(step_ran_for);
          right_matter.appendChild(right_arrow_elt);
          right_matter.setAttribute(
            "class",
            "flex text-sm font-normal text-gray-500 space-x-8 items-center"
          );

          row.appendChild(main);
          row.appendChild(right_matter);
          return row;
        }

        const steps_table = document.getElementById("table-container");
        steps_table.replaceChildren(); // clears the table

        // TODO: Clearing the table out and appending each step row
        // is the simplest thing I could do to get started. It seems
        // like it should result in jankiness from the table being completely redrawn.
        // I'm not seeing anything like that though so I am leaving it like this.
        // I was going to do something clever like checking if a step has already
        // been rendered and if so, modifying its data in place.
        for (step of data["steps"]) {
          steps_table.appendChild(
            build_step_row(step, data["step_route_prefix"])
          );
        }

        if (
          data["status"].startsWith("passed") ||
          data["status"].startsWith("failed")
        ) {
          build_ran_for.textContent = "Ran for " + data["ran_for"];
          if (data["can_cancel"]) {
            document
              .getElementById("rebuild-build")
              .style.setProperty("display", "none");
            document
              .getElementById("cancel-build")
              .style.removeProperty("display");
          } else if (data["can_rebuild"]) {
            document
              .getElementById("cancel-build")
              .style.setProperty("display", "none");
            document
              .getElementById("rebuild-build")
              .style.removeProperty("display");
          }
          console.log("Build has finished. Stop polling.");
        }

        // If the build has not yet finished and the timeout hasn't elapsed, go again
        else if (Number(new Date()) < endTime) {
          setTimeout(checkCondition, interval, resolve, reject);
        }
        // Didn't finish and too much time, reject!
        else {
          reject(new Error("timed out for " + api_path + ": " + arguments));
        }
      });
  };

  return new Promise(checkCondition);
}

poll(location.origin + "/api" + location.pathname);
