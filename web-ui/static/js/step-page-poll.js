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

  var checkCondition = function (resolve, reject) {
    // If the condition is met, we're done!
    fetch(api_path)
      .then((response) => response.json())
      .then((data) => {
        console.log(data);

        const element_created_at = document.getElementById("step-created-at");
        element_created_at.innerHTML = "Created at " + data["created_at"];
        const element_finished_at = document.getElementById("step-finished-at");
        element_finished_at.innerHTML = "Finished at " + data["finished_at"];
        const element_ran_for = document.getElementById("step-ran-for");
        element_ran_for.innerHTML = "Ran for " + data["ran_for"];
        const element_queued_for = document.getElementById("step-queued-for");
        element_queued_for.innerHTML = data["queued_for"] + " in queue";

        if (
          data["status"].startsWith("passed") ||
          data["status"].startsWith("failed")
        ) {
          const element_step_status = document.getElementById("step-status");
          if (data["status"].startsWith("passed")) {
            element_step_status.innerHTML = iconSuccess;
          } else {
            element_step_status.innerHTML = iconFailed;
          }
          if (data["can_rebuild"]) {
            document
              .getElementById("rebuild-step")
              .style.removeProperty("display");
          }
          console.log("Build has finished. Stop polling.");
        }

        // If the condition isn't met but the timeout hasn't elapsed, go again
        else if (Number(new Date()) < endTime) {
          setTimeout(checkCondition, interval, resolve, reject);
        }
        // Didn't match and too much time, reject!
        else {
          reject(new Error("timed out for " + api_path + ": " + arguments));
        }
      });
  };

  return new Promise(checkCondition);
}

// Usage:  ensure element is visible
poll(location.origin + "/api" + location.pathname);
