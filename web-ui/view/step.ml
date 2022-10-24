let title_card ~status ~card_title ~hash_link ~created_at ~finished_at
    ~queued_for ~ran_for ~button =
  let rebuild_button = Option.value ~default:(Tyxml.Html.div []) button in
  Tyxml.Html.(
    div
      ~a:[ a_class [ "justify-between items-center flex" ] ]
      [
        div
          ~a:[ a_class [ "flex flex-col space-y-6" ] ]
          [
            div
              ~a:[ a_class [ "flex items-center space-x-4" ] ]
              [
                div ~a:[ a_id "step-status" ] [ Common.status_icon status ];
                div
                  ~a:[ a_class [ "flex flex-col space-y-1" ] ]
                  [
                    div
                      ~a:[ a_class [ "flex items-baseline space-x-2" ] ]
                      [
                        h1 ~a:[ a_class [ "text-xl" ] ] [ txt card_title ];
                        (* TODO: Breakdown by OS, Compiler and Opam
                           <div class="text-sm font-normal text-gray-500">
                             OS: debian-11 - Compiler: 4.14+flambda - Opam: 2.1
                           </div>
                        *)
                      ];
                    div
                      ~a:[ a_class [ "text-gray-500" ] ]
                      [
                        div
                          ~a:[ a_class [ "flex text-sm space-x-2" ] ]
                          [
                            div
                              ~a:[ a_id "step-created-at" ]
                              [ txt @@ Fmt.str "Created at: %s" created_at ];
                            div [ txt "-" ];
                            div
                              ~a:[ a_id "step-queued-for" ]
                              [ txt @@ Fmt.str "%s in queue" queued_for ];
                            div [ txt "-" ];
                            div
                              ~a:[ a_id "step-finished-at" ]
                              [ txt @@ Fmt.str "Finished at: %s" finished_at ];
                            div [ txt "-" ];
                            div [ hash_link ];
                          ];
                      ];
                  ];
              ];
          ];
        div
          ~a:[ a_class [ "flex items-center justify-between space-x-4" ] ]
          [
            div
              ~a:[ a_id "step-ran-for"; a_class [ "text-sm" ] ]
              [ txt @@ Fmt.str "Ran for %s" ran_for ];
            rebuild_button;
          ];
      ])

let log_highlight_js =
  Tyxml.Html.script ~a:[]
    (Tyxml.Html.Unsafe.data
       {|
document.addEventListener('alpine:init', () => {
    console.log("alpine init");
    Alpine.data('codeLink', () => ({
        permalinkButton: false,

        copyCode(e) {
          this.linkCopied = true;
          const index = this.url.indexOf("#");

          if (index >= 0) {
            this.url = this.url.substring(0, index);
          }

          if (this.endingLine) {
            this.url += `#L${this.startingLine}-${this.endingLine}`;
          } else {
            this.url += `#L${this.startingLine}`;
          }

          location.href = this.url;
          navigator.clipboard.writeText(this.url);
          // this.$clipboard(this.url);
          this.manualSelection = false;
        },

        positionCopyButton(e) {
            this.$refs.copyLinkBtn.style.top = `${e.layerY-15}px`;
        },

        highlightLine(e) {
            if (e) {
              const currentLine = e.target.parentNode.id;
              const currentID = parseInt(currentLine.substring(1, currentLine.length));
              this.manualSelection = true;
              this.positionCopyButton(e);

              if (!this.startingLine) {
                  this.startingLine = currentID;
                  this.endingLine = currentID;
                  console.log(this.startingLine);
              }

              if (this.startingLine) {
                  if (e.shiftKey) {
                      if (currentID > this.startingLine) {
                          this.endingLine = currentID;
                      } else if (currentID < this.startingLine) {
                          this.endingLine = this.startingLine;
                          this.startingLine = currentID;
                      } else {
                          this.startingLine = currentID;
                          this.endingLine = currentID;
                      }
                  } else {
                      this.startingLine = currentID;
                      this.endingLine = currentID;
                      this.linkCopied = false;
                  }
              }
            } else {
              const index = this.url.indexOf("#")+2;

              if (index >= 0) {
                const lines = this.url.substring(index, this.url.length);
                const lineNumbers = lines.split("-");
                this.startingLine = parseInt(lineNumbers[0]);
                this.endingLine = parseInt(lineNumbers[1]);
              }

              if (this.startingLine) {
                setTimeout(() => {
                  console.log(this.startingLine);
                  document.getElementById(`L${this.startingLine}`).scrollIntoView();
                }, 500)
              }
            }
        }
    }))
})

|})

let poll =
  Tyxml.Html.script ~a:[]
    (Tyxml.Html.Unsafe.data
       {|
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
              element_created_at.innerHTML = "Created at: " + data["created_at"];
              const element_finished_at = document.getElementById("step-finished-at");
              element_finished_at.innerHTML = "Finished at: " + data["finished_at"];
              const element_ran_for = document.getElementById("step-ran-for");
              element_ran_for.innerHTML = "Ran for: " + data["ran_for"];
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
                };
                if (data["can_rebuild"]) {
                  document
                    .getElementById("rebuild-step")
                    .style.removeProperty("display");
                };
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
|})
