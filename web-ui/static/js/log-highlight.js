document.addEventListener('alpine:init', () => {
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
            var currentLine = e.target.dataset.lineNumber;
            if (currentLine == undefined) {
              currentLine = e.target.parentNode.dataset.lineNumber;
            }
            const currentID = parseInt(currentLine.substring(1, currentLine.length));
            this.manualSelection = true;
            this.positionCopyButton(e);

            if (!this.startingLine) {
              this.startingLine = currentID;
              this.endingLine = currentID;
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
                document.getElementById(`L${this.startingLine}`).scrollIntoView();
              }, 500)
            }
          }
        }
    }))
})