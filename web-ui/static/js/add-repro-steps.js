function extractStepsToReproduce() {
  const tables = document.getElementsByClassName("steps-table");
  const start = document.getElementsByClassName("repro-block-start");
  const finish = document.getElementsByClassName("repro-block-end");
  if ((start.length === 0) || (finish.length === 0) || (tables.length === 0)) {
    document.getElementById("build-repro-container").style.display = 'none';
    return null
  };
  const startIndex = parseInt(start[0].parentElement.id.split('L')[1], 10);
  const finishIndex = parseInt(finish[0].parentElement.id.split('L')[1], 10);

  const br = document.createElement("br");
  const reproDiv = document.getElementById("build-repro");
  const rows = [...tables].map((t) => Array.from(t.children)).flat(1);

  document.getElementById("build-repro-container").style.removeProperty('display');

  for (let i = (startIndex); i < (finishIndex - 1); i++) {
    // Remove line-number, whitespace and possible newlines from the row
    const newContent = document.createTextNode(rows[i].innerText.replace(/\s*\n?/, ''));
    reproDiv.appendChild(newContent);
    if (i > startIndex) {
      reproDiv.appendChild(br.cloneNode());
    }
  }
}

extractStepsToReproduce();
