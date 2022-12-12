function extractStepsToReproduce () {
    const tableRows = document.getElementById("steps-table");
    const start = document.getElementsByClassName("repro-block-start");
    const finish = document.getElementsByClassName("repro-block-end");
    if ((start.length === 0) || (finish.length === 0)) {
        return null
    };
    const startIndex = parseInt(start[0].parentElement.id.split('L')[1], 10);
    const finishIndex = parseInt(finish[0].parentElement.id.split('L')[1], 10);

    const br = document.createElement("br")
    const reproDiv = document.getElementById("build-repro")

    for (let i=(startIndex); i < (finishIndex - 1); i++) {
        // Remove line-number, whitespace and possible newlines from the row
        const newContent = document.createTextNode(tableRows.rows[i].innerText.replace(/\d+\s*\n?/,''))
        reproDiv.appendChild(newContent)
        reproDiv.appendChild(br.cloneNode())
    }
}

extractStepsToReproduce();