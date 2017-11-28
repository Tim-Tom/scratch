"use strict";

(function() {
  var cells = [];
  var constraints = {
    "row": [],
    "column": [],
  };

  var numRows, numColumns;

  cells[-1] = {};

  (function() {
    var i, c;
    numRows = numColumns = 0;
    for (i = 0; i < document.input_data.cells.length; ++i) {
      c = document.input_data.cells[i];
      if (c.row_index > numRows) {
        numRows = c.row_index;
      }
      if (c.col_index > numColumns) {
        numColumns = c.col_index;
      }
      if (!cells[c.row_index]) {
        cells[c.row_index] = {};
      }
      cells[c.row_index][c.col_index] = c;
    }
    numRows++;
    numColumns++;
  })();

  (function() {
    var i, j, c, a;
    for (i = 0; i < document.input_data.constraints.length; ++i) {
      c = document.input_data.constraints[i];
      c.cells = [];
      c.sum = c.possible[0].reduce(function(a, b) { return a + b; }, 0);
      c.name = "Constraint-" + c.type + "-" + c.index_major + "-" + c.index_minor;
      if (c.type === "Row") {
        a = constraints.row;
        for (j = c.start; j <= c.end; ++j) {
          cells[c.index_major][j].row = c;
        }
        if (!cells[c.index_major][c.start - 1]) {
          cells[c.index_major][c.start - 1] = { row: null, col: null };
        }
        cells[c.index_major][c.start - 1].row = c;
      } else if (c.type === "Column") {
        a = constraints.column;
        for (j = c.start; j <= c.end; ++j) {
          cells[j][c.index_major].col = c;
        }
        if (!cells[c.start - 1][c.index_major]) {
          cells[c.start - 1][c.index_major] = { row: null, col: null };
        }
        cells[c.start - 1][c.index_major].col = c;
      } else {
        console.error("Could not determine constraint type", c);
        continue;
      }
      if (!a[c.index_major]) {
        a[c.index_major] = [];
      }
      a[c.index_major][c.index_minor] = c;
    }
  })();

  function buildCell(cell) {
    var tbl, row, data;
    var name, i, n;
    name = "cell-" + cell.row_index + "-" + cell.col_index;
    tbl = document.createElement('table');
    tbl.id = name + "-content";
    tbl.className = "number-set";
    for(n = 1, i = 0; n <= 9; ++n) {
      if (n % 3 === 1) {
        if (row) {
          tbl.appendChild(row);
        }
        row = document.createElement('tr');
        row.className = "number-set-row";
      }
      data = document.createElement('td');
      data.id = name + "-content-element-" + n;
      data.className = "number-set-element";
      data.innerHTML = n;
      if (cell.data[i] === n) {
        ++i;
        data.className += " possible";
      } else {
        data.className += " impossible";        
      }
      row.appendChild(data);
    }
    tbl.appendChild(row);
    return tbl;
  }

  (function() {
    var r, c, cell;
    var table, row, data, inner;
    table = document.getElementById('board');
    for (r = -1; r <= numRows; ++r) {
      row = document.createElement('tr');
      row.className = "row";
      row.id = "row-" + r;
      for (c = -1; c <= numColumns; ++c) {
        data = document.createElement('td');
        data.id = "cell-" + r  + "-" + c;
        data.className = "cell"
        if (cells[r] && cells[r][c]) {
          cell = cells[r][c];
          if (cell.data) {
            data.className += " square";
            data.className += " selector-" + cell.row.name.toLowerCase();
            data.className += " selector-" + cell.col.name.toLowerCase();
            data.appendChild(buildCell(cell));
          } else {
            data.className += " constraint";
            inner = document.createElement("div");
            inner.className = "horz-constraint";
            if (cell.row) {
              inner.innerHTML = cell.row.sum;
            }
            data.appendChild(inner);
            inner = document.createElement("div");
            inner.className = "vert-constraint";
            if (cell.col) {
              inner.innerHTML = cell.col.sum;
            }
            data.appendChild(inner);
          }
        } else {
          data.className += " empty";
        }
        row.appendChild(data);
      }
      table.appendChild(row);
    }
  })();

  (function() {
    var nextBtn = document.getElementById('next-button');
    var currentStep = 0;
    var steps = document.input_data.steps;
    nextBtn.onclick = function() {
      console.log("Performing step %d of %d", currentStep + 1, steps.length, steps[currentStep]);
      if (++currentStep == steps.length) {
        nextBtn.onclick = null;
        nextBtn.disabled = true;
      }
    };
  })();
  
})();
