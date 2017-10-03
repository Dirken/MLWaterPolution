$(document).ready(setTimeout(function() {
  document.getElementById('searchId').onchange = function(e){
    var hot_instance = HTMLWidgets.getInstance(hot).hot
    var aux = document.getElementById('searchId').value;
    hot_instance.search.query(aux);
    hot_instance.render();
  }
}))

/*
$(document).ready(function() {
    var colsToHide = [];

    function updateSettings() {
        var newCols = [];
        var newHeaders = [];

        for (var i = 0; i < globalColumns.length; i++) {
            if (colsToHide.indexOf(i) === -1) {
                newCols.push(globalColumns[i]);
                newHeaders.push(globalHeaders[i]);
            }
        }
        hot1.updateSettings({
            columns: newCols,
            colHeaders: newHeaders
        });
    }

    $("#add").on("change", function(e) {
        var val = this.value;
        this.value = "";
        var intVal = parseInt(val);
        if (intVal >= 0) {
            if (colsToHide.indexOf(intVal) === -1) {
                colsToHide.push(intVal);
            }
        }
        updateSettings();

    })

    $("#show").on("change", function(e) {
        var val = this.value;
        this.value = "";
        var intVal = parseInt(val);
        if (intVal >= 0) {
            var indexToRemove = colsToHide.indexOf(intVal);
            if (~indexToRemove) {
                colsToHide.splice(indexToRemove, 1);
            }
        }
        updateSettings();
    })

    var container = document.getElementById("example2");
    var hot1 = new Handsontable(container, {
        data: data,
        startRows: 5,
        minSpareRows: 1,
        colHeaders: globalHeaders,
        columns: globalColumns,
    });
});*/