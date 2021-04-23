window.MyJsLib = {
    focus: function (elt) {
        console.log("try focus ", elt)
        elt.focus();
    },

    unfocus: function (elt) {
        console.log("try unfocus ", elt)
        elt.blur();
    },

    newPlot: function (id, traces = [], layout = {}) {
        Plotly.newPlot(id, traces, layout);
    }
};
