window.MyJsLib = {
    saveSettings: function(s) {
        console.log("save settings", s)
        localStorage.setItem("NotenTrainerSettings", s);
    },

    loadSettings: function() {
        console.log("load settings")
        const s = localStorage.getItem("NotenTrainerSettings")
        return s == undefined || s == null ? "" : s
    },
};