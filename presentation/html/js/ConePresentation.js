

function getAjaxAsync(ajaxUrl, callback){
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange =
        function() {
            if (xhr.readyState == 4 && xhr.status == 200)
                callback(xhr.responseText);
        };

    console.log ("content request: GET " + ajaxUrl);
    xhr.open("GET", ajaxUrl, true);
    xhr.timeout = 5000;
    xhr.ontimeout =
        function () {
            console.log("getAjaxAsync(): timeout");
        };
    xhr.send();
};

var origPresentation =
    { origScript: []
    , origModel: {}
    , origPrefs: {}
    };

function initCont(){
    console.log ("--> initial data loading complete");
    // console.log ("model: " + JSON.stringify(origPresentation, null, 2));
}

function init(){
    console.log("init() entry")
    var ajaxCalls = 3;
    var receiveAndCont = function (mutatePresentation){
        return function (resp) {
            mutatePresentation(JSON.parse(resp));
            --ajaxCalls;
            if (ajaxCalls == 0)
                initCont();
        }
    };

    getAjaxAsync("script.json", receiveAndCont(function(val){ origPresentation.origScript = val;}));
    getAjaxAsync("model.json", receiveAndCont(function(val){ origPresentation.origModel = val;}));
    getAjaxAsync("prefs/default.json", receiveAndCont(function(val){ origPresentation.origPrefs = val;}));
}
