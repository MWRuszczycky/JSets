// =============================================================== //
// Function definitions for HTML tables of contents

// --------------------------------------------------------------- //
function isAlphaNum(x) {
// Determine if character is alpha-numeric.
    if (x.length === 1) {
        var v     = x.charCodeAt(0);
        var isNum = (v > 47 && v < 58);
        var isCap = (v > 64 && v < 91);
        var isLow = (v > 96 && v < 123);
        return isNum || isCap || isLow;
    };
    return false;
};

// --------------------------------------------------------------- // 
function makeFileName(prefix, xs) {
// Generate a file name for the selection.
    var suffix = "";
    for (i = 0; i < xs.length; i++) {
        if (isAlphaNum(xs[i])) {
            suffix += xs[i];
        };
    };
    if (suffix) {
        return prefix + "-" + suffix + ".txt";
    };
    return prefix + "-" + "none.txt";
};

// --------------------------------------------------------------- // 
function createSelection() {
// Create selection content for saving and update page.
    var selection = jsetHeader + "\n";
    var count     = 0;
    for (i = 0; i < issues.length; i++ ){
        selection += issues[i].header() + "\n";
        var xs = document.getElementsByClassName(issues[i].key);
        var pmids = ""
        for (j = 0; j < xs.length; j++){
            if (xs[j].checked) {
                pmids += "    " + xs[j].id + "\n";
                count += 1;
            };
        };
        selection += pmids;
    };

    var inst          = document.getElementById('inst');
    var saveLink      = document.getElementById('saveLink');
    var createWgt     = document.getElementById('createWgt');
    var initials      = document.getElementById('initials');
    var fileNameWgt   = document.getElementById('fileNameWgt');
    var countWgt      = document.getElementById('count');
    var payload       = encodeURIComponent(selection);
    var fileName      = makeFileName(savePrefix, initials.value);

    saveLink.href     = "data:text/plain;charset=utf-8," + payload;
    saveLink.download = fileName;

    fileNameWgt.innerHTML = fileName;
    countWgt.innerHTML    = count;
    inst.style            = "display:block";
    createWgt.style       = "display:none";
};
