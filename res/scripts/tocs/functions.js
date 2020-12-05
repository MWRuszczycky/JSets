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

// ------------------------------------------------------------------
function userCitationHTML(citeId, citeClass, index) {

    var indexStr = index.toString();
    var content  = "";

    switch(citeClass) {
        case "user-citation":
            content  =
            `<p id="${citeId}" class="${citeClass}">
                <b><span class="${citeClass}-name">Article ${indexStr}</span></b>
                <button type="button">Check Links</button>
                <button type="button"
                        onClick="remUserCitation('${citeId}','${citeClass}')">
                    Remove</button></br>
                <label for="${citeId}-refType">Address</label>
                <select name="${citeId}-refType" id="${citeId}-refType">
                    <option value="PMID">PMID</option>
                    <option value="DOI">DOI</option>
                    <option value="LINK">https://</option>
                </select>
                <label for="${citeId}-ref">:</label>
                <input type="text" id="${citeId}-ref"
                       size="60" class="txtbox">
             </p>`;
             break;
        default:
            content  =
            `<p id="${citeId}", class="${citeClass}">
                <b><span class="${citeClass}-name">Article ${indexStr}</span></b>
                <button type="button"
                        onClick="remUserCitation('${citeId}', '${citeClass}')">
                    Remove</button></br>
                <label for="${citeId}-title">Title</label>
                    <input type="text" id="${citeId}-title"
                           class="txtbox" size="50">
                <label for="${citeId}-page">Page</label>
                    <input type="text" id="${citeId}-page"
                           size="8" class="txtbox"><br>
                <label for="${citeId}-doi">doi</label>
                    <input type="text" id="${citeId}-doi"
                           size="15" class="txtbox"
                           style="margin-left: 1.0em;">
             </p>`;
    }; // switch on citeClass

    return content;

}; // userCitationHTML

// ------------------------------------------------------------------
function userCitation(citeClass) {

    var n = 1;
    var citeId = citeClass + "-" + n.toString();

    while ( document.getElementById(citeId) ) {
        ++n;
        citeId = citeClass + "-" + n.toString();
    }

    var count     = document.getElementsByClassName(citeClass).length
    var citeGroup = document.getElementById(citeClass);
    var userCite  = document.createElement("div");

console.log("count = ", count);

    userCite.id        = citeId;
    userCite.innerHTML = userCitationHTML(citeId, citeClass, count + 1);

    citeGroup.appendChild(userCite);
};

// ------------------------------------------------------------------
function remUserCitation(citeId, citeClass) {
// Delete a user-added citation and rename the remaining user-
// citations so they remain sequentially numbered. Note that the
// numbers associated with the user-citation names are unrelated to
// their ids, which remain unchanged after creation.

    var toRemove = document.getElementById(citeId);
    toRemove.remove();

    var xs = document.getElementsByClassName(citeClass + "-name");
    for (i = 0; i < xs.length; i++) {
        xs[i].innerHTML = "Article " + (i+1).toString();
    };

};
