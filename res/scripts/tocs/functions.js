// =============================================================== //
// Function definitions for HTML tables of contents

// --------------------------------------------------------------- //
function isAlphaNum(x) {
// Determine if character is alpha-numeric.
    if (x.length === 1) {
        let v     = x.charCodeAt(0);
        let isNum = (v > 47 && v < 58);
        let isCap = (v > 64 && v < 91);
        let isLow = (v > 96 && v < 123);
        return isNum || isCap || isLow;
    };
    return false;
};

// --------------------------------------------------------------- // 
function makeFileName(prefix, xs) {
// Generate a file name for the selection.
    let suffix = "";
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
function userCitationLocator(citeId) {

    let refType = document.getElementById(citeId + "-refType").value;
    let ref     = document.getElementById(citeId + "-ref").value.trim();
    let url     = "https://";
    let valid   = true;

    if (ref.length == 0) {
        return { url: "", refType: refType, valid: false };
    };

    switch(refType) {
        case "PMID":
            url += "pubmed.ncbi.nlm.nih.gov/" + ref;
            break;
        case "DOI":
            url += "www.doi.org/" + ref;
            break;
        case "LINK":
            url += ref;
            break;
    }; // switch on refType

    return { url: url, refType: refType, valid: valid };

};

// --------------------------------------------------------------- //
function userCitationHTML(citeId, citeClass, index) {

    let indexStr = index.toString();
    let content  = "";

    switch(citeClass) {
        case "user-citation":
            content  =
            `<p id="${citeId}" class="${citeClass}">
                <b><span class="${citeClass}-name">Article ${indexStr}</span></b>
                <button type="button"
                        onClick="checkUserCitation('${citeId}')">
                    Check link</button>
                <button type="button"
                        onClick="remUserCitation('${citeId}','${citeClass}')">
                    Remove</button></br>
                <label for="${citeId}-refType">Address</label>
                <select name="${citeId}-refType" id="${citeId}-refType">
                    <option value="PMID">PMID</option>
                    <option value="DOI">DOI</option>
                    <option value="LINK">LINK</option>
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
                <label for="${citeId}-doi">DOI</label>
                    <input type="text" id="${citeId}-doi"
                           size="15" class="txtbox"
                           style="margin-left: 0.6em;">
             </p>`;
    }; // switch on citeClass

    return content;

};

// ------------------------------------------------------------------
function userCitation(citeClass) {
// Create a new html element for entering a user-citation. Handles
// citations from missing configured issues and extra citations.

    let n = 1;
    let citeId = citeClass + "-" + n.toString();

    while ( document.getElementById(citeId) ) {
        ++n;
        citeId = citeClass + "-" + n.toString();
    }

    let count     = document.getElementsByClassName(citeClass).length
    let citeGroup = document.getElementById(citeClass);
    let userCite  = document.createElement("div");

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

    let toRemove = document.getElementById(citeId);
    toRemove.remove();

    let xs = document.getElementsByClassName(citeClass + "-name");
    for (i = 0; i < xs.length; i++) {
        xs[i].innerHTML = "Article " + (i+1).toString();
    };

};

// ------------------------------------------------------------------
function checkUserCitation(citeId) {
// Attempt to open a link to a user-specified citation in a new tab.

    let locator = userCitationLocator(citeId);

    if (!locator.valid) {
        alert("A " + locator.refType + " identifier is must be provided!");
        return;
    };

    window.open(locator.url, "_blank");

};

// --------------------------------------------------------------- //
function readSelection() {
// Read the user's selections.

    let selection = jsetHeader + "\n";
    let count     = 0;

    for (i = 0; i < issues.length; i++ ) {

        let key       = issues[i].key;
        let selected  = "";

        selection += issues[i].header() + "\n";

        // Get articles selected from configured issues indexed at PubMed
        let citations = document.getElementsByClassName(key);

        for (j = 0; j < citations.length; j++) {
            if (citations[j].checked) {
                selected += "    " + citations[j].id + "\n";
                count    += 1;
            };

        };

        // Get articles selected from configured issues not indexed at PubMed
        let added = document.getElementsByClassName(key + "-Add");

        for (j = 0; j < added.length; j++) {
            let title = document.getElementById(added[j].id + "-title").value;
            let doi   = document.getElementById(added[j].id + "-doi").value;
            let page  = document.getElementById(added[j].id + "-page").value;

            if (title.length + page.length + doi.length > 0) {
                count    += 1;
                selected += "    add: " + title.replace(/\|/g, "<p>")
                                  + "|" + page.replace(/\|/g, "<p>")
                                  + "|" + doi.replace(/\|/g, "<p>") + "\n";
            } else {
                alert( "A citation from " + key + " lacks an identifier\n"
                       + "(title, page or doi). It will not be recorded.");
            };
        };

        selection += selected;
    };

    // Get extra articles from non-configured issues
    let extras = document.getElementsByClassName("user-citation");

    for (i = 0; i < extras.length; i++) {

        let locator = userCitationLocator(extras[i].id);

        if (!locator.valid) {
            alert( "There is an 'extra' article without an identifier.\n"
                   + "It will not be recorded." );
            continue;
        };

        selection += "    " + locator.refType + ": " + locator.url + "\n";
        count     += 1;

    };

    return { listing: selection, count: count };

};

// --------------------------------------------------------------- // 
function createSelection() {
// Create selection content for saving and update page.

    let inst          = document.getElementById('inst');
    let saveLink      = document.getElementById('saveLink');
    let createWgt     = document.getElementById('createWgt');
    let initials      = document.getElementById('initials');
    let fileNameWgt   = document.getElementById('fileNameWgt');
    let countWgt      = document.getElementById('count');

    let selection     = readSelection();
    let payload       = encodeURIComponent(selection.listing);
    let fileName      = makeFileName(savePrefix, initials.value);

    saveLink.href     = "data:text/plain;charset=utf-8," + payload;
    saveLink.download = fileName;

    fileNameWgt.innerHTML = fileName;
    countWgt.innerHTML    = selection.count;
    inst.style            = "display:block";
    createWgt.style       = "display:none";

    console.log(selection.listing);
};
