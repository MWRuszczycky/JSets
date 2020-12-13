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
    }
    return false;
}

// --------------------------------------------------------------- // 
function makeFileName(prefix, xs) {
// Generate a file name for the selection.
    let suffix = "";
    for (let i = 0; i < xs.length; i++) {
        if (isAlphaNum(xs[i])) {
            suffix += xs[i];
        }
    }
    if (suffix) {
        return prefix + "-" + suffix + ".txt";
    }
    return prefix + "-" + "none.txt";
}

// --------------------------------------------------------------- //
function userCitationLocator(citeId) {

    let refType = document.getElementById(citeId + "-refType").value;
    let ref     = document.getElementById(citeId + "-ref").value.trim();
    let url     = "https://";

    if (ref.length == 0) {
        return { url: "", refType: refType, valid: false };
    }

    switch(refType) {
        case "PMID":
            url += "pubmed.ncbi.nlm.nih.gov/" + ref;
            break;
        case "DOI":
            ref = ref.replace(/.*doi\.org\//,"");
            url += "www.doi.org/" + ref;
            break;
        case "WEB":
            ref = ref.replace("https://","")
            url += ref;
            break;
    } // switch on refType

    return { url: url, refType: refType, ref: ref, valid: true };

}

// --------------------------------------------------------------- //
function userCitationHTML(citeId, citeClass, index) {

    let indexStr = index.toString();
    let content  =
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
                <option value="DOI">DOI</option>
                <option value="WEB">WEB</option>
                <option value="PMID">PMID</option>
            </select>
            <label for="${citeId}-ref">:</label>
            <input type="text" id="${citeId}-ref"
                   size="60" class="txtbox">
         </p>`;

    return content;

}

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

}

// ------------------------------------------------------------------
function remUserCitation(citeId, citeClass) {
// Delete a user-added citation and rename the remaining user-
// citations so they remain sequentially numbered. Note that the
// numbers associated with the user-citation names are unrelated to
// their ids, which remain unchanged after creation.

    let toRemove = document.getElementById(citeId);
    toRemove.remove();

    let xs = document.getElementsByClassName(citeClass + "-name");
    for (let i = 0; i < xs.length; i++) {
        xs[i].innerHTML = "Article " + (i+1).toString();
    }

}

// ------------------------------------------------------------------
function checkUserCitation(citeId) {
// Attempt to open a link to a user-specified citation in a new tab.

    let locator = userCitationLocator(citeId);

    if (!locator.valid) {
        alert("A " + locator.refType + " identifier is must be provided!");
        return;
    }

    window.open(locator.url, "_blank");

}

function readUserCitations(citeClass) {
// Read article selections for user-specified citations.

    let citations = document.getElementsByClassName(citeClass);
    let selected  = [];

    for (let i = 0; i < citations.length; i++) {

        let locator = userCitationLocator(citations[i].id);

        if (!locator.valid) {
            alert( "There is an article without a locator.\n"
                   + "It will not be recorded." );
            continue;
        }

        let prefix = locator.refType.toLowerCase() + ": ";
        if (locator.refType == "PMID") {
            prefix = "";
        }

        selected[selected.length] = prefix + locator.ref;

    }

    return selected;

}

// --------------------------------------------------------------- //
function readSelection() {
// Read the user's selections.

    let selection = jsetHeader + "\n";
    let count     = 0;

    // Get articles from configured issues
    for (let i = 0; i < issues.length; i++ ) {

        selection += issues[i].header() + "\n";

        // Get articles indexed at PubMed for the issue
        let key       = issues[i].key;
        let citations = document.getElementsByClassName(key);

        for (let j = 0; j < citations.length; j++) {
            if (citations[j].checked) {
                selection += "    " + citations[j].id + "\n";
                count     += 1;
            }
        }

        // Get articles not indexed at PubMed for the issue
        let userCitations = readUserCitations(key + "-Add");
        count += userCitations.length;
        for (let j = 0; j < userCitations.length; j++) {
            selection += "    " + userCitations[j] + "\n";
        }
    }

    // Get extra articles from non-configured issues
    let extras = readUserCitations("user-citation");

    selection += "Extra-Citations\n";
    count     += extras.length;
    for (let i = 0; i < extras.length; i++) {
        selection += "    " + extras[i] + "\n";
    }

    return { listing: selection, count: count };

}

// --------------------------------------------------------------- // 
function createSelection() {
// Create selection content for saving and update page.

    let saveWgt     = document.getElementById('saveWgt');
    let saveLink    = document.getElementById('saveLink');
    let createWgt   = document.getElementById('createWgt');
    let initials    = document.getElementById('initials');
    let fileNameWgt = document.getElementById('fileNameWgt');
    let countWgt    = document.getElementById('count');
    let selection   = readSelection();
    let payload     = encodeURIComponent(selection.listing);
    let fileName    = makeFileName(savePrefix, initials.value);

    saveLink.href         = "data:text/plain;charset=utf-8," + payload;
    saveLink.download     = fileName;
    fileNameWgt.innerHTML = fileName;
    countWgt.innerHTML    = selection.count;
    saveWgt.style         = "display:block";
    createWgt.style       = "display:none";

    console.log(selection.listing);
}

// --------------------------------------------------------------- // 
function resetSelection() {
// Reset selection without deleting anything the user has entered.

   let saveLink    = document.getElementById('saveLink');
   let countWgt    = document.getElementById('count');
   let fileNameWgt = document.getElementById('fileNameWgt');

   saveLink.href         = "";
   saveLink.download     = "";
   fileNameWgt.innerHTML = "";
   countWgt.innerHTML    = "";
   saveWgt.style         = "display:none";
   createWgt.style       = "display.block;"

}
