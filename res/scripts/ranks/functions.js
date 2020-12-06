// =============================================================== //
// Function definitions for HTML rank-lists

// --------------------------------------------------------------- // 
function isAnInt(x) {
    let intOK = !Number.isNaN(parseInt(x));
    let numOK = !Number.isNaN(x);
    return intOK && numOK;
}

// --------------------------------------------------------------- // 
function compareRanks(r1, r2) {
    return r1[1] - r2[1];
}

// --------------------------------------------------------------- // 
function formatRanks(rs) {
    if (rs.length === 0) {
        return "<br>I have no preferences.";
    }

    str = "<br>most preferred : ";

    for (i = 0; i < rs.length; i++) {
        if (i === rs.length - 1) {
            str += rs[i][0] + " : least preferred";
        } else if (rs[i][1] === rs[i + 1][1]) {
            str += rs[i][0] + " = ";
        } else {
            str += rs[i][0] + " > ";
        }
    }
    return str + "<br><br><br>";
}

// --------------------------------------------------------------- // 
function createRanks() {
// Create ranks and update page
    let xs    = document.getElementsByClassName("_citation");
    let ranks = [];
    for (i = 0; i < xs.length; i++) {
        let v = xs[i].value;
        if (isAnInt(v)) {
            ranks.push([i + 1, Number(v)]);
        }
    }

    ranks.sort(compareRanks);

    let inst     = document.getElementById('inst');
    let rankList = document.getElementById('ranks');
    let rankBtn  = document.getElementById('rankBtn');

    rankList.innerHTML = formatRanks(ranks);
    rankBtn.style      = "display:none";
}
