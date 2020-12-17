// =============================================================== //
// Class definitions for HTML tables of contents

class JournalIssue {

    constructor(key, name, volume, issue, published){
        this.key       = key;
        this.name      = name;
        this.volume    = volume;
        this.issue     = issue;
        this.published = published;
    }

    header(){
        let volNo   = this.volume + ":" + this.issue;
        let pubdate = "(" + this.published + ")";
        if ( volNo.trim() == ":" ){
            return this.name;
        }
        return this.name + " " + volNo + " " + pubdate;
   }

}
