// =============================================================== //
// Class definitions for HTML tables tables of contents

class JournalIssue {
    constructor(key, name, volume, issue, published){
        this.key = key;
        this.name = name;
        this.volume = volume;
        this.issue = issue;
        this.published = published;
    };
    header(){
        var volNo   = this.volume + ":" + this.issue;
        var pubdate = "(" + this.published + ")";
        return this.name + " " + volNo + " " + pubdate;
   };
};
