# JSets

## Overview

**JSets** is a tool customized for managing Literature Review in the laboratory where I work.

The basic work flow involves the laboratory following a set of published journals.
The issues of these journals published throughout the year are grouped into *journal sets*.
The journal sets are then reviewed during periodic *Literature Review* meetings.
The review process entails members of the lab selecting articles for review from the journal set.
These articles are then proposed to the boss, who then makes a final selection of approximately a dozen articles for review.
The selected articles are then assigned to those lab members who are scheduled to present at the next Literature Review meeting.
*JSets* streamlines this process and makes everything electronic.

## Literature Review

### Prior to JSets

The Literature Review work flow described above raises a number of logistical questions:

* Which issues of each journal are to be included in each journal set?
* When are all issues in a given journal set expected to be published?
* When should Literature Review meetings be scheduled, and who should present at each meeting?
* How should you collect the tables of contents for all issues in a journal set together to facilitate selecting articles for review?
* How should selections from multiple presenters be collected together and submitted to the boss for review?
* Once a selection is finalized, how should the articles be assigned to the presenters according to their preferences?

Previously, these questions were addressed in a somewhat haphazard way.
Some unfortunate person was assigned the task of scheduling the meetings
and keeping track of what issues would be in each journal set.
Furthermore, article selections involved downloading the tables of contents for each issue in the journal set, printing them out and annotating the printouts by hand.
Article assignments were then negotiated between the presenters.
This wasted a lot of time and paper and became a particular problem when many people were forced to work from home in 2020.

*Most importantly*, I became the unfortunate person in charge of all this in 2020.

### After JSets

*JSets* is a simple command line utility that addresses all of the above logistical problems.
Reference issues from all followed journals are collected together in a configuration file.
These reference issues are then used to compute future issues with publication dates usually correct to within a day or two.
This allows all journal sets in a given year to be computed along with their availability dates using the `year` command.
Literature Review meetings and presenters can then be assigned to the journal sets using the `meetings` command.

Article selections for a given journal set are then handled with the `toc` command.
This will use
[PubMed](https://pubmed.ncbi.nlm.nih.gov)
to create an html document with all citations associated with each issue in a journal set.
The html document is interactive and allows you to select articles using checkboxes and also enter unlisted articles via their digital object identifiers (DOI) or PubMed ID (PMID).
The html document can then generate a small text file containing all of your selections,
which can then be sent back to the Literature Review administrator.
These selection files can also be used with the `toc` command to generate a table of contents html file with the selected files highlighted for review by the boss.
The boss then submits the final selection file.

Articles are assigned in a two step process.
First, the `ranks` command is used with the final selection to generate an html file that allows each presenter to rank the selected article in order of preference.
The resulting ranks can then be used to assign the articles to the presenters using the
[Hungarian Algorithm](https://en.wikipedia.org/wiki/Hungarian_algorithm)
so as to maximize their preferences.

All of this is can be done electronically via email correspondence.

## PubMed & Current Limitations

*JSets* works closely with [PubMed](https://pubmed.ncbi.nlm.nih.gov/) to determine what articles are in each issue of a configured journal,
and this is how the `toc` command effectively works.
*JSets* also allows you to submit queries directly to *PubMed* from the command line as well as direct DOI lookups.

However, this also introduces some limitations.
For example, *JSets* will not be able to determine the tables of contents for journal isses that are not properly registered at *PubMed*.
Nevertheless, the `toc` command will allow you to enter URLs to the publisher's website for any issue that returns fewer than the expected number of articles from a *PubMed* query.
Furthermore, *JSets* can also handle articles selected from journals that are not configured as well as articles that are not registered at *PubMed*.

## Getting help

*JSets* has a considerable amount of help documentation available (in case I forget how to use it),
though much of it is still rough.
General help information can be accessed using the `--help/-h` option.
This option will also list all of the available commands and options.
Help for a specific command can be accessed using
```bash
$ jsets help command_name
```
The document `JSets/res/help/jsets.1` is the draft *man-page* for *JSets*.
It can be read from the main repository directory using
```bash
$ man res/help/jsets.1
```

## Installation & configuration

### Installing *JSets*

*JSets* uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).
To clone and build the repository, run the following in your terminal:
```bash
$ clone https://github.com/MWRuszczycky/JSets.git
$ cd JSets
$ stack build
```
To run JSets in the cloned repository without installing, create the alias
```bash
$ alias jsets='stack exec jsets --'
```
To locally install *JSets*, use
```bash
$ stack install
```

### Configuring *JSets*

*JSets* also requires a configuration file.
This file lists the reference issues for the followed journals
as well as the list of presenters.
A sample configuration files is provided in the `res/config/` directory of this repository that you can use as a template.
You can specify a configuration file using the `--config/-c` option,
or you can create a default configuration file `~/.config/jsets/config` in your home directory.

### Updating *JSets*

To update *JSets* to the newest version, change to the *JSets* repository directory and run the following:
```bash
$ git pull origin master
$ stack clean --full
$ stack build
```
You can then install the updated version using `stack install` or run it through *Stack* inside the *JSets* repository with `stack exec jsets --`.

## To Do

* Write installers or better installation instructions.
* Include copying information for binary distributions.
* Write more tests.
