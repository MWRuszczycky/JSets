# JSets

## Overview

*JSets* is a tool customized for managing literature review in the laboratory where I work.

The basic work flow involves the laboratory following a set collection of journals that may change periodically. Every other week a selection of articles from a set of issues from these journals is reviewed and discussed in *Literature Review*. *JSets* helps in maintaining the set of issues and associated articles that will be reviewed during each *Literature Review*.

The journals that are followed are specified in a configuration file. Each journal must be specified with a reference issue and a publication frequency from which all future issues can be computed.

A set of all issues for review in a given *Literature Review* is called a *journal set* and the corresponding selection of articles for review is called a *selection*. A collection of 26 journal sets (i.e., every two weeks) for an entire year can be constructed using the `year` command. You can also specify journal set frequencies other than every two weeks. Once constructed, this collection can then be edited, saved and used for the remainder of the year. Each journal set is dated according to when all issues in the set are expected to be published.

Given a collection of journal sets, a selection is created and assigned to the graduate students for review during *Literature Review* according to the following work flow:
1. A table of contents `html` document is created for the journal set using the `toc` command and sent to those students who will be presenting. This document allows the students to propose their own selections for review.
2. The proposed selections are collected and used to construct another table of contents `html` document with the articles proposed for review highlighted. This is again done using the `toc` command with the selection files returned by the students. The resulting document is sent to the boss. The boss then makes his own selection, which is final.
3. The boss's selection is used to generate an html document for ranking the articles selected for review using the `ranks` command. This document is then sent to the students, who use it to indicate their preference for reviewing each article in the selection.
4. The preferences are used by the `match` command to assign the articles to each student for presentation during the *Literature Review* meeting. This attempts to match each student to the articles according to their preferences using the *Hungarian Algorithm*.

### PubMed

*JSets* works closely with [PubMed](https://pubmed.ncbi.nlm.nih.gov/) to determine what articles are in each issue of a configured journal. Therefore, the *JSets* features will not all work with journals that are not tracked by *PubMed*. However, *JSets* *does not* require that journal articles also be available at [PubMed Central](https://www.ncbi.nlm.nih.gov/pmc/). I have also found that every once in a while a journal issue does not appear to be consistently indexed at *PubMed* so that it cannot be accessed using the standard search parameters that *JSets* uses. Consequently, *JSets* will fail to find such issue contents; however, when this happens you have the option to enter a URL directly to the table of contents at the publisher's website.

## Getting help

*JSets* has a considerable amount of help documentation available (in case I forget how to use it). General help information can be accessed using the `--help/-h` option. This option will also list all of the available commands and options. Help for a specific command can be accessed using
```sh
jsets help command_name
```

## Installation & configuration

### Installing *JSets*

*JSets* uses the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). To clone and build the repository, run the following in your terminal:
```sh
clone https://github.com/MWRuszczycky/JSets.git
cd JSets
stack build
```
To run JSets in the cloned repository without installing, create the alias
```
alias jsets='stack exec jsets --'
```
To locally install *JSets*, use
```sh
stack install
```

### Configuring *JSets*

*JSets* also requires a configuration file be provided that lists the journals that are reviewed during each *Literature Review*. A sample configuration files is provided in the `res/config/` that you can use as a template. You can specify a configuration file using the `--config/-c` option or you can create it as the `~/.config/jsets/config` file in your home directory.

### Updating *JSets*

To update *JSets* to the newest version, change to the *JSets* repository directory and run the following:
```sh
git pull origin master
stack clean --full
stack build
```
You can then install the updated version usinge `stack install` or run it through *Stack* inside the *JSets* repository with `stack exec jsets --`.

## To Do

* Write a man page.
