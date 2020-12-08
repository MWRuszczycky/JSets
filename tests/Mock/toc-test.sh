# toc-test.sh
# Mock html output for the jsets <toc> command
#
# run from JSets main directory with
#    bash tests/Mock/toc-test.sh
#
# The mock toc file will be named toc-mock.html and is not under
# version controlled.
#
# The script includes simulation of a configured journal that is not
# indexed at PubMed. This is done by specifying a journal issue that
# will not be published until 2109 (JACS 231:30) and then replacing
# its links with one that was published in 2009 (JACS 131:30) so that
# all the features for handling missing issues that still have tables
# of contents available at the publisher's website can be tested.

# ==================================================================
# Paths: These are all relative to the JSets repository directory

outDir="tests/Mock/html"
srcSet="${outDir}/set5-test.txt"
result="${outDir}/toc-mock.html"
cssInsert="${outDir}/cssInsert.html"
jsInsert="${outDir}/jsInsert.html"

# ===================================================================
# Functions

checkSetup() {
    if [ ! -f "$1" ]; then
        echo -e "\e[31mCannot find required test file $1\e[0m"
        echo "Aborting."
        exit 1
    fi
}

checkResult() {
    if [ $? = 0 ]; then
        echo -e "\e[32m$1 .. OK\e[0m"
    else
        echo -e "\e[31m$1 .. Failed\e[0m"
        echo "Aborting toc-test.sh"
        exit 1
    fi
}

# ===================================================================
# Make sure everything is set up correctly

if [ ! -d "${outDir}" ]; then
    echo -e "\e[31mCannot find relative output directory ${outDir}\e[0m"
    echo "Make sure you are running (from the JSets main repository directory):"
    echo "    bash tests/Mock/toc-test.sh"
    echo "Aborting."
    exit 1
fi

checkSetup ${srcSet}
checkSetup ${cssInsert}
checkSetup ${jsInsert}

# ===================================================================
# Script

echo -e "Running toc-test.sh to generate mock <jsets toc html> output.."
echo -e "   Note that the missing JACS 231:30 issue will be swapped with JACS 131:30"
echo -e "   to test the missing issue features of the html document.\n"
echo -e "\e[38;5;214mRunning command: \e[0m"
echo -e  "  stack exec jsets -- toc ${srcSet} --out=${result}\n"
echo -e "\e[38;5;214mEnter the following link for the missing JACS article:\e[0m"
echo -e "    pubs.acs.org/toc/jacsat/131/30\n"

stack exec jsets -- toc ${srcSet} --out=${result}

checkResult "\nRunning the 'stack exec jsets -- toc ..' command exit status"

styleStart=$(  grep -n '<style>'   ${result} | cut -f1 -d: )
styleStop=$(   grep -n '</style>'  ${result} | cut -f1 -d: )
scriptStart=$( grep -n '<script>'  ${result} | cut -f1 -d: )
scriptStop=$(  grep -n '</script>' ${result} | cut -f1 -d: )

# Remove the inline css and javascript
sed -e "${styleStart},${styleStop}cCSS-MARKER"\
    -e "${scriptStart},${scriptStop}cJS-MARKER"\
    -i  ${result}

checkResult "Removing embedded css and javascript"

# Source the css and javascript files
# Replace the JACS tags to refer to JACS (2009) 131:30
sed -e "/CSS-MARKER/r${cssInsert}"\
    -e "/JS-MARKER/r${jsInsert}"\
    -e "/\(CSS-MARKER\|JS-MARKER\)/d"\
    -e "s/JACS-231-30/JACS-131-30/"\
    -e "s/2109-07-31/2009-08-05/"\
    -e "s/\"231\"/\"131\"/"\
    -e "s/231:30/131:30/"\
    -i ${result}

checkResult "Linking css & javascript source and swapping JACS 231:30 -> JACS 131:30"

echo -e "Mocked toc output saved to:\n    \e[38;5;214m${result}\e[38;5;214m"
