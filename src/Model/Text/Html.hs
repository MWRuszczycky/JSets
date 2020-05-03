{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Html
    ( htmlHeader
    , htmlBody
    ) where

import qualified Data.Text             as Tx
import qualified Model.Core.Types      as T
import qualified Model.Core.Core       as C
import qualified Model.Journals        as J
import qualified Model.Core.Dates      as D
import           Data.Text                      ( Text    )
import           Data.Char                      ( isSpace )

-- --------------------------------------------------------------- --
-- Helpers

issueToCToJSet :: Int -> [T.IssueToC] -> T.JournalSet
issueToCToJSet setNumber = T.JSet setNumber . map go
    where go ( T.IssueToC x _ ) = x

ind :: Int -> Text -> Text
ind n xs = Tx.replicate (4 * n) " " <> xs

sectionBreak :: Text -> Text
sectionBreak name = Tx.unlines xs
    where xs = [ "<!-- " <> Tx.replicate 60 "=" <> " -->"
               , "<!-- " <> name <> " -->"
               ]

className :: T.Issue -> Text
className iss = Tx.intercalate "-" xs
    where xs = [ ("_" <>) . spaceToUnder . T.key . T.journal $ iss
               , C.tshow . T.volNo $ iss
               , C.tshow . T.issNo $ iss
               ]

citationID :: T.Issue -> T.Citation -> Text
citationID iss c = className iss <> "-" <> C.tshow p1
    where (p1,_) = T.pages c

spaceToUnder :: Text -> Text
spaceToUnder = Tx.map go
    where go x | isSpace x = '_'
               | otherwise = x

fixReserved :: Text -> Text
fixReserved = Tx.concatMap go
    where go '<' = "&lt"
          go '>' = "&gt"
          go '&' = "&amp"
          go x   = Tx.singleton x

-- --------------------------------------------------------------- --
-- Page components

htmlHeader :: Int -> [T.IssueToC] -> Text
htmlHeader setNumber tocs = Tx.unlines xs
    where xs = [ "<!DOCTYPE html>"
               , "<html>"
               , ind 1 "<head>"
               , ind 2 $ "<title>Journal Set " <> C.tshow setNumber <> "</title>"
               , ind 2 "<meta charset=\"UTF-8\">\n"
               , css
               , scripts setNumber tocs
               , ind 1 "</head>"
               ]

htmlBody :: Int -> [T.IssueToC] -> Text
htmlBody setNumber tocs = Tx.unlines xs
    where xs = [ sectionBreak "CONTENT"
               , ind 1 "<body>\n"
               , ind 2 "<h1>Journal Set " <> C.tshow setNumber <> "</h1>\n"
               , sectionBreak "Tables of Contents"
               , Tx.unlines . map (issToCHtml 2) $ tocs
               , sectionBreak "Selection"
               , ind 2 "<h2>Create your selection, save it and send it to Mark!</h2>\n"
               , ind 3 "<ol id=\"inst\" style=\"display:none\">"
               , ind 3 "    <li>Your selection has been created.</li>"
               , ind 3 "    <li>There are <b><span id=\"count\"></span></b> articles in the selection.</li>"
               , ind 3 "    <li>If you want to change your selection, refresh the webpage.</li>"
               , ind 3 "    <li>If you want to save your selection <a id=\"saveLink\"><b>click here</b></a>.</li>"
               , ind 3 "    <li>Your selection should be saved in your <b>Downloads</b> folder.</li>"
               , ind 3 "    <li>The file name of your selection is <b><span id=\"fileNameWgt\"></span></b>.</li>"
               , ind 3 "    <li>Email your selection file to Mark.</li>"
               , ind 3 "    <li>Afterwards, you can delete this html file and the selection file.</li>"
               , ind 3 "</ol>\n"
               , ind 3 "<div id=\"createWgt\">"
               , ind 3 "    <label for=\"initials\">Enter your initials here:</label>"
               , ind 3 "    <input type=\"text\" id=\"initials\">"
               , ind 3 "    <button type=\"button\" id=\"createBtn\" onclick=\"createSelection()\">"
               , ind 3 "        Create Selection!"
               , ind 3 "    </button>"
               , ind 3 "</div>"
               , ind 1 "</body>"
               , "</html>"
               ]

css :: Text
css = Tx.unlines xs
    where xs = [ sectionBreak "STYLES"
               , ind 2 "<style>"
               , ind 3 "body { background-color: white;"
               , ind 3 "       color: #101010;"
               , ind 3 "       font-family: sans-serif;"
               , ind 3 "       font-size: 100%; }"
               , ind 3 "a:link { color: steelblue; }"
               , ind 3 "a:visited { color: darkslateblue; }"
               , ind 3 "div.selectedPages { text-indent: 4em; }"
               , ind 3 "p { display: block;"
               , ind 3 "    padding-left: 2.5em;"
               , ind 3 "    text-indent: -1.7em; }"
               , ind 2 "</style>"
               ]

scripts :: Int -> [T.IssueToC] -> Text
scripts setNumber tocs = Tx.unlines xs
    where xs = [ sectionBreak "SCRIPTS"
               , ind 2 "<script>\n"
               , Tx.unlines . map (ind 3) $ setInfo setNumber tocs
               , ind 3 "class Journal {"
               , ind 3 "    constructor(key, name, volume, issue, published){"
               , ind 3 "        this.key = key;"
               , ind 3 "        this.name = name;"
               , ind 3 "        this.volume = volume;"
               , ind 3 "        this.issue = issue;"
               , ind 3 "        this.published = published;"
               , ind 3 "    };"
               , ind 3 "    header(){"
               , ind 3 "        var volNo   = this.volume + \" \" + this.issue;"
               , ind 3 "        var pubdate = \"(\" + this.published + \")\";"
               , ind 3 "        return this.name + \" \" + volNo + \" \" + pubdate;"
               , ind 3 "   };"
               , ind 3 "};\n"
               , Tx.unlines . map (ind 3) . journalsArray . map T.tocIssue $ tocs
               , ind 3 "function pageNumber(journalObj, articleObj) {"
               , ind 3 "// Parse the page numbers from a article id."
               , ind 3 "    var p = articleObj.id.slice(journalObj.key.length + 1);"
               , ind 3 "    return \"    \" + p + \"\\n\""
               , ind 3 "};\n"
               , ind 3 "function isAlphaNum(x) {"
               , ind 3 "// Determine if character is alpha-numeric."
               , ind 3 "    if (x.length === 1) {"
               , ind 3 "        var v     = x.charCodeAt(0);"
               , ind 3 "        var isNum = (v > 47 && v < 58);"
               , ind 3 "        var isCap = (v > 64 && v < 91);"
               , ind 3 "        var isLow = (v > 96 && v < 123);"
               , ind 3 "        return isNum || isCap || isLow;"
               , ind 3 "    };"
               , ind 3 "    return false;"
               , ind 3 "};\n"
               , ind 3 "function makeFileName(prefix, xs) {"
               , ind 3 "// Generate a file name for the selection."
               , ind 3 "    var suffix = \"\";"
               , ind 3 "    for (i = 0; i < xs.length; i++) {"
               , ind 3 "        if (isAlphaNum(xs[i])) {"
               , ind 3 "            suffix += xs[i];"
               , ind 3 "        };"
               , ind 3 "    };"
               , ind 3 "    if (suffix) {"
               , ind 3 "        return prefix + \"-\" + suffix + \".txt\";"
               , ind 3 "    };"
               , ind 3 "    return prefix + \"-\" + \"none.txt\";"
               , ind 3 "};\n"
               , ind 3 "function createSelection() {"
               , ind 3 "// Create selection content for saving and update page."
               , ind 3 "    var selection = jsetHeader + \"\\n\";"
               , ind 3 "    var count     = 0;"
               , ind 3 "    for (i = 0; i < journals.length; i++ ){"
               , ind 3 "        selection += journals[i].header() + \"\\n\";"
               , ind 3 "        var xs = document.getElementsByClassName(journals[i].key);"
               , ind 3 "        var pages = \"\""
               , ind 3 "        for (j = 0; j < xs.length; j++){"
               , ind 3 "            if (xs[j].checked) {"
               , ind 3 "                pages += pageNumber(journals[i], xs[j]);"
               , ind 3 "                count += 1;"
               , ind 3 "            };"
               , ind 3 "        };"
               , ind 3 "        selection += pages;"
               , ind 3 "    };\n"
               , ind 3 "    var inst          = document.getElementById('inst');"
               , ind 3 "    var saveLink      = document.getElementById('saveLink');"
               , ind 3 "    var createWgt     = document.getElementById('createWgt');"
               , ind 3 "    var initials      = document.getElementById('initials');"
               , ind 3 "    var fileNameWgt   = document.getElementById('fileNameWgt');"
               , ind 3 "    var countWgt      = document.getElementById('count');"
               , ind 3 "    var payload       = encodeURIComponent(selection);"
               , ind 3 "    var fileName      = makeFileName(savePrefix, initials.value);\n"
               , ind 3 "    saveLink.href     = \"data:text/plain;charset=utf-8,\" + payload;"
               , ind 3 "    saveLink.download = fileName;\n"
               , ind 3 "    fileNameWgt.innerHTML = fileName;"
               , ind 3 "    countWgt.innerHTML    = count;"
               , ind 3 "    inst.style            = \"display:block\";"
               , ind 3 "    createWgt.style       = \"display:none\";"
               , ind 3 "};\n"
               , ind 2 "</script>"
               ]

-- --------------------------------------------------------------- --
-- Page subcomponents

setInfo :: Int -> [T.IssueToC] -> [Text]
setInfo setNumber tocs = [ Tx.concat hdr, Tx.concat pre ]
    where jset = issueToCToJSet setNumber tocs
          d    = J.dateOfJSet jset
          y    = D.getYear d
          hdr  = [ "jsetHeader = \"" , C.tshow setNumber
                 , " | "
                 , C.tshow . J.dateOfJSet $ jset
                 , "\";"
                 ]
          pre  = [ "savePrefix = \"sel"
                 , C.tshow y
                 , "-" <> C.tshow setNumber
                 , "\";"
                 ]

issToCHtml :: Int -> T.IssueToC -> Text
issToCHtml indent (T.IssueToC iss cs) = Tx.unlines $ hdr : bdy
    where hdr = ind indent (issueHeader iss)
          msg = "<p>There are no articles listed for this issue at PubMed!</p>"
          bdy | null cs   = [ ind (indent + 1) msg ]
              | otherwise = map ( citationHtml indent iss ) cs

issueHeader :: T.Issue -> Text
issueHeader iss = Tx.concat xs
    where xs = [ "<h2>"
               , T.name . T.journal $ iss
               , " "
               , C.tshow . T.volNo $ iss
               , ":"
               , C.tshow . T.issNo $ iss
               , "</h2>\n"
               ]

citationHtml :: Int -> T.Issue -> T.Citation -> Text
citationHtml indent iss c = Tx.unlines xs
    where jn = T.journal iss
          ps = T.pages c
          n  = indent + 1
          xs = [ ind n "<p>"
               , ind n $ "<input type=\"checkbox\" id=\""
                         <> citationID iss c
                         <> "\" class=\""
                         <> className iss
                         <> "\">"
               , ind n "<label>"
               , ind n $ "    <b><a href=\"" <> T.doi c <> "\" target=\"_blank\">"
               , ind n $ "    " <> (fixReserved . T.title)   c <> "</a></b><br>"
               , ind n $ "    " <> (fixReserved . T.authors) c <> "<br>"
               , ind n $ "    <i>" <> T.name jn <> "</i> "
                         <> (C.tshow . T.volNo) iss <> ":"
                         <> (C.tshow . T.issNo) iss
                         <> " " <> (C.tshow . fst) ps
                         <> "-" <> (C.tshow . snd) ps
               , ind n "</label>"
               , ind n "</p>"
               ]

journalsArray :: [T.Issue] -> [Text]
journalsArray issues = let goInd x = Tx.replicate 11 " " <> ", " <> x
                       in   case map journalsArrayElement $ issues of
                                 []     -> [ "journals = [];"]
                                 (e:es) -> ("journals = [ " <> e)
                                           : map goInd es
                                           <> [ Tx.replicate 11 " " <> "];" ]

journalsArrayElement :: T.Issue -> Text
journalsArrayElement iss = Tx.intercalate ", " xs
    where xs = [ "new Journal(\"" <> className iss <> "\""
               , "\"" <> (T.key . T.journal) iss <> "\""
               , "\"" <> (C.tshow . T.volNo) iss <> "\""
               , "\"" <> (C.tshow . T.issNo) iss <> "\""
               , "\"" <> (C.tshow . T.date ) iss <> "\")"
               ]
