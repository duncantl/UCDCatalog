# https://catalog.ucdavis.edu/courses-subject-code/

subjectCodes =
function(u = "https://catalog.ucdavis.edu/courses-subject-code/")
{
    doc = htmlParse(readLines(u))
    a = getNodeSet(doc, "//a[starts-with(@href, '/courses-subject-code/')]")
    browser()
    txt = sapply(a, xmlValue)    
    w = grepl("\\([A-Z]{3}\\)$", txt)
    txt2 = trimws(gsub("\\([A-Z]{3}\\)$", "", txt[w]))
    names(txt2) = toupper( basename(sapply(a[w], xmlGetAttr, "href")))
    txt2
}
