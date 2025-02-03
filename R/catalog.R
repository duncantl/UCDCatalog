# From OGS/ESLCourses
#
# Problems
## DVM

courseInfo =
function(prog = "uwp", url = paste0("https://catalog.ucdavis.edu/courses-subject-code/", tolower(prog), "/"))
{        
    doc = htmlParse(readLines(url))
    #    z = getNodeSet(doc, "//h3[contains(., 'UWP 102') or contains(., 'UWP 104')]")
    z = getNodeSet(doc, "//h3")

    if(length(z) == 0)
        return(list())
    
    z2 = lapply(z, function(x) xpathSApply(x, ".//b", xmlValue))

    tmp = as.data.frame(matrix(unlist(z2), , 3, byrow = TRUE))
    names(tmp) = c("number", "title", "units")

    cleanCourses(tmp)
}

getCourseText =
function(node, xp)
{
    node = xmlParent(node)
    p = getNodeSet(node, xp)
    trimws(paste(xmlSApply(p[[1]], xmlValue)[-1], collapse = " "))
}

getDescription =
function(node)    
{
    getCourseText(node, ".//p[.//em[. = 'Course Description:']]")
}

getPrereqs =
function(node)    
{
    getCourseText(node, ".//p[contains(@class, 'detail-prerequisite')]")
}

cleanCourses =
function(tmp)
{
    tmp$description = gsub("^— ", "", tmp$description)
    u = tmp$units
    tmp$units = as.numeric(NA)
    rx = "\\(([0-9]) units\\)"
    w = grepl(rx, tmp$units)
    tmp$units[w] = as.numeric(gsub(rx, "\\1", tmp$units[w]))
    tmp$minUnits = tmp$maxUnits = tmp$units
    i = grep("-", u)
    rx = "\\(([0-9.]+)-[0-9.]+ units\\)"
    tmp$minUnits[i] = as.numeric(gsub(rx, "\\1", u[i]))
    tmp$maxUnits[i] = as.numeric(gsub(rx, "\\2", u[i]))    
    
    tmp$number = gsub("^UWP ", "", tmp$number)
    
    tmp
}


offered =
function(pdf, doc = readPDFXML(pdf))
{
    v = xpathSApply(doc, "//text[starts-with(., 'UWP')]", xmlValue)
    structure(trimws(gsub("^UWP [0-9A-Z/]+( - )?", "", v)), names = gsub("^(UWP [0-9A-Z]+).*", "\\1", v))
}
