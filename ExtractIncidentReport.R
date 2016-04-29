###################################################################################
#####     Scrape Police Incident Report data from City of Madison Website     #####
###################################################################################

# Scrape data frame of incident reports -----------------------------------

##Download XML Data and parse and html
.url <- getURL('http://www.cityofmadison.com/police/newsroom/incidentreports/index.cfm?page=109')
.parsed.tree <- htmlTreeParse(.url, useInternalNodes = T)
.root <- xmlRoot(.parsed.tree)

##Select body and subset XML content of data
.incidents <- xpathSApply(.root, '//div[@class="row"]//div[@headers="date"]/..')
.i.dates <- xpathSApply(.root, '//div[@class="row"]//div[@headers="date"]')
.i.title <- xpathSApply(.root, '//div[@class="row"]//div[@headers="title"]')
.i.casenumber <- xpathSApply(.root, '//div[@class="row"]//div[@headers="casenumber"]')
.i.address <- xpathSApply(.root, '//div[@class="row"]//div[@headers="address"]')
.i.releasedby <- xpathSApply(.root, '//div[@class="row"]//div[@headers="releasedby"]')
.i.updated <- xpathSApply(.root, '//div[@class="row"]//div[@headers="updated"]')
..l <- list(.i.dates, .i.title, .i.casenumber, .i.address, .i.releasedby, .i.updated)

##Extract XML values and create data frame as R content
.l <- lapply(..l, function(x) lapply(x, function(y) xmlSApply(y, xmlValue)))
.l.l <- lapply(.l, function(x) lapply(x, function(y) unlist(strsplit(y, '[[:space:]]{2}'))))
.l.l.cleaned <- lapply(.l.l, function(x) lapply(x, function(y) if(length(unique(y)) == 2){y[which(y != "")]} else if(length(unique(y)) > 2){paste(unique(y)[2:length(unique(y))], collapse = " ")} else if(length(unique(y)) <= 1){y <- NA}))
.l.cleaned.unlisted <- lapply(.l.l.cleaned, unlist)
.df <- as.data.frame(.l.cleaned.unlisted)

##Reclassify data to resonable data classes
names(.df) <- c("DATE", "TYPE", "CASENUM", "LOC", "RELBY", "UPDATE")
.df$DATE <- as.Date(.df$DATE, '%m/%d/%Y')
.df$CASENUM <- as.character(.df$CASENUM)
.df$LOC <- as.character(.df$LOC)
.df$UPDATE <- as.Date(.df$UPDATE, '%m/%d/%Y')
df <- .df
df


# Build .txt file of detailed report --------------------------------------

##Download XML Content
.case.url <- getURL('http://www.cityofmadison.com/police/newsroom/incidentreports/incident.cfm?id=17255')
.case.parsed <- htmlTreeParse(.case.url, useInternalNodes = T)
.case.root <- xmlRoot(.case.parsed)

##Extract XML values and create character sting
.char <- xpathSApply(.case.root, "//div[@class='incident-report-detail']/div", xmlValue)
.releasedby <- xpathSApply(.case.root, '//div[@class="incident-report-detail"]//span[@class="helper-text"]', xmlValue)

#Format stings to readable text with repeated text and space
.v.char <- unlist(lapply(.char, gsub, pattern = "[\\\r\n\t]+[[:space:]\\\r\n\t[:space:]]*", replace = '\\\r\n'))
.v.r.char <- gsub('([[:graph:]])(Suspect)+', '\\1\n\\2', .v.char)
.v.r.char <- gsub(paste0('(', .releasedby, ')*', collapse = ""), "", .v.r.char)
.case.char <- c(.v.r.char, paste("\r\n", .releasedby, "\r\n", sep = ""))
.case.char.padded <- gsub('([[:print:]]{80})([[:space:]])+', '\\1\n', .case.char)
names(.case.char.padded) <- rep(str_extract(.case.char.padded, '#[0-9]+-[0-9]+')[1], length(.case.char.padded))
cat(.case.char.padded, file = "~/detailed_reports.txt")

