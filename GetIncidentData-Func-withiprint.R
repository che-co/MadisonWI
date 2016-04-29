getIncidentData.i.print <- function(from, to){
  df <- data.frame()
  for(i in from:to){
    print(i)
  
    
      #Download XML Content
    .url <- getURL(paste('http://www.cityofmadison.com/police/newsroom/incidentreports/index.cfm?page=', i, sep = ''))
    .parsed.tree <- xmlRoot(htmlTreeParse(.url, useInternalNodes = T))
    .root <- xmlRoot(.parsed.tree)
    
    #Select body and subset XML content of data
    .incidents <- xpathSApply(.root, '//div[@class="row"]//div[@headers="date"]/..')
    .i.dates <- xpathSApply(.root, '//div[@class="row"]//div[@headers="date"]')
    .i.title <- xpathSApply(.root, '//div[@class="row"]//div[@headers="title"]')
    .i.casenumber <- xpathSApply(.root, '//div[@class="row"]//div[@headers="casenumber"]')
    .i.address <- xpathSApply(.root, '//div[@class="row"]//div[@headers="address"]')
    .i.releasedby <- xpathSApply(.root, '//div[@class="row"]//div[@headers="releasedby"]')
    .i.updated <- xpathSApply(.root, '//div[@class="row"]//div[@headers="updated"]')
    ..l <- list(.i.dates, .i.title, .i.casenumber, .i.address, .i.releasedby, .i.updated)
    
    #Extract XML values and create data frame as R content
    .l <- lapply(..l, function(x) lapply(x, function(y) xmlSApply(y, xmlValue)))
    .l.l <- lapply(.l, function(x) lapply(x, function(y) unlist(strsplit(y, '[[:space:]]{2}'))))
    .l.l.cleaned <- lapply(.l.l, function(x) lapply(x, function(y) if(length(unique(y)) == 2){y[which(y != "")]} else if(length(unique(y)) > 2){paste(unique(y)[2:length(unique(y))], collapse = " ")} else if(length(unique(y)) <= 1){y <- NA}))
    .l.cleaned.unlisted <- lapply(.l.l.cleaned, unlist)
    .df <- as.data.frame(.l.cleaned.unlisted)
    
    #Reclassify data to resonable data classes
    names(.df) <- c("DATE", "TYPE", "CASENUM", "LOC", "RELBY", "UPDATE")
    .df$DATE <- as.Date(.df$DATE, '%m/%d/%Y')
    .df$CASENUM <- as.character(.df$CASENUM)
    .df$LOC <- as.character(.df$LOC)
    .df$UPDATE <- as.Date(.df$UPDATE, '%m/%d/%Y')
    df <- rbind(df, .df)
  }
  df
}