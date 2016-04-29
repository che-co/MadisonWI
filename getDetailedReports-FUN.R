getDetailedReports <- function(from, to, padding = 80, sep = "+"){
  if(padding < 10){warning("A padding of < 10 will probs get you all fucked up... just saying")}
  
  .sep <- paste("\n", paste(rep(sep, padding / nchar(sep)), collapse = ""), "\n", sep = "")
  cases <- .sep
  for(i in from:to){
    #Get URL of case report from incident report site
    .url <- getURL(paste('http://www.cityofmadison.com/police/newsroom/incidentreports/index.cfm?page=', i, sep = ''))
    .parsed.tree <- htmlTreeParse(.url, useInternalNodes = T)
    .root <- xmlRoot(.parsed.tree)
    .case.urls <- xpathSApply(.root, '//div[@class="row"]//div[@headers="casenumber"]/a', xmlAttrs)
    
    ##Go to detailed report sites
    for(j in 1:length(.case.urls)){
      ##Download XML Content
      .case.url <- getURL(paste('http://www.cityofmadison.com/police/newsroom/incidentreports/', .case.urls[j], sep = ""))
      .case.parsed <- htmlTreeParse(.case.url, useInternalNodes = T)
      .case.root <- xmlRoot(.case.parsed)
      
      ##Extract XML values and create character sting
      .char <- xpathSApply(.case.root, "//div[@class='incident-report-detail']/div", xmlValue)
      .releasedby <- xpathSApply(.case.root, '//div[@class="incident-report-detail"]//span[@class="helper-text"]', xmlValue)
      
      #Format stings to readable text with repeated text
      .v.char <- unlist(lapply(.char, gsub, pattern = "[\\\r\n\t]+[[:space:]\\\r\n\t[:space:]]*", replace = '\\\r\n'))
      .v.char <- gsub('([[:graph:]])(Suspect)+', '\\1\n\\2', .v.char)
      .v.char <- gsub(paste0('(', .releasedby, ')*', collapse = ""), "", .v.char)
      .ccase.char <- c(.v.char, paste("\r\n", .releasedby, "\r\n", sep = ""))
      .case.char.padded <- gsub(paste0('([[:print:]]{', padding, '})([[:space:]])+'), '\\1\n', .ccase.char)
      names(.case.char.padded) <- rep(str_extract(.case.char.padded, '#[0-9]+-[0-9]+')[1], length(.case.char.padded))
      cases <- c(cases, .case.char.padded, .sep)
    }
  }
  return(cases)
}