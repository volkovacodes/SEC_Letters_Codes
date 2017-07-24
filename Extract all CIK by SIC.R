require(data.table)
require(stringr)
delay <- 0.11
all_sics <- fread("./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/sic_to_office.csv")
all_sics[, SIC := str_pad(SIC, 4, pad = "0")]


dir <- "./Projects/SEC Letter Project/Data After Review/All SIC to CIK/"



get.all.sic.ciks <- function(dir, sic)
{
  require(XML)
  require(RCurl)
  start <- "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&SIC="
  step <- 0
  repeat
  {
    Sys.sleep(delay)
    link <- paste0(start, sic, "&count=100", "&start=", step*100)
    try(file <- RCurl::getURL(link, ssl.verifyhost = 0L, ssl.verifypeer = 0L))
    if(class(file) == "try-error")
    {
      print("AAAAAAAAAAAAAA")
      print(sic)
    }
    doc <- htmlParse(file, useInternalNodes=TRUE)
    
    if(length(doc) == 0) next
    x <- readHTMLTable(doc)[[1]]
    x$SIC <- sic
    write.csv(x, paste0(dir, sic, "_", step, ".csv"), row.names = F)
    if(length(x$CIK) < 100 | !grepl('value="Next 100"', file)) break
    step <- step + 1
  }
  return(1)
}


for(sic in all_sics$SIC) get.all.sic.ciks(dir, sic)

files <- list.files(dir)
all_cik <- NULL
for(fl in files) 
{
  tmp <- fread(paste0(dir, fl))
  if(ncol(tmp) < 2) next
  all_cik <- rbind(all_cik, tmp)
}

match <- match(all_cik$SIC, all_sics$SIC)
all_cik$AD_Office <- all_sics$AD_Office[match] 
write.csv(all_cik, "./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/All_CIKs.csv", row.names = F)
unlink(dir, recursive = T)