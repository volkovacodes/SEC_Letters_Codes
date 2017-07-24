require(data.table)

file_upload_csv <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload_4000.csv"
sys_delay <- 0.10001

upload <- fread(file_upload_csv, nrows = 167456)


read_url <- function(url, ...) {
  on.exit(close(url))
  readLines(url, ...)
}

index <- intersect(1:length(upload$CIK), which(is.na(upload$DATE_Writen)))
for(i in index)
{
  Sys.sleep(sys_delay)
  if(i %% 500 == 0) 
  {
    print(i)
    print(Sys.time())
    name <- paste0("C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload_", i, ".csv")
    write.csv(upload, name, row.names = F)
  }
  link <- upload$Link[i]
  top <- try(read_url(url(link))[1:200])
  if(class(top) == "try-error") next
  ind <- grep("<SEC-DOCUMENT>", top)
  if(length(ind) == 0) next
  upload$DATE_Writen[i] <- top[ind[1]]
  
  ind <- grep("ACCEPTANCE-DATETIME", top)
  if(length(ind) == 0) next
  upload$DATE_Acceptance[i] <- top[ind[1]]
  
  closeAllConnections()
}

write.csv(upload, "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/Additional Data/upload_with_publish_dates.csv", row.names = F)
