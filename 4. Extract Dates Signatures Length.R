require(data.table)
require(lubridate)
require(stringr)
require(DBI)
require(RSQLite)
require(caroline)
sql_dir <- "/Volumes/SD_card/Yandex.Disk.localized/SEC Master/UPLOAD/UPLOAD sqlite/"
get.header <- function(x)
{
  x <- unlist(x)
  x <- strsplit(x, "\n")
  x <- unlist(x)
  i <- grep("(Re\\s:)|(Re:)",x, ignore.case = T, useBytes = TRUE)
  if(length(i) == 0) return(NA)
  out <- x[1:i[1]]
  
  out <- paste(out, collapse = "\n")
  return(out)
}

get.date <- function(x)
{
  x <- unlist(x)
  x <- str_split(x, "\n")
  x <- unlist(x)
  x <- gsub("Spetember", "September", x)
  i <- grep("January|February|March|April|May|June|July|August|September|October|November|December",x)
  if(length(i) == 0) return(NA)
  if(grepl("(2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)",x[i[1]])) line <- x[i[1]]
  if(!grepl("(2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017)",x[i[1]])) line <- paste(x[i[1]:(i[1]+3)], collapse = " ")
    
  line <- gsub(".*(January|February|March|April|May|June|July|August|September|October|November|December)", "\\1", line, perl = T)
  line <- gsub("(2003|2004|2005|2006|2007|2008|2009|2010|2011|2012|2013|2014|2015|2016|2017).*", "\\1", line, perl = T)
  
  line <- paste0(line, collapse = " ")
  date <- as.character(mdy(line))
  return(date)
}

### get actual letter dates
for(cyear in 2004:2017)
{
  print(Sys.time())
  print(cyear)
  db_name <- paste0(sql_dir, "upload_", cyear, ".sqlite")
  con <- dbConnect(SQLite(), db_name)  
  
  files <- dbGetQuery(con, 'SELECT * FROM content')
  header <- lapply(files$Letter, get.header)
  dates <- sapply(header, get.date)
  files$DATE_sign <- dates
  
  dbSendQuery(conn=con,
              "CREATE TABLE dates
            (out_filename TEXT, DATE_sign TEXT)")
  
  dbWriteTable(con, "dates", value= data.frame(out_filename = files$out_filename, DATE_sign = files$DATE_sign), append = T)
  dbDisconnect(con)
  rm(con)
}

### get re and signatures
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
get.re <- function(x)
{
  x <- unlist(x)
  x <- strsplit(x, "\n")
  x <- unlist(x)
  
  start <- grep("(RE\\s:)|(RE:)", x, ignore.case = T)
  if(length(start) == 0) return(NA)
  
  start <- start[1]
  end <- grep("(Dear:)|(Dear\\s:)", x, ignore.case = T)
  if(length(end) == 0) end <- start+5
  if(length(end) > 0) end <- end[1] - 1
  re <- paste(x[start:end], collapse = " ")
  re <- trim(re)
  re <- gsub("\\t+", " ", re, perl = T)
  return(re)
}

get.sign <- function(letter)
{
  letter <- unlist(letter)
  letter <- unlist(strsplit(letter, "\n"))
  if(length(letter) <= 10) 
  {
    return(0)
  }
  letter <- gsub("\\t+", " ", letter)
  letter <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", letter, perl=TRUE)
  pos <- max(grep("(Sincerely)|(Regards,)|(Very truly yours)|(Respectflly submitted,)", letter, ignore.case = T))
  if(is.infinite(pos)) return(NA)
  test <- letter[pos:min(pos+6, length(letter))]
  test <- paste0(test, collapse = " ")
  test <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", test, perl=TRUE)
  test <- gsub("(cc:)|(cc :)|( via ).*", "", test, ignore.case = T)
  return(test)
}
get.n_words <- function(x)
{
  x <- unlist(x)
  x <- strsplit(x, "\n")
  x <- unlist(x)
  
  require(qdapDictionaries)
  require(stringr)
  x <- unlist(str_extract_all(x, "\\w+"))
  x <- tolower(x)
  x <- x[x %in% GradyAugmented]
  return(length(x))
}
for(cyear in 2004:2017)
{
  print(Sys.time())
  print(cyear)
  db_name <- paste0(sql_dir, "upload_", cyear, ".sqlite")
  con <- dbConnect(SQLite(), db_name)  
  
  files <- dbGetQuery(con, 'SELECT * FROM content')
  re <- lapply(files$Letter, get.re)
  files$re <- unlist(re)
  
  sign <- lapply(files$Letter, get.sign)
  files$sign <- unlist(sign)
  
  n_words <- lapply(files$Letter, get.n_words)
  files$n_words <- unlist(n_words)
  
  dbSendQuery(conn=con,
              "CREATE TABLE extra
              (out_filename TEXT, re TEXT, sign TEXT, n_words TEXT)")
  
  dbWriteTable(con, "extra", value= data.frame(out_filename = files$out_filename, re = files$re, sign = files$sign, n_words = files$n_words), append = T)
  dbDisconnect(con)
  rm(con)
}


