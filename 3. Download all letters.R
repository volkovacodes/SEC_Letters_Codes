require(data.table)
require(pdftools)
require(downloader)

db_name <- "C:/Users/ev99/YandexDisk/SEC Master/SEC_master.sqlite"
master_dir_upload <- "C:/Users/ev99/Desktop/"
file_upload_csv <- "C:/Users/ev99/Desktop/upload.csv"


get.upload <- function(db_name)
{
  con = dbConnect(SQLite(), dbname=db_name)
  start <- Sys.time()
  line <- paste0('SELECT * FROM master where Form_Type == "UPLOAD"')
  upload = dbGetQuery(con, line)
  upload <- as.data.table(upload)
  end <- Sys.time()
  print(end - start)
  dbDisconnect(con)
  rm(con)
  return(upload)
}

read_url <- function(url, ...) {
  on.exit(close(url))
  readLines(url, ...)
}

if(file.exists(file_upload_csv))
{
  upload <- fread(file_upload_csv)
} else
  {
    require(DBI)
    require(RSQLite)
    upload <- get.upload(db_name)

    upload[, out_filename := gsub("edgar/data/", "", Filename)]
    upload[, out_filename := gsub("/", "_", out_filename)]
    upload[, start_link := gsub("-", "", Link)]
    upload[, start_link := gsub(".txt", "/", start_link)]
    upload <- upload[!duplicated(out_filename)]
    write.csv(upload, file_upload_csv, row.names = F)
  }

#####################
####################
######################
download.upload <- function(file_ind, dir_upload)
{
  for(i in file_ind)
  {
    #print(i)
    closeAllConnections()
    master_link <- as.character(upload$Link[i])
    res <- try(download.file(master_link,"tmp.txt", mode = "wb", quiet = T))
    if(class(res) == "try-error") next
    
    master <- readLines("tmp.txt")
    ind <- grep("<FILENAME>", master)
    filename <- master[ind[1]]
    filename <- gsub("<FILENAME>", "", filename)
    link <- paste0(upload$start_link[i], filename)
    
    if(grepl(".pdf", filename))
    {
      res <- try(download(link, "tmp.pdf", mode = "wb", quiet = T)) 
      if(class(res) == "try-error") next
      txt <- pdf_text("tmp.pdf")
      unlink("tmp.pdf")
    } else {
      res <- try(download.file(link, "tmp.txt", mode = "wb", quiet = T))
      if(class(res) == "try-error") next
      txt <- readLines("tmp.txt")
      unlink("tmp.txt")
    }
    
    write(txt, paste0(dir_upload, upload$out_filename[i]))
  }
  return(1)
}
options(timeout=2000)

require(lubridate)
upload[, year := year(ymd(DATE_Filed))]

for(yr in 2004:2017)
{
  print(yr)
  print(Sys.time())
  dir_upload <- paste0(master_dir_upload, yr, "/")
  file_ind <- which(upload$year == yr)
  
  if(!dir.exists(dir_upload)) dir.create(dir_upload)
  if(dir.exists(dir_upload))
  {
    ex_files <- list.files(dir_upload) 
    file_ind <- which(upload$year == yr & !(upload$out_filename %in% ex_files))
  }
  print("file downloaded this year: ")
  print(length(ex_files))
  
  print("file to download in this year: ")
  print(length(file_ind))
  download.upload(file_ind, dir_upload)
}

put.in.db <- function(folder, cyear)
{
  
  files <- list.files(paste0(folder, cyear, "/"))
  letters <- sapply(paste0(folder, cyear, "/", files), readLines)

  df <- upload[year == cyear]
  df <- as.data.frame(df)
  match <- match(df$out_filename, files)
  
  tmp <- letters[match]
  together <- function(x)
  {
    return(paste(x, collapse = "\n"))
  }
  clean <- sapply(tmp, together)
  df$Letter <- clean
  
  require(DBI)
  require(RSQLite)
  setwd(folder)
  db_name <- paste0("upload_", cyear, ".sqlite")
  print(db_name)
  con = dbConnect(SQLite(), dbname = db_name)
  
  if(!file.exists(db_name))
  {
    dbSendQuery(conn=con,
                "CREATE TABLE content
                (CIK INTEGER, Company_Name TEXT, Form_Type TEXT, 
                DATE_Filed DATE, Filename TEXT, Link TEXT,
                out_filename TEXT, start_link TEXT, year TEXT, Letter TEXT)")
  }
  dbWriteTable(conn=con, name = "content", df, append = F)
  dbDisconnect(con)
  rm(con)
  return(1)
}
  
for(yr in 2004:2017)
{
  put.in.db(master_dir_upload, yr)
}

