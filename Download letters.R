require(data.table)
require(pdftools)
require(downloader)

ipo <- fread("C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/ipo_20170422.csv")
db_name <- "C:/Users/ev99/YandexDisk/SEC Master/SEC_master.sqlite"
dir_upload <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/All Uploads/"
file_upload_csv <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload.csv"


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
  upload <- read.csv(file_upload_csv)
} else
  {
    require(DBI)
    require(RSQLite)
    upload <- get.upload(db_name)
    upload <- upload[CIK %in% as.numeric(as.character(ipo$Cik_SDC))]
    match <- match(upload$CIK, as.numeric(as.character(ipo$Cik_SDC)))
    upload$film <- ipo$SEC_file_number[match]
    upload$Deal_number <- ipo$Deal_number[match]
    upload[, out_filename := gsub("edgar/data/", "", Filename)]
    upload[, out_filename := gsub("/", "_", out_filename)]
    upload[, start_link := gsub("-", "", Link)]
    upload[, start_link := gsub(".txt", "/", start_link)]
    upload[,`:=`(GOT_TXT = NA)]
    
    write.csv(upload, file_upload_csv, row.names = F)
  }


downloaded_files <- list.files(dir_upload)
file_ind <- which(!upload$out_filename %in% downloaded_files)

options(timeout=2000)
for(i in file_ind)
{
  print(i)
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
    res <- try(download(link, "tmp.pdf", mode = "wb")) 
    if(class(res) == "try-error") next
    txt <- pdf_text("tmp.pdf")
    unlink("tmp.pdf")
  } else {
    res <- try(download.file(link, "tmp.txt", mode = "wb"))
    if(class(res) == "try-error") next
    txt <- readLines("tmp.txt")
    unlink("tmp.txt")
  }
  
  write(txt, paste0(dir_upload, upload$out_filename[i]))
}
