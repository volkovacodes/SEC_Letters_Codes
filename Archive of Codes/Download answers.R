require(data.table)
db_name <- "C:/Users/ev99/YandexDisk/SEC Master/SEC_master.sqlite"
file_corresp_csv <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/corresp_all.csv"
upload_ipo <- fread("C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
dir_corresp <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/Corresp Master/"

get.corresp <- function(db_name)
{
  con = dbConnect(SQLite(), dbname=db_name)
  start <- Sys.time()
  line <- paste0('SELECT * FROM master where Form_Type == "CORRESP"')
  upload = dbGetQuery(con, line)
  upload <- as.data.table(upload)
  end <- Sys.time()
  print(end - start)
  dbDisconnect(con)
  rm(con)
  return(upload)
}

if(file.exists(file_corresp_csv))
{
  corresp <- fread(file_corresp_csv)
} else
{
  require(DBI)
  require(RSQLite)
  corresp <- get.corresp(db_name)
  
  corresp[, out_filename := gsub("edgar/data/", "", Filename)]
  corresp[, out_filename := gsub("/", "_", out_filename)]
  corresp[, start_link := gsub("-", "", Link)]
  corresp[, start_link := gsub(".txt", "/", start_link)]
  corresp <- corresp[!duplicated(out_filename)]
  write.csv(corresp, file_corresp_csv, row.names = F)
}


setkey(upload_ipo, CIK, dates)
upload_ipo[, n_letters := .N, by = CIK]

corresp <- corresp[CIK %in% upload_ipo$CIK]
match <- match(corresp$CIK, upload_ipo$CIK)
corresp$first_upload <- upload_ipo$dates[match]
corresp$Issue_date <- upload_ipo$Issue_date[match]
corresp$n_letters_upload <- upload_ipo$n_letters[match]

require(lubridate)
setkey(corresp, CIK, DATE_Filed)
corresp <- corresp[ymd(DATE_Filed) <= ymd(Issue_date) + 5 & ymd(DATE_Filed) >= ymd(first_upload) - 3]
corresp[, n_letters_corresp := .N, by = CIK]

write.csv(corresp,"./Projects/SEC Letter Project/Data After Review/corresp_ipo.csv", row.names = F)
have_files <- list.files(dir_corresp)

ind <- which(!corresp$out_filename %in% have_files)

for(i in ind)
{
  if(i %% 100 == 0) print(Sys.time())
  Sys.sleep(0.11)
  tmp <- gsub(".*/", "", corresp$Filename[i])
  link <- paste0(corresp$start_link[i],  tmp)
  
  try(download.file(link, paste0(dir_corresp, corresp$out_filename[i]),quiet = T))
}