require(data.table)
require(bit64)
require(qdapDictionaries)
require(stringr)
require(lubridate)
get_S1 <- F ### I do not need to download S1 everytime
#yadir <- "C:/Users/ev99/YandexDisk/"
yadir <- "/Volumes/SD_card/Yandex.Disk.localized/"
#setwd("/Users/orhahog/Dropbox/")
dir_add <- "./Projects/SEC Letter Project/Data After Review/Additional Data/"
dir_s1 <- "./Projects/SEC Letter Project/Data After Review/S1/"
ipo <- read.csv("./Projects/SEC Letter Project/Data After Review/ipo_20170422.csv")
s1_length <- paste0(dir_add, "S1_length.csv")
jobs_act <- paste0(dir_add, "JOBS ACT.csv")
ipo <- as.data.table(ipo)
ipo[, tmp := .N, by = Cik_SDC]
ipo <- ipo[tmp == 1]

### start of the film link
prefix <- "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&filenum=333-"
### end of the film link
postfix <- c("&owner=exclude&count=100", "&type=&dateb=&owner=exclude&start=100&count=100",
             "&type=&dateb=&owner=exclude&start=200&count=100")


### download the main file from each prospectus
### put F here is files should be downloaded
if(get_S1 == T)
{
  ### convert body from .html forms
  html2txt <- function(file, clean = T) {
    library(XML)
    start <- grep("<HTML>", file, ignore.case = T)[1]
    end <-  grep("</HTML>", file, ignore.case = T)[1]
    if(clean == T) file <- file[start:end]
    xpathApply(htmlParse(file, encoding="UTF-8"), "//body", xmlValue)[[1]] 
  }
  ### covert body from .txt forms
  cleantxt <- function(file) {
    library(XML)
    start <- grep("<TEXT>", file, ignore.case = T)[1]
    end <-  grep("</TEXT>", file, ignore.case = T)[1]
    file <- file[start:end]
    return(file)
    #xpathApply(htmlParse(file, encoding="UTF-8"), "//body", xmlValue)[[1]] 
  }
  
  for(i in 1:length(ipo$Filing_date))
  {
    print(i)
    ### how many pages with 100 links are there?
    ind_end <- ipo$n_film[i] %/% 100 + 1
    if(file.exists(paste0(dir_s1, ipo$Cik_SDC[i], ".txt"))) next
    link <- paste0(prefix, ipo$SEC_file_number[i], postfix[ind_end])
    ### read all files from the page
    file <- readLines(url(link))
    iii<- max(grep("/Archives/edgar/data/", file))
    ### I have one exception where the first filing is SE instead of S-1
    ### this if() just to handle on case
    if(ipo$Form1[i] == "SE") iii<- sort(grep("/Archives/edgar/data/", file), decreasing = T)[2]
    line <- file[iii] 
    line <- gsub(".*/Archives/edgar/", "https://www.sec.gov/Archives/edgar/", line)
    line <- gsub("html.*", "html", line)
    line <- gsub("htm.*", "htm", line)
    index <- try(readLines(url(line)))
    if(class(index) == "try-error") next 
    
    iii <- max(grep("/Archives/edgar/data/.*(txt|htm)", index))
    line <- index[iii]
    line <- gsub(".*/Archives/edgar/", "https://www.sec.gov/Archives/edgar/", line)
    line <- gsub("html.*", "html", line)
    line <- gsub("htm.*", "htm", line)
    line <- gsub("txt.*", "txt", line)
    s1 <- try(readLines(url(line)))
    if(class(s1) == "try-error") next 
    ### extract "main" part from filings
    if(length(grep("<HTML>", s1, ignore.case = T)) > 0) s1 <- html2txt(s1)
    if(length(grep("<TEXT>", s1, ignore.case = T)) > 0) s1 <- cleantxt(s1)
    write(s1, paste0(dir_s1, ipo$Cik_SDC[i], ".txt"))
    closeAllConnections()
  }
  
  ### delete very short files
  file_info <- file.info(paste0(dir_s1,list.files(dir_s1)))
  file.remove(row.names(file_info[file_info$size < 150000,]))
  missing_ind <- which(!paste0(ipo$Cik_SDC,".txt") %in% list.files(dir_s1))
  for(i in missing_ind)
  {
    print(i)
    ind_end <- ipo$n_film[i] %/% 100 + 1
    if(file.exists(paste0(dir_s1, ipo$Cik_SDC[i], ".txt"))) next
    link <- paste0(prefix, ipo$SEC_file_number[i], postfix[ind_end])
    file <- readLines(url(link))
    iii<- max(grep("/Archives/edgar/data/", file))
    if(ipo$Form1[i] == "SE") iii<- sort(grep("/Archives/edgar/data/", file), decreasing = T)[2]
    line <- file[iii] 
    line <- gsub(".*/Archives/edgar/", "https://www.sec.gov/Archives/edgar/", line)
    line <- gsub("html.*", "html", line)
    line <- gsub("htm.*", "htm", line)
    res <- try(index <- readLines(url(line)))
    if(class(res) == "try-error") next 
    
    iii <- max(grep("/Archives/edgar/data/.*(txt|htm)", index))
    line <- index[iii]
    line <- gsub(".*/Archives/edgar/", "https://www.sec.gov/Archives/edgar/", line)
    line <- gsub("html.*", "html", line)
    line <- gsub("htm.*", "htm", line)
    line <- gsub("txt.*", "txt", line)
    res <- try(s1 <- readLines(url(line)))
    if(class(res) == "try-error") next 
    if(length(grep("<HTML>", s1, ignore.case = T))) s1 <- html2txt(s1, clean = F)
    write(s1, paste0(dir_s1, ipo$Cik_SDC[i], ".txt"))
    closeAllConnections()
  }
}


### loading uncertanty measure
uncertain <- readLines("./Projects/SEC Letter Project/Data After Review/Additional Data/Loughran_McDonald_AggregateIPOWordList.txt")
uncertain <- tolower(uncertain)
### Measuring number of words and uncertanty
if(file.exists(s1_length))
{
  df <- read.csv(s1_length)
  m <- match(ipo$Deal_number, df$Deal_number)
  ipo[, `:=`(S1_words = df$S1_words[m], S1_uncertanty = df$S1_uncertanty[m])]
  rm(df)
}
if(!file.exists(s1_length))
{
  for(i in 1:length(ipo$Filing_date))
  {
    print(i)
    prosp <- readLines(paste0(dir_s1, ipo$Cik_SDC[i], ".txt"))
    prosp <- unlist(str_extract_all(prosp, "\\w+"))
    prosp <- tolower(prosp)
    prosp <- prosp[prosp %in% GradyAugmented]
    
    ipo$S1_words[i] <- length(prosp)
    ipo$S1_uncertanty[i] <- length(prosp[prosp %in% uncertain])/length(prosp)
  }
  df <- data.frame(Deal_number = ipo$Deal_number, S1_words = ipo$S1_words, S1_uncertanty = ipo$S1_uncertanty)
  write.csv(df, s1_length, row.names = F)
}


### fixing a couple of data points
ipo[Form1 == "SE", Form1 := "S-1"]
ipo[Form1 == "S-1/A", Form1 := "S-1"]
ipo <- ipo[Form1 != "S-3"]
ipo <- ipo[S1_words > 1000] ### one "problem" file has 833 words => drop it
ipo[, list(n = length(S1_words), mean_l = mean(S1_words), 
           mean_unc = mean(S1_uncertanty)), by = Form1]


ritter_rank <- fread("./Projects/SEC Letter Project/Data After Review/Additional Data/Underwriter-Rank-1980-2015.csv",
                        select = c("Underwriter_Name", "Rank9200", "Rank0104", "Rank0507", "Rank0809", "Rank1011", "Rank1215"))
### fixing UW rank
ipo[, main_uw :=  gsub("\n.*", "", Managers)]
ipo$main_uw[grep("JP Morgan", ipo$main_uw)] <- "JP Morgan (JPM)"
ipo$main_uw[grep("Morgan Stanley", ipo$main_uw)] <- "Morgan Stanley & Co"
ipo$main_uw[grep("Merrill Lynch", ipo$main_uw)] <- "Merrill Lynch & Co Inc"
ipo$main_uw[grep("(CS First Boston Corp)|(Credit Suisse)", ipo$main_uw)] <- "Credit Suisse First Boston"
ipo$main_uw[ipo$main_uw == "Bank of America Merrill Lynch"] <- "Bank of America-Merrill Lynch (BOA-Merrill)"
ipo$main_uw[ipo$main_uw == "Citi"] <- "Citigroup"
ipo$main_uw[ipo$main_uw == "Barclays"] <- "Barclays Capital"
ipo$main_uw[ipo$main_uw == "Thomas Weisel Partners"] <- "Thomas Weisel Partners LLC"
ipo$main_uw[ipo$main_uw == "Credit Suisse Securities (USA)"] <- "Credit Suisse First Boston"
ipo$main_uw[ipo$main_uw == "CIBC World Markets Inc"] <- "CIBC World Markets"
ipo$main_uw[ipo$main_uw == "WR Hambrecht & Co LLC"] <- "W.R. Hambrecht & Company"
ipo$main_uw[ipo$main_uw == "Deutsche Bank Securities"] <- "Deutsche Bank Securities Corp"
ipo$main_uw[ipo$main_uw == "Cowen & Co"] <- "Cowen"
ipo$main_uw[ipo$main_uw == "Ferris Baker Watts"] <- "Ferris, Baker Watts"
ipo$main_uw[ipo$main_uw == "Jefferies LLC"] <- "Jefferies & Co Inc"
ipo$main_uw[ipo$main_uw == "Leerink Swann & Co"] <- "Leerink Swann & Co."
ipo$main_uw[ipo$main_uw == "National Securities Corp (US)"] <- "National Securities Corp"
ipo$main_uw[ipo$main_uw == "Warburg Dillon Read Inc"] <- "SBC Warburg Dillon Read Inc"
ipo$main_uw[ipo$main_uw == "FBR Capital Markets Corp"] <- "Friedman Billings Ramsey Group"
ipo$main_uw[ipo$main_uw == "Needham & Co LLC"] <- "Needham & Co Inc"
ipo$main_uw[ipo$main_uw == "Barclays PLC"] <- "Barclays Capital"
ipo$main_uw[ipo$main_uw == "Leerink Partners LLC"] <- "Leerink Swann & Co."
ipo$main_uw[ipo$main_uw == "MDB Capital Corp"] <- "MDB-Capital"
ipo$main_uw[ipo$main_uw == "Deutsche Bank"] <- "Deutsche Bank Securities Corp"
ipo$main_uw[ipo$main_uw == "Howe Barnes Investments Inc"] <- "Howe Barnes Investments Inc."
ipo$main_uw[ipo$main_uw == "JMP Securities LLC"] <- "JMP-Sec"
ipo$main_uw[ipo$main_uw == "Dawson James Securities"] <- "Dawson James Securities, Inc."
ipo$main_uw[ipo$main_uw == "Janney Montgomery Scott LLC"] <- "Janney Montgomery Scott Inc"
ipo$main_uw[ipo$main_uw == "Key Banc Capital Markets"] <- "KeyBanc Capital Markets"
ipo$main_uw[ipo$main_uw == "Merriman Curhan Ford & Co"] <- "Merriman Curhan Ford & Co."
ipo$main_uw[ipo$main_uw == "Morgan Stanley Dean Witter(AU)"] <- "Morgan Stanley Dean Witter"
ipo$main_uw[ipo$main_uw == "TAGLICH BROTHERS INC"] <- "Taglich Brothers, Inc."
ipo$main_uw[ipo$main_uw == "ThinkEquity Partners"] <- "Thinkequity Partners LLC"
ipo$main_uw[ipo$main_uw == "BA Securities Inc"] <- "BA Securities"
ipo$main_uw[ipo$main_uw == "Chapman Co"] <- "Chapman Company"
ipo$main_uw[ipo$main_uw == "Credit Suisse First Boston Inc"] <- "Credit Suisse First Boston"
ipo$main_uw[ipo$main_uw == "Davenport & Co LLC"] <- "Davenport"
ipo$main_uw[ipo$main_uw == "Dominick & Dominick Inc"] <- "Dominick"
ipo$main_uw[ipo$main_uw == "First Albany Capital Inc"] <- "First Albany"
ipo$main_uw[ipo$main_uw == "Hornblower, Weeks, Noyes & Tra"] <- "Hornblower Weeks Noyes & Trask"
ipo$main_uw[ipo$main_uw == "Imperial Capital LLC"] <- "Imperial-Cap"
ipo$main_uw[grep("KeyCorp", ipo$main_uw)] <- "Keycorp/McDonald Investments"
ipo$main_uw[ipo$main_uw == "Laidlaw & Co (UK) Ltd"] <- "Laidlaw Global Securities"
ipo$main_uw[ipo$main_uw == "Legg Mason & Co Inc"] <- "Legg, Mason"
ipo$main_uw[ipo$main_uw == "Merrill Lynch, Pierce, Fenner"] <- "Merrill Lynch & Co Inc"
ipo$main_uw[grep("Wells Fargo", ipo$main_uw)] <- "Wells Fargo"

match <- match(ipo$main_uw, ritter_rank$Underwriter_Name)
ipo[, UW_rank := NA]
ipo[, tmp := 1:.N]
ind <- which(ipo$Year %in% 1992:2000)
ipo$UW_rank[ind] <- ritter_rank$Rank9200[match[ind]]
ind <- which(ipo$Year %in% 2001:2004)
ipo$UW_rank[ind] <- ritter_rank$Rank0104[match[ind]]
ind <- which(ipo$Year %in% 2005:2007)
ipo$UW_rank[ind] <- ritter_rank$Rank0507[match[ind]]
ind <- which(ipo$Year %in% 2008:2009)
ipo$UW_rank[ind] <- ritter_rank$Rank0809[match[ind]]
ind <- which(ipo$Year %in% 2010:2016)
ipo$UW_rank[ind] <- ritter_rank$Rank1011[match[ind]]
ind <- which(ipo$Year %in% 2012:2016 & (is.na(ipo$UW_rank) | ipo$UW_rank == -9))
ipo$UW_rank[ind] <- ritter_rank$Rank1215[match[ind]]


ritter_2014 <- fread("./Projects/SEC Letter Project/Data After Review/Additional Data/2014 ranks.csv",
                     select = c("Underwriter_Name", "Rank1214"))
match2 <- match(ipo$main_uw, ritter_2014$Underwriter.Name)
ind <- which(ipo$Year %in% 2012:2016 & (is.na(ipo$UW_rank) | ipo$UW_rank == -9))
ipo$UW_rank[ind] <- ritter_2014$Rank1214[match2[ind]]

ipo$UW_rank[grepl("Piper Jaffray", ipo$main_uw) & ipo$Year %in% 1992:2009] <- 7.001
ipo$UW_rank[grepl("Piper Jaffray", ipo$main_uw) & ipo$Year %in% 2010:2016] <- 7.501
ipo$UW_rank[grepl("Robert W Baird & Co Inc", ipo$main_uw) & ipo$Year %in% 1992:2016] <- 7.001
ipo$UW_rank[grepl("Dawson", ipo$main_uw) & ipo$Year %in% 1992:2016] <- 2.001

ind <- which( (is.na(ipo$UW_rank) | ipo$UW_rank == -9))
ipo$UW_rank[ind] <- 0
### adding IPO age
age <- read.csv("./Projects/SEC Letter Project/Data After Review/Additional Data/age7516.csv")
match <- match(ipo$Permno, age$CRSP.perm)
ipo$founding_year <- age$Founding[match]
ipo$founding_year[ipo$Issuer == "Data Dimensions Inc"] <- 1974
ipo$founding_year[ipo$Issuer == "Abigail Adams National Bancorp"] <- 1981
ipo$founding_year[ipo$Issuer == "Beverly Bancorp,Tinley Park,IL"] <- 1996 
ipo$founding_year[ipo$Issuer == "American Bus Finl Svcs Inc"] <- 1985
ipo$founding_year[ipo$Issuer == "Meteor Industries Inc"] <- 1993
ipo$founding_year[ipo$Issuer == "Simione Central Holdings Inc"] <-  1995
ipo$founding_year[ipo$Issuer == "Century Bancshares Inc"] <- 1969
ipo$founding_year[ipo$Issuer == "Sportsmans Guide Inc"] <- 1970
ipo$founding_year[ipo$Issuer == "ProtoSource Corp"] <- 1988
ipo$founding_year[ipo$Issuer == "Flour City International Inc"] <- 1987
ipo$founding_year[ipo$Issuer == "Aztec Technology Partners Inc"] <- 1998
ipo$founding_year[grep("TeleBanc Financial Corp", ipo$Issuer)] <-  1992
ipo$founding_year[ipo$Issuer == "Mediconsult.com Inc"] <-  1996
ipo$founding_year[ipo$Issuer == "Direct Focus Inc"] <-  1986
ipo$founding_year[ipo$Issuer == "Evercel Inc"] <-  1998
ipo$founding_year[ipo$Issuer == "CyBear Inc(Andryx Corp)"] <-  1984
ipo$founding_year[grep("ThermoView Industries In", ipo$Issuer)] <-  1987
ipo$founding_year[ipo$Issuer == "Uproar Inc"] <-  "1999"
ipo$founding_year[ipo$Issuer == "Entropin Inc"] <-  1984
ipo$founding_year[grep("AT&T Wireless Services Inc", ipo$Issuer)] <-  1987
ipo$founding_year[grep("Crescent Finl", ipo$Issuer)] <-  1991
ipo$founding_year[ipo$Issuer == "Somerset Hills Bancorp"] <-  1997
ipo$founding_year[ipo$Issuer == "Q Comm International Inc"] <-  1986
ipo$founding_year[ipo$Issuer == "SpectraSite Inc"] <-  1997
ipo$founding_year[grep("Southcoast Financial Corp", ipo$Issuer)] <-  1998
ipo$founding_year[ipo$Issuer == "Zilog Inc"] <-  1974
ipo$founding_year[grep("Southern Connecticut Bancorp", ipo$Issuer)] <-  2000
ipo$founding_year[ipo$Issuer == "Pure Cycle Corp"] <-  1976
ipo$founding_year[ipo$Issuer == "Majesco Holdings Inc"] <-  1986
ipo$founding_year[grep("Beach First Natl Bancshares", ipo$Issuer)] <- 1996
ipo$founding_year[ipo$Issuer == "Franklin Credit Mgmt Corp"] <-  1988
ipo$founding_year[ipo$Issuer == "US Airways Group Inc"] <-  1939
ipo$founding_year[ipo$Issuer == "Appalachain Bancshares"] <-  1996
ipo$founding_year[ipo$Issuer == "BCB Bancorp Inc"] <-  2000
ipo$founding_year[ipo$Issuer == "Tidelands Bancshares Inc"] <-  2002
ipo$founding_year[ipo$Issuer == "Alliance Bancorp Inc of PA"] <-  1938
ipo$founding_year[ipo$Issuer == "Coleman Cable Inc"] <-  1970
ipo$founding_year[ipo$Issuer == "First Capital Bancorp"] <-  2006
ipo$founding_year[ipo$Issuer == "ZBB Energy Corp"] <-  1986
ipo$founding_year[ipo$Issuer == "MMC Energy Inc"] <-  2003
ipo$founding_year[ipo$Issuer == "Silver State Bancorp,NV"] <-  1996
ipo$founding_year[ipo$Issuer == "Meridian Interstate Bancorp"] <-  1848
ipo$founding_year[ipo$Issuer == "KIT digital Inc"] <-  1998
ipo$founding_year[ipo$Issuer == "Penn Millers Holding Corp"] <-  1999
ipo$founding_year[ipo$Issuer == "Celsius Holdings Inc"] <-  2004
ipo$founding_year[ipo$Issuer == "GenMark Diagnostics Inc"] <-  2010
ipo$founding_year[ipo$Issuer == "Zoo Entertainment Inc"] <-  1990
ipo$founding_year[ipo$Issuer == "Oconee Federal Financial Corp"] <-  1924
ipo$founding_year[ipo$Issuer == "Wolverine Bancorp Inc"] <-  1933
ipo$founding_year[ipo$Issuer == "Anchor Bancorp"] <-  2008
ipo$founding_year[ipo$Issuer == "MedQuist Holdings Inc"] <-  2007
ipo$founding_year[ipo$Issuer == "Franklin Financial Corp"] <- 2001
ipo$founding_year[ipo$Issuer == "First Connecticut Bancorp Inc"] <-  1851
ipo$founding_year[ipo$Issuer == "State Investors Bancorp Inc"] <-  1926
ipo$founding_year[ipo$Issuer == "BSB Bancorp Inc"] <-  2011
ipo$founding_year[ipo$Issuer == "Pacific Drilling SA"] <-  2006
ipo$founding_year[ipo$Issuer == "Puma Biotechnology Inc"] <-  2007
ipo$founding_year[ipo$Issuer == "United Insurance Holdings Corp"] <-  2007
ipo$founding_year[ipo$Issuer == "Imprimis Pharmaceuticals Inc"] <-  1998
ipo$founding_year[ipo$Issuer == "Atlas Financial Holdings Inc"] <-  2009
ipo$founding_year[ipo$Issuer == "XG Technology Inc"] <-  2002
ipo$founding_year[ipo$Issuer == "Pioneer Power Solutions Inc"] <-  2008
ipo$founding_year[ipo$Issuer == "Tri-County Financial Corp"] <-  1950
ipo$founding_year[ipo$Issuer == "RiceBran Technologies"] <-  1998
ipo$founding_year[ipo$Issuer == "Retrophin Inc"] <-  2008
ipo$founding_year[ipo$Issuer == "Intra-Cellular Therapies Inc"] <-  2012
ipo$founding_year[ipo$Issuer == "SMTP Inc"] <-  1998
ipo$founding_year[ipo$Issuer == "National General Holdings Corp"] <-  1939
ipo$founding_year[ipo$Issuer == "Ignyta Inc"] <-  2011
ipo$founding_year[ipo$Issuer == "Sysorex Global Holdings Corp"] <-  1984
ipo$founding_year[ipo$Issuer == "Viggle Inc"] <-  2010
ipo$founding_year[ipo$Issuer == "Franklin Financial Network Inc"] <-  2007
ipo$founding_year[ipo$Issuer == "Unique Fabricating Inc"] <-  1975
ipo$founding_year[ipo$Issuer == "Rapid7 Inc"] <-  2000
ipo$founding_year[ipo$Issuer == "Eyegate Pharmaceuticals Inc"] <-  1998
ipo$founding_year[ipo$Issuer == "Xcel Brands Inc"] <-  1989
ipo$founding_year[ipo$Issuer == "CPI Card Group Inc"] <-  2007
ipo$founding_year[ipo$Issuer == "Kura Oncology Inc"] <-  2007
ipo$founding_year[ipo$Issuer == "First Guaranty Bancshares Inc"] <-  1934
ipo$founding_year[ipo$Issuer == "AveXis Inc"] <-  2010
ipo$founding_year[ipo$Issuer == "Amer Renal Assoc Hldg Inc"] <-  1999
ipo$founding_year[ipo$Issuer == "Red Rock Resorts Inc"] <-  2015
ipo$founding_year[ipo$Issuer == "Global Water Resources Inc"] <-  2003
ipo$founding_year[ipo$Issuer == "Spring Bk Pharms Inc"] <-  2002
ipo$founding_year[ipo$Issuer == "SiteOne Landscape Supply Inc"] <-  2001
ipo$founding_year[ipo$Issuer == "Acacia Communications Inc"] <-  2009
ipo$founding_year[ipo$Issuer == "Midland States Bancorp Inc"] <-  1988
ipo$founding_year[ipo$Issuer == "Cotiviti Holdings Inc"] <-  2001
ipo$founding_year[ipo$Issuer == "Reata Pharmaceuticals Inc"] <-  2002
ipo$founding_year[ipo$Issuer == "US Foods Holding Corp"] <-  2007
ipo$founding_year[ipo$Issuer == "Atkore International Group Inc"] <-  2010
ipo$founding_year[ipo$Issuer == "Paragon Commercial Corp"] <-  1999
ipo$founding_year[ipo$Issuer == "Twilio Inc"] <- 2008
ipo$founding_year[ipo$Issuer == "AdvancePierre Foods Hldg Inc"] <-  2010
ipo$founding_year[ipo$Issuer == "Impinj Inc"] <-  2000
ipo$founding_year[ipo$Issuer == "TACTILE SYSTEMS TECHNOLOGY INC"] <-  1995
ipo$founding_year[ipo$Issuer == "Atomera Inc"] <-  2001
ipo$founding_year[ipo$Issuer == "Medpace Inc"] <-  1992
ipo$founding_year[ipo$Issuer == "Airgain Inc"] <-  1995
ipo$founding_year[ipo$Issuer == "FB Financial Corp"] <-  1984
ipo$founding_year[ipo$Issuer == "The Trade Desk Inc"] <-  2009
ipo$founding_year[ipo$Issuer == "Apptio Inc"] <-  2007
ipo$founding_year[ipo$Issuer == "elf Beauty Inc"] <-  2004
ipo$founding_year[ipo$Issuer == "Valvoline Inc"] <-  1866
ipo$founding_year[ipo$Issuer == "Camping World Holdings Inc"] <-  2016
ipo$founding_year[ipo$Issuer == "Extraction Oil & Gas Inc"] <-  2012
ipo$founding_year[ipo$Issuer == "Acushnet Holdings Corp"] <-  1910
ipo$founding_year[ipo$Issuer == "Polar Power Inc"] <-  1979
ipo$founding_year[ipo$Issuer == "SenesTech Inc"] <-  2004
### this information was missing in Ritter file
ipo$founding_year[ipo$Issuer == "Amertranz Worldwide Hldg Corp"] <- 1982
ipo$founding_year[ipo$Issuer == "K2 Design Inc"] <- 1994
ipo$founding_year[ipo$Issuer == "Integrated Technology USA Inc"] <- 1990
ipo$founding_year[ipo$Issuer == "Cragar Industries Inc"] <- 1930
ipo$founding_year[ipo$Issuer == "Amplidyne Inc"] <- 1995
ipo$founding_year[ipo$Issuer == "Paradise Music & Entertainment"] <- 1996
ipo$founding_year[ipo$Issuer == "Southwest Bancorp of Texas,TX"] <- 1989
ipo$founding_year[ipo$Issuer == "Complete Wellness Centers Inc"] <- 1994
ipo$founding_year[ipo$Issuer == "Hamilton Bancorp"] <- 1915
ipo$founding_year[ipo$Issuer == "Commodore Separation Tech"] <- 1996
ipo$founding_year[ipo$Issuer == "Capital Beverage Corp"] <- 1995
ipo$founding_year[ipo$Issuer == "Retrospettiva"] <- 1990
ipo$founding_year[ipo$Issuer == "Omega Orthodontics Inc"] <- 1996
ipo$founding_year[ipo$Issuer == "Tekgraf Inc"] <- 1997
ipo$founding_year[ipo$Issuer == "Sonic Foundry Inc"] <- 1991
ipo$founding_year[ipo$Issuer == "Ontro Inc"] <- 1994
ipo$founding_year[ipo$Issuer == "Factual Data Corp"] <- 1985
ipo$founding_year[ipo$Issuer == "audiohighway.com"] <- 1994
ipo$founding_year[ipo$Issuer == "Digital Lava Inc"] <- 1995
ipo$founding_year[ipo$Issuer == "US Laboratories Inc"] <- 1996
ipo$founding_year[ipo$Issuer == "Auriga Laboratories Inc"] <- 1996
ipo$founding_year[ipo$Issuer == "Kinder Morgan Mgmt LLC"] <- 1997
ipo$founding_year[ipo$Issuer == "Taylor Capital Group Inc"] <- 1929
ipo$founding_year[ipo$Issuer == "First State Financial Corp"] <- 1988
ipo$founding_year[ipo$Issuer == "CapitalSouth Bancorp"] <- 1990
ipo$founding_year[ipo$Issuer == "Territorial Bancorp Inc"] <- 1921
ipo$founding_year[ipo$Issuer == "OBA Financial Services Inc"] <- 1861
ipo$founding_year[ipo$Issuer == "OmniAmerican Bancorp Inc"] <- 1956
ipo$founding_year[ipo$Issuer == "FS Bancorp Inc"] <- 1936
ipo$founding_year[ipo$Issuer == "Georgetown Bancorp Inc"] <- 1868
ipo$founding_year[ipo$Issuer == "Coastway Bancorp Inc"] <- 1920
ipo$founding_year <- as.numeric(as.character(ipo$founding_year))

### F score
get_Fscore <- function(gvkeys, years)
{
  comp <- fread(paste0(yadir, "Compustat/crsp_compustat_merger_annual.csv"),
                select = c("GVKEY", "act", "che", "lct", "dlc", 
                           "at", "ivao", "lt", "dltt", "fyear",
                           "ivst", "pstk", "rect", "sale", "invt", "ppent", "ib"), nrows = 508030)
  comp <- comp[!is.na(at) & at > 0]
  comp <- comp[GVKEY %in% gvkeys]
  
  na0 <- function(x)
  {
    x[is.na(x)] <- 0
    return(x)
  }
  ### WC = [current assets total - cash and st inv] - [current liabilites - debt in current liabilities]
  comp[, WC := (na0(act) - na0(che)) - (na0(lct) - na0(dlc)) ]
 # comp$WC <- (comp$act - comp$che) - (comp$lct - comp$dlc)
  ### NCO = total assets - current assets - 
  comp[, NCO := (na0(at) - na0(act) - na0(ivao)) - (na0(lt) - na0(lct) - na0(dltt))]
 # comp$NCO <- (comp$at - comp$act - comp$ivao) - (comp$lt - comp$lct - comp$dltt)
  ### 
  comp[, FIN := (na0(ivst) + na0(ivao))-(na0(dltt) + na0(dlc) + na0(pstk))]
  #comp$FIN <- (comp$ivst + comp$ivao) - (comp$dltt + comp$dlc + comp$pstk)
  setkey(comp, GVKEY, fyear)
  
  comp[, l.WC := c(NA, WC[-.N]), by = GVKEY]
  comp[, l.NCO := c(NA, NCO[-.N]), by = GVKEY]
  comp[, l.FIN := c(NA, FIN[-.N]), by = GVKEY]
  comp[, AV_AT := mean(at, na.rm = T), by = GVKEY]

  comp$RSST_ACC <- (comp$WC - comp$l.WC + comp$NCO - comp$l.NCO + comp$FIN - comp$l.FIN)/comp$at
  
  comp[, rect := na0(rect)]
  comp[, l.rect := c(NA, rect[-.N]), by = GVKEY]
  comp$CH_REC <- (comp$rect - comp$l.rect)/comp$at
  
  comp[, l.invt := c(NA, invt[-.N]), by = GVKEY]
  comp$CH_INV <- (comp$invt - comp$l.invt)/comp$at
  
  comp$SOFT_ASSET <- (comp$at - comp$ppent - comp$che)/comp$at
  
  comp$cash_sale <- comp$sale - (comp$rect - comp$l.rect)
  comp[, l.cash_sale := c(NA, cash_sale[-.N]), by = GVKEY]
  comp$CH_CS <- comp$cash_sale/comp$l.cash_sale - 1
  
  comp[, l.at := c(NA, at[-.N]), by = GVKEY]
  comp[, l.ib := c(NA, ib[-.N]), by = GVKEY]
  comp$CH_EARN <- comp$ib/comp$at - comp$l.ib/comp$l.at
  
  
  comp$ISSUE <- 0
  comp$ISSUE[comp$sstk > 0 | comp$dltis > 0 ] <- 1
  
  require(psych)
  comp$predict_value <-winsor( -7.893 + 0.790*comp$RSST_ACC + 2.518*comp$CH_REC + 1.191*comp$CH_INV +
                                 1.979*comp$SOFT_ASSET + 0.171*comp$CH_CS - 0.932*comp$CH_EARN + 1.029*comp$ISSUE, 0.0)
  
  comp$predict_prob <- exp(comp$predict_value)/(1+exp(comp$predict_value))
  comp$F_score <- winsor(comp$predict_prob/0.0037, 0.0)
  comp[, ID := paste(GVKEY, fyear)]
  all_ID <- paste(ipo$Gvkey, ipo$Year)
  match <- match(all_ID, comp$ID)
  return(comp$F_score[match])
}

ipo$F_score <- get_Fscore(ipo$Gvkey, ipo$Year)

### loading daily CRSP
file_crsp <- paste0(dir_add, "crsp.rds")
if(file.exists(file_crsp)) crsp <- readRDS(file_crsp)
if(!file.exists(file_crsp))
{
  crsp <- NULL
  for(cyear in 1996:2016)
  {
    print(cyear)
    tmp <- fread(paste0(yadir, "CRSP/DSF/CRSP_DSF_", cyear, ".csv"),
                 select = c("PERMNO", "date", "RET"))
    tmp <- tmp[PERMNO %in% ipo$Permno]
    tmp[,`:=`(date = ymd(date), RET = as.numeric(as.character(RET)))] 
    tmp <- tmp[!is.na(RET)]
    
    match <- match(tmp$PERMNO, ipo$Permno)
    tmp$ipo_date <- ymd(ipo$Issue_date[match])
    tmp[, dif := date - ipo_date]
    tmp <- tmp[dif <= 365*3]
    crsp <- rbind(crsp, tmp)
  }
  rm(tmp)
  setkey(crsp, PERMNO, date)
  crsp[, `:=`(ipo_date = NULL, dif = NULL)]
  saveRDS(crsp, file_crsp)
}

### Loading compustat
comp <- fread(paste0(yadir, "Compustat/crsp_compustat_merger_annual.csv"), 
              select = c("LPERMNO", "fyear", "fyr", "pstkl", "pstkrv", "pstk", "seq",
                         "ceq", "at", "lt", "txditc", "dcvt", "consol", "indfmt", "datafmt", "popsrc"))

### Loading CRSP monthly
crsp_month <- fread(paste0(yadir, "CRSP/MSF/CRSP_MSF.csv"),
                    select = c("PERMNO", "date","SHRCD", "EXCHCD","PRC", "RET", "CUSIP", "SHROUT"))
crsp_month[, date := ymd(date)]
crsp_month[, `:=` (month = month(date), year = year(date))]
#### calcualting BM for IPOs and matching
clean_comp <- function(comp, real_year)
{
  comp <- comp[!is.na(LPERMNO)]
  comp <- comp[ fyr > 0 & at > 0]
  comp[, year := fyear]
  comp[fyr %in% 1:5, year := fyear + 1]
  comp <- comp[year == real_year]
  comp[, pstock := pstkl]
  comp[is.na(pstock), pstock := pstkrv]
  comp[is.na(pstock), pstock := pstk]
  comp[is.na(pstock), pstock := 0] ### is it right?
  comp[, se := seq]
  comp[is.na(se)&!is.na(ceq)&!is.na(pstk), se := ceq + pstk]
  comp[is.na(se), se := at - lt]
  comp[is.na(txditc), txditc := 0]
  comp[is.na(dcvt), dcvt := 0]
  comp[, be := se - pstock + txditc + dcvt]
  comp <- comp[!is.na(be)]
  return(comp)
}

### estimating december size and book-to-market
for(cyear in 1996:2016)
{
  print(cyear)
  
  ind <- which(ipo$Year == cyear)
  ### matching book value
  ### current year
  comp_year <- clean_comp(comp, cyear)
  match <- match(ipo$Permno[ind], comp_year$LPERMNO)
  ipo$be[ind] <- comp_year$be[match]
  ### next year
  comp_year <- clean_comp(comp, cyear + 1)
  match <- match(ipo$Permno[ipo$Year == cyear & is.na(ipo$be)], comp_year$LPERMNO)
  ipo$be[ipo$Year == cyear & is.na(ipo$be)] <- comp_year$be[match]
  
  ### matching size in december
  crsp_year <- crsp_month[year == cyear]
  crsp_year <- crsp_year[month == 12]
  crsp_year <- crsp_year[!duplicated(PERMNO)]
  crsp_year[, size := abs(PRC)*SHROUT]
  match <- match(ipo$Permno[ind], crsp_year$PERMNO)
  ipo$size_dec[ind] <- crsp_year$size[match]
  
  ### crsp should have all info
  match <- match(crsp_year$PERMNO, comp_year$LPERMNO)
  crsp_year$BE <- comp_year$be[match]
  crsp_year[, BM := 10^3*BE/size]
  
  ipo$BM[ind] <- 10^3*ipo$be[ind]/ipo$size[ind]
}
ipo[is.na(BM) | BM < 0, BM:= 0] ### two companies have missing values

### load FF-portfolio quantiles
n <- function(x) return(as.numeric(as.character(x)))
tmp <- fread("./CRSP_COMP/ME_Breakpoints.CSV")
quant <- data.table(date = n(tmp$V1), sizebp1 = n(tmp$V6), sizebp2 = n(tmp$V10), sizebp3 = n(tmp$V14), sizebp4 = n(tmp$V18))
quant[, `:=` (pyear = substr(date, 1, 4), month = as.numeric(as.character(substr(date,5, 6))))]
quant <- quant[pyear >= 1995 & month == 12]

tmp <- fread("./CRSP_COMP/BE-ME_Breakpoints.CSV")
match <- match(quant$pyear, tmp$V1)
quant[, `:=` (bmbp1 = n(tmp$V7[match]), bmbp2 = n(tmp$V11[match]), bmbp3 = n(tmp$V15[match]), bmbp4 = n(tmp$V19[match]))]

### estimating portfolio size and BM
for(cyear in 1995:2016)
{
  print(cyear)
  ind <- which(ipo$Year == cyear)
  line <- quant[pyear == cyear]
  line[, `:=` (sizebp1 = 10^3*sizebp1, sizebp2 = 10^3*sizebp2, sizebp3 = 10^3*sizebp3, sizebp4 = 10^3*sizebp4)]
  
  size1 <- which(ipo$Year == cyear & ipo$size_dec <= line$sizebp1)
  size2 <- which(ipo$Year == cyear & ipo$size_dec > line$sizebp1 & ipo$size_dec <= line$sizebp2)
  size3 <- which(ipo$Year == cyear & ipo$size_dec > line$sizebp2 & ipo$size_dec <= line$sizebp3)
  size4 <- which(ipo$Year == cyear & ipo$size_dec > line$sizebp3 & ipo$size_dec <= line$sizebp4)
  size5 <- which(ipo$Year == cyear & ipo$size_dec > line$sizebp4)
  
  bm1 <- which(ipo$Year == cyear & ipo$BM <= line$bmbp1)
  bm2 <- which(ipo$Year == cyear & ipo$BM > line$bmbp1 & ipo$BM <= line$bmbp2)
  bm3 <- which(ipo$Year == cyear & ipo$BM > line$bmbp2 & ipo$BM <= line$bmbp3)
  bm4 <- which(ipo$Year == cyear & ipo$BM > line$bmbp3 & ipo$BM <= line$bmbp4)
  bm5 <- which(ipo$Year == cyear & ipo$BM > line$bmbp4)
  ipo$portfolio_size[size1] <- 1
  ipo$portfolio_size[size2] <- 2
  ipo$portfolio_size[size3] <- 3
  ipo$portfolio_size[size4] <- 4
  ipo$portfolio_size[size5] <- 5
  
  ipo$portfolio_bm[bm1] <- 1
  ipo$portfolio_bm[bm2] <- 2
  ipo$portfolio_bm[bm3] <- 3
  ipo$portfolio_bm[bm4] <- 4
  ipo$portfolio_bm[bm5] <- 5
}
ipo$port_25 <- (ipo$portfolio_size - 1)*5 + ipo$portfolio_bm

match <- match(crsp$PERMNO, ipo$Permno)
crsp$port_25 <- ipo$port_25[match]

### load FF returns
port25 <- fread("./CRSP_COMP/25_Portfolios_5x5_Daily.CSV", header = F)
port25[, date := ymd(V1)]
port25[, year := year(date)]
port25 <- port25[ year > 1994]
port25 <- as.data.frame(port25)
for(i in 2:26) port25[,i] = n(port25[,i])/100

### adding FF returns to CRSP
for(i in 1:25)
{
  print(i)
  print(Sys.time())
  ind <- which(crsp$port_25 == i)
  print(length(ind))
  match <- match(ymd(crsp$date[ind]),port25$date)
  crsp$ret_port25[ind] <- port25[match,i + 1]
}

setkey(crsp, PERMNO, date)
crsp[, date := ymd(date)]
crsp[, dif := 1:.N, by = PERMNO]
crsp[, vol := sqrt(252)*sd(RET[dif %in% 22:274]), by = PERMNO]
crsp[, ret_175 := prod(1+RET[dif %in% 0:126]), by = PERMNO]
crsp[, ret_365 := prod(1+RET[dif %in% 0:252]), by = PERMNO]
crsp[, ret_port_175 := prod(1+ret_port25[dif %in% 0:126]), by = PERMNO]
crsp[, ret_port_365 := prod(1+ret_port25[dif %in% 0:252]), by = PERMNO]
crsp[, `:=`(ret_abn_175 = ret_175 - ret_port_175, ret_abn_365 = ret_365 - ret_port_365)]

ipo <- as.data.table(ipo)
match <- match(ipo$Permno, crsp$PERMNO)
ipo[, `:=` (ret_175 = crsp$ret_175[match], ret_365  = crsp$ret_365[match], ret_abn_175 = crsp$ret_abn_175[match],
            ret_abn_365 = crsp$ret_abn_365[match], vol = crsp$vol[match])]
ipo[, `:=` (Type = NULL, Law_firm = NULL, Lawyers_code = NULL, Lawyers_name = NULL, CUSIP = NULL, CUSIP9 = NULL,
            Date1 = NULL, tmp = NULL, be = NULL, size_dec = NULL, portfolio_size = NULL, portfolio_bm = NULL)]

### price update
sdc_history <- data.table(read.csv("./Projects/SEC Letter Project/Data After Review/Additional Data/Amend_history.csv"))
for(i in 2:length(sdc_history$Amendment_Date))
{
  if(is.na(sdc_history$Deal_number[i])) sdc_history$Deal_number[i] <- sdc_history$Deal_number[i-1]
}
sdc_history[, date := mdy(Amendment_Date)]
setkey(sdc_history, Deal_number, date)
sdc_history[, amend_n := 1+1:.N, by = Deal_number]
sdc_history[, mean_range := 0.5*(High_Price + Low_Price)]

updates <- sdc_history[!is.na(mean_range)]
m <- match(ipo$Deal_number, as.numeric(as.character(updates$Deal_number)))
ipo[, `:=` (mid_range = updates$mean_range[m], low_range = updates$Low_Price[m], high_range = updates$High_Price[m], PRange_date = updates$date[m])]
ipo$price_update <- 100*(ipo$Offer_Price/ipo$mid_range - 1)
ipo[is.na(price_update)|is.infinite(price_update), price_update := 0]

to_n <- function(x) return(as.numeric(as.character(gsub(",","",x))))

law <- read.csv(paste0(dir_add, "add_price_law.csv"))
match <- match(ipo$Deal_number, law$X.Deal.Number)
ipo$Law_firm <- law$Issuer.Legal.Advisors.Code[match]

get.law.firm.rank <- function(ipo)
{
  ipo$Issue_date <- ymd(ipo$Issue_date)
  for(yr in 2004:2016)
  {
    ind <- which(ipo$Year == yr)
    tmp <- ipo[Year %in% (yr-5):(yr-1), list(n_ipos = length(Issue_date)), by = Law_firm]
    m <- match(ipo$Law_firm[ind], tmp$Law_firm)
    ipo$Law.firm.5years[ind] <- tmp$n_ipos[m]
  }
  ipo$Law.firm.5years[is.na(ipo$Law.firm.5years)] <- 0
  return(ipo)
}
ipo <- get.law.firm.rank(ipo)


### insider transaction
file_insider <- paste0(dir_add, "insider.rds")
if(file.exists(file_insider)) insider <- readRDS(file_insider)
if(!file.exists(file_insider))
{
  insider <- NULL
  for(cyear in 1996:2017)
  {
    print(cyear)
    tmp <- fread(paste0(yadir, "Insider/Table_1_", cyear, ".csv"),
                 select = c("CUSIP6", "CUSIP2", "PERSONID", "TRANDATE_AR", "SHARES"))
    tmp <- tmp[CUSIP6 %in% substr(ipo$CUSIP8, 1, 6)]
    tmp[,`:=`(date = ymd(TRANDATE_AR), SHARES = as.numeric(as.character(SHARES)))] 
    tmp <- tmp[!is.na(SHARES)]
    tmp[, CUSIP8 := paste0(CUSIP6, CUSIP2)]
    
    match <- match(tmp$CUSIP8, ipo$CUSIP8)
    tmp$ipo_date <- ymd(ipo$Issue_date[match])
    tmp[, dif := date - ipo_date]
    tmp <- tmp[dif %in% 0:200]
    insider <- rbind(insider, tmp)
  }
  rm(tmp)
  setkey(insider, CUSIP6, date)
  saveRDS(insider, file_insider)
}

insider <- insider[dif %in% 170:190]
ipo$insider_sales <- 0
ipo$insider_sales[ipo$CUSIP8 %in% insider$CUSIP8] <- 1

ipo[, `:=` (age = log(1 + Year - founding_year), IR = Close_price1/ipo$Offer_Price - 1)]

### mean initial returns before IPO
long_ipo <- fread("./Projects/IPO review chapter/Chapter write up/Data 20170315/ipo.csv")
long_ipo[, `:=` (IR = Close_price1/Offer_Price - 1, date = ymd(Issue_date))]
for(i in 1:length(ipo$Filing_date))
{
  if(i %% 100 == 0) print(i)
  date <- ymd(ipo$Issue_date[i])
  ind <- which(long_ipo$date >= date - 30 & long_ipo$date <= date)
  mean_IR <- mean(long_ipo$IR[ind], na.rm = T)
  ipo$mean_IR[i] <- mean_IR
}

### JOBS act dummy
if(file.exists(jobs_act))
{
  df <- read.csv(jobs_act)
  ipo$JOBS <- df$JOBS[match(ipo$Deal_number, df$Deal_number)]
}
if(!file.exists(jobs_act))
{
  jobs_date <- ymd("20120405") ### this when JOBS act became active
  ipo[, Proceeds := to_n(Proceeds)]
  ind <- which(ymd(ipo$Issue_date) >= jobs_date & ipo$Proceeds <= 1000)
  
  ipo[, JOBS := 0]
  for(i in ind)
  {
    print(i)
    prosp <- readLines(paste0(dir_s1, ipo$Cik_SDC[i], ".txt"))
    
    if(length(grep("(JOBS|EGC|Jumpstart)", prosp)) > 0) ipo$JOBS[i] <- 1
  }
  df <- data.frame(Deal_number = ipo$Deal_number, JOBS = ipo$JOBS)
  write.csv(df, jobs_act, row.names = F)
}


### number of segments
comp_seg <- fread(paste0(yadir, "Compustat/compustat_monthly_updated_historical_segments.csv"),
                  select = c("gvkey", "datadate", "snms"))
comp_seg[, year := substr(datadate, 1, 4)]
comp_seg <- comp_seg[year > 1994 & gvkey %in% ipo$Gvkey]
comp_seg[, n_seg := length(unique(snms)), by = c("gvkey", "year")]
comp_seg[, id := paste0(gvkey, year)]

match <- match(paste0(ipo$Gvkey, ipo$Year), comp_seg$id)
ipo$n_segments <- comp_seg$n_seg[match]
ipo$n_segments[is.na(ipo$n_segments)] <- 1

### Check CIK to SIC codes
sics_codes <- fread("./Projects/SEC Letter Project/Data After Review/Sorting SEC letters/All_CIKs.csv")
match <- match(ipo$Cik_SDC, to_n(sics_codes$CIK))
ipo$sic <- to_n(sics_codes$SIC[match])

match_FF <- function(sic_codes, n_ind = 48)
{
  require(stringr)
  wd <- getwd()
  
  #### make a table
  if(n_ind == 48) ffind <- readLines("./CRSP_COMP/Siccodes48.txt")
  if(n_ind == 12) ffind <- readLines("./CRSP_COMP/Siccodes12.txt")
  indtable <- NULL
  
  for(i in 1:length(ffind))
  {
    str <- ffind[i]
    if(str_length(str) == 0) next
    if (grepl("\\b(\\d|\\d\\d)\\b", str, perl = T))
    {
      res <- regexpr("\\b(\\d|\\d\\d)\\b", str, perl = T)
      ind <- regmatches(str, res)
      print(ind)
    }
    else
    {
      res <- regexpr("\\b(?<year>\\d\\d\\d\\d)(?=-)", str, perl = T)
      begin <- regmatches(str, res)
      res <- regexpr("(?<=-)(?<year>\\d\\d\\d\\d)\\b", str, perl = T)
      end <- regmatches(str, res)
      indtable <- rbind(indtable, data.frame(begin = begin, end = end, ind = ind))
    }
  }
  table <- NULL
  table$sic <- as.numeric(as.character(sic_codes))
  table$FF <- NA
  table <- as.data.frame(table)
  
  ### actual matching
  for(i in 1:length(indtable$begin))
  {
    start <- as.numeric(as.character(indtable$begin[i]))
    end <- as.numeric(as.character(indtable$end[i]))
    index <- which(table$sic >= start & table$sic<= end)
    table$FF[index] <- indtable$ind[i]
  }
  setwd(wd)
  return(table$FF)
}

ipo$FF_48 <- match_FF(ipo$sic, n_ind = 48)
ipo$FF_48[is.na(ipo$FF_48)] <- 48

ipo$FF_12 <- match_FF(ipo$sic, n_ind = 12)
ipo$FF_12[is.na(ipo$FF_12)] <- 12
save(list = ls(all=TRUE), file = "./Projects/SEC Letter Project/Data After Review/Rdata/IPO Variables.RData")

write.csv(ipo, "./Projects/SEC Letter Project/Data After Review/ipo_20170510.csv", row.names = F)
print(Sys.time())
