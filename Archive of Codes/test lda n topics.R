#require(RTextTools)
require(topicmodels)
require(tm)
clean_letters_files <- "C:/Users/ev99/Dropbox/Projects/SEC Letter Project/Data After Review/upload_ipo.csv"
upload <- fread(clean_letters_files, select = c("out_filename", "Clean_Body", "letter_number"))
docs <- Corpus(VectorSource(upload$Clean_Body))
docs <- tm_map(docs, removeWords, c(stopwords("english"), "please", "page", "comment"))
docs <- tm_map(docs, stripWhitespace)
require(SnowballC)
docs <- tm_map(docs,stemDocument)
dtm <- DocumentTermMatrix(docs)


### LDA function
test_LDA <- function(k) 
{
  print(k)
  print(Sys.time())
  out <- LDA(dtm[ind,], k = k, method = "Gibbs",
      control = list(burnin = burnin,iter = iter, keep = keep))
  return(out)
}

### random sample to check  
ind <- sample(1:length(upload$Clean_Body), 100)
### searching here
seqk <- seq(2, 25, 1)

### paraments
burnin <- 400
iter <- 600
keep <- 200
print(Sys.time())
info <- system.time(fitted_many <- lapply(seqk, test_LDA))
print(info)

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

harmonicMean <- function(logLikelihoods, precision = 2000L) 
{
  require(Rmpfr)
  llMed <- median(logLikelihoods)
  out <- as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
  return(out)
}

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))
print(seqk[which.max(hm_many)])

require(ggplot2)
daplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  annotate("text", x = 30, y = -1300000, label = paste("The optimal number of topics is", 
                                                     seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", 
                          atop(italic("How many distinct topics in the abstracts?"), ""))))

daplot
#save(list = ls(all=TRUE), file = "./Projects/SEC Letter Project/Data After Review/Rdata/Search n topics.RData")
