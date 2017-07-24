require(tm)
require(topicmodels)
require(data.table)
require(tidytext)
require(tidyr)
require(dplyr)
require(ggplot2)
#############################################
#########   Loading Data    #################
#############################################
load("./Projects/SEC Letter Project/Data After Review/Rdata/lda_20170710_8_topics.RData")
words <- as.matrix(dtm)
### word's frequency
p_w <- colSums(words)/sum(words)
### average topic frequency
p_T <- colMeans(as.data.frame(ldaOut@gamma))

tmp <- data.table(tidy(ldaOut, matrix = "beta"))
p_w_T <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))

for(i in 1:k) p_w_T[,i] <- tmp[topic == i]$beta

p_T_w <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
for(i in 1:k) p_T_w[i] <- p_w_T[i]*p_T/(p_w)

tmp <- as.data.frame(matrix(,length(tmp[topic == 1]$beta),k))
for(i in 1:k) tmp[i] <- p_T_w[i]*log(p_T_w[i]/p_T[i])

distinctive_w <- tmp
saliency_w <- p_w*distinctive_w
rownames(saliency_w) <- labels(p_w)

i <- 1

for(i in 1:k)
{
  vocab <- labels(p_w)
  df1 <- data.frame(topic = "1. frequency", term = as.character(vocab), beta = p_w_T[,i])
  df2 <- data.frame(topic = "2. saliency", term = vocab, beta = saliency_w[,i])
  ap_topics <- rbind(df1, df2)
  
  df <- df1
  
  make_one_plot <- function(df, topic, panel)
  {
    if(panel == 1) 
    {
      clr <- "#CC6666"
      namey <- paste0("Topic ", topic, " Panel A")
    }
    
    if(panel == 2)
    {
      clr <- "#9999CC"
      namey <- paste0("Topic ", topic, " Panel B")
    }
    
    top_terms <- df %>%
      group_by(topic) %>%
      top_n(15, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
    
    x <- top_terms %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta, fill = topic)) +
      geom_col(show.legend = FALSE) +
      facet_wrap( ~ topic, scales = "free_y") +
      labs(y = namey, x = NULL) + 
      scale_fill_manual(values=clr) +
      coord_flip() + theme_set(theme_gray(base_size = 20))
    return(x)
  }
  
  
  x1 <- make_one_plot(df1, i, 1)
  x2 <- make_one_plot(df2, i, 2)
  
  require(gridExtra)
  grid.arrange(x1, x2, ncol = 2)
  
  name_plot <- paste0("./Projects/SEC Letter Project/Data After Review/Additional Data/Topic Plots/20170710/", i, ".png")
  png(filename=name_plot, width = 800, height = 400)
  grid.arrange(x1, x2, ncol = 2)
  dev.off()
  
}

### plotting top companies
upload <- fread("./Projects/SEC Letter Project/Data After Review/upload_ipo.csv")
probs <- as.data.frame(ldaOut@gamma)
probs$company_name <- upload$Company_Name
probs$letter_number <- upload$letter_number

k <- 8

ind <- order(probs[,k,], decreasing = T)
View(probs$company_name[ind[1:10]])

p_T
