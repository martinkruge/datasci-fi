
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(tidyverse)
library(tidytext)
library(tidyr)
library(dplyr)
library(stringr)
library(scales)
library(topicmodels)


# Function to load pre-processed data needed from .RData
loadData <- function (cwd) {
   print(cwd)
   
   load(paste0(cwd,"/output/CFPB_sentiment.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_LDA.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_LDA_k_beta.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_LDA_k_gamma.RData"), globalenv())
}


topN.Beta <- function (lda_vem, N) {
   
  topics <- tidy(lda_vem, matrix = "beta")

  # TopN terms per topic 
  top_terms <- topics %>%  group_by(topic) %>%  top_n(N, beta) %>%  ungroup() %>%  arrange(topic, -beta)
  
  return(top_terms)
}




# Function to perform topic LDA on tidy complaints
topic.LDA <- function(k, tidy.complaints) {

  complaints_tdf <- tidy.complaints %>%
    filter(!word %in% stop_words$word, str_detect(word, "^[a-z]")) %>% # Remove stopwords and non-words
    group_by(id, word) %>%
    count() %>%  
    ungroup()

  complaints_dtm <- complaints_tdf %>% 
    cast_dtm(id, word, n)    

  set.seed(790904)
  
  # Perform LDA with k topics 
  complaints_lda <- LDA(complaints_dtm, k = k)  
  
  return(complaints_lda)
}



# Function to align graphs
# Source: https://stackoverflow.com/questions/26159495/align-multiple-ggplot-graphs-with-and-without-legends
AlignPlots <- function(...) {
  LegendWidth <- function(x) x$grobs[[8]]$grobs[[1]]$widths[[4]]
  
  plots.grobs <- lapply(list(...), ggplotGrob)
  
  max.widths <- do.call(unit.pmax, lapply(plots.grobs, "[[", "widths"))
  plots.grobs.eq.widths <- lapply(plots.grobs, function(x) {
    x$widths <- max.widths
    x
  })
  
  legends.widths <- lapply(plots.grobs, LegendWidth)
  max.legends.width <- do.call(max, legends.widths)
  plots.grobs.eq.widths.aligned <- lapply(plots.grobs.eq.widths, function(x) {
    if (is.gtable(x$grobs[[8]])) {
      x$grobs[[8]] <- gtable_add_cols(x$grobs[[8]],
                                      unit(abs(diff(c(LegendWidth(x),
                                                      max.legends.width))),"mm"))
    }
    x
  })
  
  plots.grobs.eq.widths.aligned
}
