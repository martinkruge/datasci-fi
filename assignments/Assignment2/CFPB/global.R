
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
   #print(cwd)
   load(paste0(cwd,"/output/CFPB_sentiment.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_tidy.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_LDA.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_LDA_k_beta.RData"), globalenv())
   load(paste0(cwd,"/output/CFPB_LDA_k_gamma.RData"), globalenv())
}


# Function to calculate sentiment of a string
str.Sentiment <- function(string, replace_reg, lexicon) {
  
  str.tib <- tibble(id = 0, text = as.character(string))
  
  # Create tidy data by unnesting by words
  net_sentiment <- str.tib %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(word, text, token = "words") %>%
    group_by(id, word) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    left_join(get_sentiments(lexicon)) %>% # add sentiments (pos or neg)
    select(word, sentiment, everything()) %>%
    mutate(sentiment = ifelse(is.na(sentiment), "neutral", sentiment)) %>%
    filter(sentiment %in% c("positive","negative")) %>%
    group_by(id) %>%
    summarise(net_sentiment = (sum(sentiment == "positive") - sum(sentiment == "negative"))) %>%
    ungroup() %>%
    select(net_sentiment) %>% 
    as.numeric()  # return numeric sentiment value
  
}


# Function to calculate the Negated sentiment of a string
str.NegSentiment <- function(string, replace_reg, lexicon, negation_words) {
  
  str.tib <- tibble(id = 0, text = as.character(string))
  
  # Create bigrams to handle negation
  # Negate sentiment where relevant and compute neg.net_sentiment
  neg.net_sentiment <- str.tib %>%
    mutate(text = str_replace_all(text, replace_reg, "")) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    
    # add sentiment for word 1
    left_join(get_sentiments(lexicon), by = c(word1 = "word")) %>%
    rename(sentiment1 = sentiment) %>%
    mutate(sentiment1 = ifelse(is.na(sentiment1), "neutral", sentiment1)) %>%
    
    # add sentiment for word 2
    left_join(get_sentiments(lexicon), by = c(word2 = "word")) %>%
    rename(sentiment2 = sentiment) %>%
    mutate(sentiment2 = ifelse(is.na(sentiment2), "neutral", sentiment2)) %>%
    select(word1,word2,sentiment1,sentiment2,everything()) %>%
    
    # create a variable that is the opposite of sentiment2
    mutate(opp_sentiment2 = recode(sentiment2, "positive" = "negative",
                                   "negative" = "positive",
                                   "neutral" = "neutral")) %>%
    
    # reverse sentiment2 if word1 is a negation word
    mutate(sentiment2 = ifelse(word1 %in% negation_words, opp_sentiment2, sentiment2)) %>%
    # remove the opposite sentiment variable, which we don't need any more
    select(-opp_sentiment2) %>%
    mutate(net_sentiment = (sentiment1 == "positive") + (sentiment2 == "positive") - 
             (sentiment1 == "negative") - (sentiment2 == "negative")) %>%
    #unite(bigram, word1, word2, sep = " ", remove = FALSE) %>% 
    filter(!word2 %in% stop_words$word) %>% # remove cases where word2 is a stop word
    filter(word1 %in% negation_words) %>% # only keep cases where negation took place
    group_by(id) %>%
    summarise(net_sentiment = sum(net_sentiment)) %>%
    ungroup() %>%
    select(net_sentiment) %>% 
    as.numeric()
  
  neg.net_sentiment <- ifelse(is.na(neg.net_sentiment), 0, 2*neg.net_sentiment) # return numeric negated sentiment value
}


# Function to calculate the Net sentiment of a string (Sentiment + negated sentiment)
str.NetSentiment <- function(string, replace_reg, lexicon, negation_words) {
  str.Sentiment(string, replace_reg, lexicon) + 
    str.NegSentiment(string, replace_reg, lexicon, negation_words)
}


# TopN terms from LDA analysis per topic (Beta)
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
