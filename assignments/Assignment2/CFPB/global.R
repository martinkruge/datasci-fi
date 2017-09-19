
loadData <- function (cwd) {
   print(cwd)
   
   load(paste0(cwd,"/output/CFPB_sentiment.RData"), globalenv())
}

