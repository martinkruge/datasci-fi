library(ggplot2)

# create some data
x <- runif(100)
# save as RData file
save(x, file = "data/randomnumbers.RData")

# some workings
y <- 2*x 
mydata <- data.frame(y = y, x = x)
xyplot <- ggplot(mydata, aes(x = x, y = y)) + geom_point()
ggsave("output/xyplot.png", xyplot, width = 7, height = 6, dpi = 200)
