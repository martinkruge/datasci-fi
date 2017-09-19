
library(shiny)
library(shinythemes)
library(ggplot2)

source("global.R")


PAGE_TITLE <- "US Consumer Financial Protection Bureau"
CWD <- getwd()

loadData(CWD)

# Define UI for application 
ui <- fluidPage( theme = shinytheme("flatly"),
            
            # Application title
            headerPanel( windowTitle = PAGE_TITLE,
                title =
                div(  
                      h1(align="center", style = "font-weight: bold;color:#27ae60;", PAGE_TITLE),
                      h4(align="center", "Text Mining of Financial Product Complaints for the period 2015 to 2016"),
                      img(style="vertical-align:middle;", src="cfpb_building_logo.png", width="150", height="100")
                   )              
            ),
   br(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( width = 2,
         
        checkboxGroupInput(inputId = "prodSelect", label = "Select products:", choices = products, selected = products ),
        
        checkboxGroupInput(inputId = "compSelect", label = "Select compensated:", 
                           choiceNames = c("Yes","No"), choiceValues = c(TRUE,FALSE),
                           selected = c(T,F), inline=T ),
        
        sliderInput("topicSelect", "Number of topics:", min = 2, max = 5, value = 3)
      
      ),
      
      # Show plots
      mainPanel(
        tabsetPanel(
          tabPanel( "Sentiment analysis", 
                    #CWD
                    plotOutput("sentimentHistPlot")
          ),
          
          tabPanel( "Topic modelling", 
                    "Tab for topic modelling"
                    
                    #plotOutput("distPlot")
          )
      )
   )
)
)


# Define server logic 
server <- function(input, output) {
   
  
   output$distPlot <- renderPlot({
      
     # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$topicSelect + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$sentimentHistPlot <- renderPlot({
     
     # generate histogram based on input$prodSelect, input$compSelect from ui
     sentiment.summary %>%
       filter(product %in% input$prodSelect) %>%
       filter(compensated %in% input$compSelect) %>%
       ggplot(aes(x = net_sentiment, y = count, fill = product)) + 
       geom_col() + # show.legend = FALSE
       ggtitle("Sentiment histogram by product") +
       labs(x = "Sentiment", y = "Frequency") 
       #+facet_wrap(~product+compensated , ncol = 2, scales = "free") 
     
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

