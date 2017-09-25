
library(shiny)
library(shinythemes)

source("global.R")

# Global variables
PAGE_TITLE <- "  US Consumer Financial Protection Bureau"
CWD <- getwd()
replace_reg <- "X{4}[/:]?|X{2}[/:]?|[^A-Za-z\\d\\s]"  # Regex for text to be removed, like XXXX, XX/
negation_words <- c("not", "no", "never", "without")

# Load data from .RData
loadData(CWD)


# Define UI for application
#---------------------------
ui <- fluidPage( theme = shinytheme("flatly"),
            
            # Application title
            headerPanel( windowTitle = PAGE_TITLE,
                title =
                div(  
                     img(src="cfpb_building_logo.png", width="170", height="120", class = "pull-left"),
                     h1(align = "center", style = "font-weight: bold;color:#27ae60;", PAGE_TITLE),
                     h4(align = "center", "Text Mining of Financial Product Complaints for the period 2015 to 2016")
                   )              
            ),
   br(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel( width = 2,
 
        checkboxGroupInput(inputId = "prodSelect", label = "Select products:", choices = products, selected = products ),
        
        br(),
        checkboxGroupInput(inputId = "compSelect", label = "Select compensated:", 
                           choiceNames = c("Yes","No"), choiceValues = c(TRUE,FALSE),
                           selected = c(T,F), inline=T ),
        
        br(),
        sliderInput(inputId = "topicSelect", label = "Number of topics:", min = 2, max = 5, value = 3),
        checkboxInput(inputId = "ldaSelect", label = "Re-run LDA (!runs long)", value = FALSE),
        br(),

        textAreaInput(inputId = "userTextIn", label = "New complaint:", value = "", width = "250px", height = "200px"),
        actionButton(inputId = "saveText", label = "Save", icon = NULL)
      
      ),
      
      # Show plots
      mainPanel(
        tabsetPanel(
          tabPanel( "Sentiment analysis", 
                    #"Tab for sentiment analysis",
                    br(),
                    h4("Sentiment Histograms:"),
                    
                    br(),
                    plotOutput("sentimentHistPlot")
          ),
          
          tabPanel( "Topic modelling", 
                    #"Tab for topic modelling",
                    br(),
                    h4("Most frequent words used by each topic:"),
                    
                    br(),
                    plotOutput("topicProbPlot")
                    
          ),
          
          tabPanel( "New complaint", 
                    #"Tab for user complaint input",
                    br(),
                    h4("New complaint sentiment:"),

                    br(),
                    #textOutput(outputId = "userTextOut") 
                    uiOutput(outputId = "userTextOut")
          )
      )
   )
)
)


# Define server logic 
#---------------------------------------------------------------------------------------------
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      
     # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$topicSelect + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   output$sentimentHistPlot <- renderPlot({
     
     
     g1 <- sentiment_per_complaint %>%
       filter(product %in% input$prodSelect) %>%
       filter(compensated %in% input$compSelect) %>%
       ggplot(aes(x=net_sentiment.new, fill=product, color=product)) + 
       ggtitle("Sentiment histogram by product") +
       geom_histogram(position = "stack", binwidth = 1, alpha=.3) +
       xlim(c(-30,30)) + #ylim(c(0,1000)) +
       xlab("Sentiment") + 
       ylab("Complaint count") 
     
     g2 <- sentiment_per_complaint %>%
       filter(product %in% input$prodSelect) %>%
       filter(compensated %in% input$compSelect) %>%
       ggplot(aes(x=net_sentiment.new, fill=compensated, color=compensated)) + 
       ggtitle("Sentiment histogram by compensated") +
       geom_histogram(position = "stack", binwidth=1, alpha=.3) +
       xlim(c(-30,30)) + #ylim(c(0,3500)) +
       xlab("Sentiment") + 
       ylab("Complaint count") 
     
     
     plots <- AlignPlots(g1, g2)
     do.call(grid.arrange, plots)

   })
   
   output$topicProbPlot <- renderPlot({
       
       N <- 15
       complaints_lda <- NULL
       
       if (input$ldaSelect) {
        
         tidy.complaints <- tidy.complaints %>%
           filter(product %in% input$prodSelect) %>%
           filter(compensated %in% input$compSelect)
         
         complaints_lda <- topic.LDA(k=input$topicSelect, tidy.complaints=tidy.complaints)
           
       }
       else
       {
         if (input$topicSelect == 2) { complaints_lda <- complaints_lda_2 } else 
           if (input$topicSelect == 3) { complaints_lda <- complaints_lda_3 } else 
             if (input$topicSelect == 4) { complaints_lda <- complaints_lda_4 } else 
               if (input$topicSelect == 5) { complaints_lda <- complaints_lda_5 }
       }
       
       top_terms <- topN.Beta(complaints_lda, N)
       
       top_terms %>%
       mutate(term = reorder(term, beta)) %>%
       ggplot(aes(term, beta, fill = factor(topic), color = factor(topic))) +
       geom_col(show.legend = FALSE, alpha=.3) +
       facet_wrap(~ topic, scales = "free", ncol=3) +
       coord_flip()
   })
   
   user_complaint <- eventReactive(input$saveText, {
     
     complainText = input$userTextIn
     #productSelected = input$prodSelect
     #compSelected = input$compSelect
    
     complaintNetSentiment = str.NetSentiment(complainText, replace_reg, lexicon="bing", negation_words)
     
     paste0("The net sentiment of the complaint: ", complaintNetSentiment, "<br><br><i>", complainText,"</i>")
     
   })
   
   output$userTextOut = renderText(user_complaint())   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

