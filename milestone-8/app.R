# Load the necessary packages

library(shiny)
library(okcupiddata)
library(tidyverse)
data(profiles)
profiles_clean <- profiles %>%
  filter(!is.na(essay0))


# Define UI for application, using the NavBar bootstrap format

ui <- navbarPage("Milestone 8",
                 tabPanel("About", includeMarkdown("about.md")),
                 tabPanel("Data", includeMarkdown("about.md")),
                 tabPanel("Overview of Profiles by Age, Sex, and Income (in progress)",
                          fluidPage(
                            
                            # Application title
                            titlePanel("OK Cupid Profiles"),
                            
                            # Sidebar with a slider input for number of bins 
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("bins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 50,
                                            value = 30)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotOutput("distPlot")
                              )
                            )
                          )),
                 tabPanel("Word Cloud of Profile Biographies",
                          fluidPage(
                            # Application title
                            titlePanel("Word Cloud"),
                            sidebarLayout(
                                sliderInput("freq",
                                            "Minimum Frequency:",
                                            min = 1,  max = 50, value = 15),
                                sliderInput("max",
                                            "Maximum Number of Words:",
                                            min = 1,  max = 300,  value = 100)
                              ),
                              
                              # Show Word Cloud
                              mainPanel(
                                plotOutput("plot")
                              ))),
                 tabPanel("Sentiment Analysis In Progress"))

# Define server logic which outputs the graphic created

server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix()
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$distPlot <- renderPlot({
    
    bins <- seq(min(profiles$age), max(profiles$age), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(profiles$age, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  
  output$bar <- renderPlot({
  
    
    our_data <- reactive_data()
    
    barplot(sent3$emotion, sent3$count,
            ylab="Total",
            xlab="Emotion")
  })
}

# Run application 
shinyApp(ui, server)

