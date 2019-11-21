# Load the necessary packages
library(shiny)
library(okcupiddata)
library(tm)
library(SentimentAnalysis)
library(wordcloud)
library(tidyverse)
library(memoise)
data(profiles)
profiles_clean <- profiles %>%
  filter(!is.na(essay0))

# Define UI for application, using the NavBar bootstrap format

ui <- navbarPage("Milestone 8",
                 tabPanel("About", includeMarkdown("about.md")),
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
                            titlePanel("Word Cloud (Please wait for corpus to process!"),
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
  
  word = reactive({
    myCorpus = Corpus(VectorSource(profiles_clean$essay0))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("english"), "like", "ive", "san", "francisco"))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    v = sort(rowSums(m),decreasing = TRUE)
    data.frame(word=names(v),freq=v)$word
  })
  
  freq = reactive({
    myCorpus = Corpus(VectorSource(profiles_clean$essay0))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("english"), "like", "ive", "san", "francisco"))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    v = sort(rowSums(m),decreasing = TRUE)
    data.frame(word=names(v),freq=v)$freq
  })
  
  output$plot <- renderPlot({
    wordcloud(words = word(), 
              freq = freq(), scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  output$distPlot <- renderPlot({
    
    bins <- seq(min(profiles$age), max(profiles$age), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(profiles$age, breaks = bins, col = 'darkgray', border = 'white')
  })
  
}

# Run application 
shinyApp(ui, server)

