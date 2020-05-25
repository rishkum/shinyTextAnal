#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(XML)
library(RCurl)
library(rvest)
library(pdftools)
library(tidyverse)
library(tidytext)
library(shinycssloaders)
library(shinythemes)
source("filegetter.R", local = TRUE)
source("topicmodeling.R", local = TRUE)
source("getnrcPlot.R", local = TRUE)
source("cleanText.R", local = TRUE)
source("twttersentiment.R", local = TRUE)

library(shinydashboard)
library(knitr)
library(rmarkdown)
library(twitteR)
library(magrittr)
library(SentimentAnalysis)
require(gridExtra)

row <- function(...) {
    tags$div(class="row", ...)
}

col <- function(width, ...) {
    tags$div(class=paste0("span", width), ...)
}

# Define UI for application that draws a histogram
ui <-  dashboardPage(  
       dashboardHeader(title = "MVP Text Analysis Dashboard"),
       dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "dashboard", icon = icon("th")),
            menuItem("Analysis", icon = icon("dashboard"), tabName = "analysis"),
            menuItem("Bank Health", icon = icon("dashboard"), tabName = "bankhealth")
        )
    ),
    
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                        h2("Explanation of the analysis"),
                        uiOutput("intro")
                    ),
            
            tabItem(tabName = "analysis",
                   column(width = 6,
                    fluidRow( h2("Analysis of Text Speech")),
                    fluidRow(textInput("text", label = h3("Link of Speech"), placeholder  = "Enter Link..",
                              value = "https://www.bankofengland.co.uk/-/media/boe/files/speech/2018/cyborg-supervision-speech-by-james-proudman.pdf?la=en&hash=6FFE5A1D19EAA76681DB615D9054C53DB8823AB4.pdf")),
                    #fluidRow(textOutput("selected_var")),
                    fluidRow(tableOutput("table")  %>% withSpinner(color="#0dc5c1")),
                    fluidRow(plotOutput("nrcGraph") %>% withSpinner(color="#0dc5c1")),
                    fluidRow(plotOutput("topicsGraph") %>% withSpinner(color="#0dc5c1"))
                   ),
                   column(width = 6,
                       fluidRow( h2("Analysis of Text Speech")),
                       fluidRow(textInput("text2", label = h3("Link of Speech"), placeholder  = "Enter Link..",
                                          value = "https://www.bankofengland.co.uk/-/media/boe/files/speech/2019/managing-machines-the-governance-of-artificial-intelligence-speech-by-james-proudman.pdf?la=en&hash=8052013DC3D6849F91045212445955245003AD7D")),
                      #fluidRow(textOutput("selected_var")),
                       fluidRow(tableOutput("table2")  %>% withSpinner(color="#0dc5c1")),
                       fluidRow(plotOutput("nrcGraph2") %>% withSpinner(color="#0dc5c1")),
                       fluidRow(plotOutput("topicsGraph2") %>% withSpinner(color="#0dc5c1"))
                   )
                   
            ),
            tabItem(tabName = "bankhealth",
                    (h2(" A possible textual way to see banking sector health")),
                    (textInput("companyName", label = h3(" Company tag"), placeholder  = "Enter ticker followed by #", value = "#BARC")),
                    box(offset = 1, plotOutput("tweetGraph") %>% withSpinner(color="#0dc5c1")),
                    box(tabBox(
                        title = paste0("Outlook & Challenges "),
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "250px",width = NULL,
                        tabPanel(title = "Challenges", "Here we will talk about the challenges faced by the bank/n
                                 after reading the yearly statements"),
                        
                        tabPanel(title = "Outlook", "Here we will talk about the outlook expected by the bank/n
                                 after reading the yearly statements")
                            )

                    )
            )
        )
    )
            
    # Put them together into a dashboardPage
    

)      
        
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$selected_var <- renderText({ 
        paste("Summaries for the speech in: ", input$text)
    })
    
    
    output$table <- renderTable({fileGetter(input$text)})
    output$nrcGraph <- renderPlot({getNRCplot(input$text)})
    output$topicsGraph <- renderPlot({showTopics(input$text)})
    
    output$table2 <- renderTable({fileGetter(input$text2)})
    output$nrcGraph2 <- renderPlot({getNRCplot(input$text2)})
    output$topicsGraph2 <- renderPlot({showTopics(input$text2)})
    
    
    output$intro <- renderUI({includeMarkdown("intro.Rmd")})
    output$tweetGraph <- renderPlot({getTweetPlot(input$companyName)})
}

# Run the application 
shinyApp(ui = ui, server = server)
