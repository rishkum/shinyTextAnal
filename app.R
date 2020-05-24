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
library(shinydashboard)
library(knitr)
library(rmarkdown)

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
            menuItem("Introduction", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Analysis", icon = icon("th"), tabName = "analysis")
        )
    ),
    
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                        h2("Explanation of the analysis"),
                        uiOutput("intro")
                    ),
            
            tabItem(tabName = "analysis",
                    h2("Analysis of Text Speech"),
                    textInput("text", label = h3("Link of Speech"), value = "Enter Link.."),
                    textOutput("selected_var"),
                    tableOutput("table")  %>% withSpinner(color="#0dc5c1"),
                    plotOutput("nrcGraph") %>% withSpinner(color="#0dc5c1"),
                    plotOutput("topicsGraph") %>% withSpinner(color="#0dc5c1")
            )
        )
    ),
            
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
    output$intro <- renderUI({includeMarkdown("readme.Rmd")})
}

# Run the application 
shinyApp(ui = ui, server = server)
