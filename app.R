#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rvest)
library(stringr)
library(xml2)
library(httr)
library(googlesheets4)
library(googledrive)

gs4_auth(cache = ".secrets", email = TRUE)
ss <- "https://docs.google.com/spreadsheets/d/1PWglsYDplIzOKxXadytzz1COJmfJvqGDcOEsA17dIqE/edit?usp=sharing"
selection <- read_sheet(ss, col_names = FALSE)

ui <- navbarPage("TADAM",
                 tabPanel("Test",
                          fluidRow(
                              lapply(seq_along(selection$...1), function(i){
                                  column(
                                      3,
                                      uiOutput(paste0("image", i)),
                                      textOutput(paste0("price", i)),
                                      textOutput(paste0("size", i))
                                  )
                              })
                              )
                          )
                 )

server <- function(input, output) {
    
# Generate Output loop using lapply : https://shiny.rstudio.com/reference/shiny/0.11/imageOutput.html

    lapply(seq_along(selection$...1), function(i){
        output[[paste0("image", i)]] <- renderUI({
            res1 <- selection$...1[i] %>% read_html() %>% html_nodes("figure.item-description:nth-child(1) > a:nth-child(1)") %>% str_extract_all("http[^\"><]+") %>% unlist()
            tags$a(href = selection$...1[i], target="_blank", tags$img(src = res1[1], height = "532.9px", width = "352.8px"))
        })
    })

    lapply(seq_along(selection$...1), function(i){
        output[[paste0("price", i)]] <- renderText({
            res2 <- selection$...1[i] %>% read_html() %>% html_nodes(".c-text--heading") %>% html_text()
        })
    })
    
    lapply(seq_along(selection$...1), function(i){
        output[[paste0("size", i)]] <- renderText({
            res3 <- selection$...1[i] %>% read_html() %>% html_nodes(".details-list--details > div:nth-child(2) > div:nth-child(2)") %>% html_text()
            paste0("Taille ", res3)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
