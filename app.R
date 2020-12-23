#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

### LOAD DATA ###
load("data.RData")

# LOGIN MODAL
authModal <- modalDialog(
    tags$img(src = "https://i.ibb.co/zS8wJdT/tadamlogo.png", height = "30px"),
    hr(),
    textInput("password","Entrer le mot secret."),
    footer = tagList(actionButton("login", "Me connecter"), actionButton("quit", "Quitter")),
    size = "s"
)

#Function to create html ui to display elements as cards
card <- function(picture, link, price, size) {
    HTML(
        paste0(
            "<div class='card'><a href='",
            link,
            "' target='_blank'><img src='",
            picture,
            "' style='width:100%'><div class='container'><p>",
            price,
            "<br><i>Size : ",
            size,
            "</i></p></div></div>")
    )
}

ui <- fluidPage(
    HTML('<meta name="viewport" content="width=1024">'),
    navbarPage(
        tags$img(src = "https://i.ibb.co/zS8wJdT/tadamlogo.png", height = "30px"),
        tabPanel(tags$img(src = "https://i.ibb.co/ZxfD4Nm/HEAD2.png", height = "30px"),
        tags$head(tags$style(".navbar{background-color:transparent;}
                             .navbar-default .navbar-brand{color:transparent;}
                             .tab-panel{background-color:transparent; color:transparent;}
                             .navbar-default .navbar-nav > .active > a, 
                             .navbar-default .navbar-nav > .active > a:focus, 
                             .navbar-default .navbar-nav > .active > a:hover {color:transparent; background-color:transparent;}
                             .card {width:210px; clear:both; box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2); transition:0.3s;}
                             .card:hover {box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2); opacity: .5;}
                             .container {width: 210px; padding: 2px 16px;}
                             a{color:black;}
                             a:link{text-decoration:none;}
                             a:hover{color:black;}
                             #modal-login .modal-dialog{width:97%;}
                             .btn.btn-default.action-button.shiny-bound-input{background-color:black; color:white; border:black;}
                             .btn.btn-default{background-color:black; color:white; border:black;}")),
    fluidRow(
        column(width = 6,
               wellPanel(tags$img(src = "https://static.zara.net/photos///2020/I/0/1/p/7969/253/605/2/w/1908/7969253605_9_1_1.jpg?ts=1606898718307", width = "100%"),
                         h3("Application 1"),
                         p("Havana brown cornish rex bombay but bombay, but havana brown devonshire rex and devonshire rex. Tomcat egyptian mau. Cornish rex sphynx sphynx yet cougar and panther. Panther siberian. Lynx munchkin american shorthair. Norwegian forest."),
                         div(actionButton("button1", "Découvrir", icon("heart"), style="background-color:black; color:white; border:black;"), style="text-align:right;"),
                         style = "height:100%; padding:5px; background-color:transparent;")),
        column(width = 6,
               wellPanel(tags$img(src = "https://static.zara.net/photos///2020/I/0/1/p/0097/239/442/2/w/1908/0097239442_9_1_1.jpg?ts=1606898702609", width = "100%"),
                         h3("Application 2"),
                         p("Havana brown cornish rex bombay but bombay, but havana brown devonshire rex and devonshire rex. Tomcat egyptian mau. Cornish rex sphynx sphynx yet cougar and panther. Panther siberian. Lynx munchkin american shorthair. Norwegian forest."),
                         div(actionButton("button2", "Découvrir", icon("heart"), style="background-color:black; color:white; border:black;"), style="text-align:right;"),
                         style = "height:100%; padding:5px; background-color:transparent;"))
    )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ## LOGIN MODAL ###
    showModal(authModal)
    observeEvent(input$login,{
        if(input$password != "0000")
            stopApp()
        else
            removeModal()
    })
    observeEvent(input$quit,{
        stopApp()
    })
    
    ### OUTPUT SELECTION 1 ###
    output$cards1 <- renderUI({
        selection <- excel %>% filter(...2 == 1)
        # https://stackoverflow.com/questions/57963034/is-it-possible-to-arrange-shiny-app-cards-in-a-masonry-grid
        args <- lapply(seq(selection[[1]]), function(x) card(picture = res_picture[[x]][1],
                                                             link = selection[[1]][x],
                                                             price = res_price[[x]][1],
                                                             size = res_size[[x]][1]))

        args$cellArgs <- list(style = "width: auto; height: auto; margin: 5px;")

        cols <- lapply(seq(2, length(selection[[1]]), 2), function(x) {
            column(width = 1, verticalLayout(args[(x - 1):x], fluid = TRUE))
        })

        do.call(shiny::flowLayout, cols)
    })
    
    ### OUTPUT SELECTION 2 ###
    output$cards2 <- renderUI({
        selection <- excel %>% filter(...2 == 2)
        # https://stackoverflow.com/questions/57963034/is-it-possible-to-arrange-shiny-app-cards-in-a-masonry-grid
        args <- lapply(seq(selection[[1]]), function(x) card(picture = res_picture[[x]][1],
                                                             link = selection[[1]][x],
                                                             price = res_price[[x]][1],
                                                             size = res_size[[x]][1]))
        
        args$cellArgs <- list(style = "width: auto; height: auto; margin: 5px;")

        cols <- lapply(seq(2, length(selection[[1]]), 2), function(x) {
            column(width = 1, verticalLayout(args[(x - 1):x], fluid = TRUE))
        })
        
        do.call(shiny::flowLayout, cols)
    })
    
    
    observeEvent(input$button1, {
        showModal(div(id="modal-login",
            modalDialog(
                tags$img(src = "https://i.ibb.co/zS8wJdT/tadamlogo.png", height = "30px"),
                hr(),
                uiOutput("cards1"),
                footer = modalButton("Retour"),
                easyClose = TRUE,
            ))
        )
    })
    
    observeEvent(input$button2, {
        showModal(div(id="modal-login",
            modalDialog(
                tags$img(src = "https://i.ibb.co/zS8wJdT/tadamlogo.png", height = "30px"),
                hr(),
                uiOutput("cards2"),
                footer = modalButton("Retour"),
                easyClose = TRUE
            ))
        )
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
