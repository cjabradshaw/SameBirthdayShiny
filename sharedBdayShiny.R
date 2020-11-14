# Shared birthday R Shiny app
# Corey Bradshaw
# Flinders University

## remove everything
rm(list = ls())

# load libraries
library(shiny)
library(ggplot2)
library(ggpubr)

## call functions
#source(file.path("./functions/", "setBackgroundColor.R"), local=T)

ui <- fluidPage(
  
  # title of app
  titlePanel("Probability two people in a group share the same birthday"),
  

  wellPanel(style = "background: ash",
    tags$p(style="font-family:Avenir", tags$i(class="fab fa-r-project", title="R Project"),"Shiny App by", tags$a(href="https://globalecologyflinders.com/people/#CJAB", "Corey Bradshaw "),
           tags$a(href = "mailto:corey.bradshaw@flinders.edu.au","(",tags$i(class="far fa-envelope"),"e-mail"),";",
           tags$a(href = "https://github.com/cjabradshaw", tags$i(class="fab fa-github",title="Github"),"Github)")),
    tags$h4(style="font-family:Avenir", "Preamble"),
    tags$p(style="font-family:Avenir", "What's the probability that two people in a group share the same birthday", tags$i(class="fas fa-birthday-cake"),
    "? It will likely surprise you that the size of the group required for this probability to be high is much smaller than you think."),
    tags$p(style="font-family:Avenir", "This", tags$i(class="fab fa-github"), "Github ",
           tags$a(href = "https://github.com/cjabradshaw/SameBirthdayShiny", "repository"),
           "provides all the 'under-the-bonnet'",tags$i(class="fab fa-r-project"),"code for the app."),
  ),
  
  mainPanel(
    
    fluidRow(
             column(11,
                    wellPanel(
                      sliderInput("groupsize", "number of people in the group:",
                                 min = 2, max = 80, value = 10, step=1, round=T, ticks=T, width="200%",
                                 animate = animationOptions(interval = 500, loop = TRUE)))),
             column(1,
                    tags$img(height = 175, src = "cake.png"))    
              ),
  
    tags$br(),
    
    
    htmlOutput('specProb'),
    tags$head(tags$style("#specProb{font-family:Avenir}"
    )),
    tags$br(),
    plotOutput(height="500px", width="150%", "probPlot")

  ) # end mainPanel
                       
  
) # close fluidPage

server <- function(input, output, session) {
  
  output$probPlot <- renderPlot({
    
    diy <- 365; prN <- rep(0,diy)
    
    for (i in 1:diy) {
      prN[i] <- (diy-i)/diy}
    prYcp <- c(0,1-cumprod(prN))
    ngroup <- 1:input$groupsize
    prgroup <- prYcp[1:(input$groupsize)]
    
    datin <- reactive({
      inpdat <- data.frame(ngroup, prgroup)
      return(inpdat)
    }) # end datin
    
    axis.theme = theme(
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      axis.text.y = element_text(size = 14))
    
    ggplot(data=datin(), aes(x=ngroup, y=prgroup)) +
          coord_cartesian(ylim = c(0, 1)) +
          geom_line() +
          geom_vline(xintercept=input$groupsize, linetype=2, color="red", size=0.5) +
          geom_hline(yintercept=prYcp[input$groupsize], linetype=2, color="red", size=0.5) +
          geom_hline(yintercept=0.5, linetype=3, color="black", size=0.5) +
          geom_vline(xintercept=22.7, linetype=3, color="black", size=0.5) +
      
          labs(x="number of people in group", y="probability 2 people share same birthday") +
          axis.theme
  })
  
  output$specProb <- renderText({
    diy <- 365; prN <- rep(0,diy)
    
    for (i in 1:diy) {
      prN[i] <- (diy-i)/diy}
    prYcp <- c(0,1-cumprod(prN))
    
    paste("probability 2 people in a group of ", input$groupsize, " people have the same birthday = ", round(prYcp[input$groupsize], 3), sep="")
  })
  
  session$onSessionEnded(stopApp)
  
} # end server

shinyApp(ui, server)
