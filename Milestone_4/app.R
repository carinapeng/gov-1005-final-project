library(shiny)
library(plotly)
library(ggthemes)

data <- readRDS("for_shiny.RDS")

ui <- navbarPage(
    "Blocking Project",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Fake Pre-Housing Day Data"),
                 mainPanel(plotlyOutput("plotName"),
                           plotlyOutput("plotName2")
             ))),
    
    tabPanel("Discussion",
             titlePanel("Conclusions from Fake Data"),
             p("We inputted model data into the survey we made to get preliminary graphs. More detailed work will follow with actual data collection and analysis.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("The Harvard College houses is one of the most thrilling and dramatic days of the school year, and we wanted to wield data as a tool for tackling some of the myths and stereotypes about housing day.
               This project aims to continue the work of the previous blocking group project with more rigorous data analytics and statistical computation, more intuitive graph design, and additional questions (including sexual orientation). It will hopefully build on the previous analysis attempting to find any discrepancies.

The project will attempt to replicate a truly random housing lottery to assess if Harvard’s housing appears to be random as well. The project will check variables such as legacy, race, religion, athletics, sex, freshman dorm, financial aid, international students, and blocking group size.

All Sensitive questions have a “prefer not to answer” option."),
             h3("About Us"),
             p("We are a group of nine students who sought to continue the work of GOV 1005 students from last year. We are
             Jamal Nimer, Carina Peng, Ilyas Mardin, Shojeh Liu, Eliot Min, Lucy He, Angie Shin, Austin Li, and Sam Saba.
               ")))


server <- function(input, output) {
  
  output$plotName <- renderPlotly(
    ggplotly(
     ggplot(data, aes(x = freshman_dorm))  +
        geom_bar() +
      labs(x = "Freshman House",
           y = "Number of Students")))
    
    output$plotName2 <- renderPlotly(
      ggplotly(
        ggplot(data, aes(x = what_house_were_you_placed_in)) + 
          geom_bar() +
          labs(x = "House Placement",
               y = "Number of Students")
    ))
    
  
  
  
}

shinyApp(ui, server)
