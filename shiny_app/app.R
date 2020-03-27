library(shiny)
library(plotly)
library(ggthemes)
library(tidyverse)

# must install plotly package

<<<<<<< HEAD
data <- readRDS("updated_fake_data.RDS")
=======
data <- readRDS("for_shiny.RDS")
data2 <- data %>%
  count(freshman_dorm)
data3 <- data %>%
  count(what_house_were_you_placed_in)


>>>>>>> eliotdmin-master

#BREAKDOWN OF FROSH

#Tibble that assigns a "yard" variable indicating the yard of the freshman house
yards <- data %>%
  mutate(yard = case_when(
    freshman_dorm %in% (c("Canaday", "Thayer")) ~ "Oak",
    freshman_dorm %in% (c("Grays", "Matthews", "Grays")) ~ "Elm",
    freshman_dorm %in% (c("Apley", "Hollis", "Holworthy", "Lionel", "Massachusetts",
                          "Mower", "Stoughton", "Straus")) ~ "Ivy",
    TRUE ~ "Crimson"))

legacy_yards <- yards %>%
  group_by(yard) %>%
  summarize(legacies = sum(legacy))

how_many_each_size <- data %>%
  count(group_size) %>%
  mutate(numberOfGroups = n/group_size)

suitemates_per_house <- data %>%
  group_by(freshman_dorm) %>%
  summarize(meanSuitemates = mean(suitemates))

#BREAKDOWN OF BLOCKING GROUPS

legacies_per_block <- data %>%
  group_by(group_name) %>%
  summarize(legacies = sum(legacy))

athletes_per_block <- data %>%
  group_by(group_name) %>%
  count(varsity) %>%
  filter(varsity == TRUE)

 
ui <- navbarPage(
<<<<<<< HEAD
  "Blocking Project",
  tabPanel("Breakdown of the Freshman Class",
           navlistPanel(
             tabPanel("Freshmen by Dorm",
                      plotlyOutput("froshByDorm")),
             tabPanel("Freshmen by Yard",
                          plotlyOutput("froshByYard")),
             tabPanel("Legacy students per Yard",
                      plotlyOutput("legaciesByYard")),
             tabPanel("Religious Composition",
                      plotlyOutput("religiousComposition")),
             tabPanel("Sexual Orientation Distribution",
                      plotlyOutput("sexualOrientation")),
             tabPanel("Ethnicity Distribution",
                      plotlyOutput("ethnicities")),
             tabPanel("Average Suitemates per House",
                      plotlyOutput("suitematesPerHouse")))),
  tabPanel("Breakdown of Blocking Groups",
           navlistPanel(
             tabPanel("Distribution of Blocking Group Sizes",
                      plotlyOutput("blockSizes")),
             tabPanel("Legacy students per Blocking Group",
                      plotlyOutput("legaciesByBlockingGroup")),
             tabPanel("Varsity Students per Blocking Group",
                      plotlyOutput("blockingVarsity")))),
=======
    "Blocking Project",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Fake Pre-Housing Day Data"),
                 mainPanel(plotlyOutput("plotName"),
                           plotlyOutput("plotName2"),
                           plotOutput("plotName3"),
                           plotOutput("plotName4")
             ))),
    
>>>>>>> eliotdmin-master
    tabPanel("Discussion",
             titlePanel("Conclusions from Fake Data"),
             p("A huge wrench was thrown into our data collection with the coronavirus evacuation. We are currently working on acquiring data in spite of this disruption.
               We inputted model data into the survey we made to get preliminary graphs. More detailed work will follow with actual data collection and analysis.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("*Update* Our project has been compromised due to the Coronavirus outbreak and the resulting postponement of housing day. We are working to develop our algorithms for analyzing the data and will work with fake data for now.
             The Harvard College houses is one of the most thrilling and dramatic days of the school year, and we wanted to wield data as a tool for tackling some of the myths and stereotypes about housing day.
               This project aims to continue the work of the previous blocking group project with more rigorous data analytics and statistical computation, more intuitive graph design, and additional questions (including sexual orientation). It will hopefully build on the previous analysis attempting to find any discrepancies.
              

The project will attempt to replicate a truly random housing lottery to assess if Harvard’s housing appears to be random as well. The project will check variables such as legacy, race, religion, athletics, sex, freshman dorm, financial aid, international students, and blocking group size.

All Sensitive questions have a “prefer not to answer” option."),
             h3("About Us"),
             p("We are a group of nine students who sought to continue the work of GOV 1005 students from last year. We are
             Jamal Nimer, Carina Peng, Ilyas Mardin, Shojeh Liu, Eliot Min, Lucy He, Angie Shin, Austin Li, and Sam Saba.
               ")))


server <- function(input, output) {
  
  #Graphs for breakdown of freshman class
  
  output$froshByDorm <- renderPlotly(
    ggplotly(
      ggplot(yards, aes(x = freshman_dorm))  +
        geom_bar(position = "dodge") +
        labs(x = "Freshman Dorm",
             y = "Number of Students") + 
        coord_flip() + 
        theme_classic()
      
    ))
  
  output$froshByYard <- renderPlotly(
    ggplotly(
     ggplot(yards, aes(x = yard))  +
        geom_bar(position = "dodge") +
      labs(x = "Freshman Yard",
           y = "Number of Students") + 
    theme_classic()
    
<<<<<<< HEAD
    ))
  
  
  output$legaciesByYard <- renderPlotly(
    ggplotly(
      ggplot(legacy_yards, aes(x = yard, y = legacies)) +
        geom_col() + 
        labs(x = "Legacy Students per Yard",
             y = "Number of Students") + 
        theme_classic()
    ))
  
  output$religiousComposition <-renderPlotly(
    ggplotly(
      ggplot(data, aes(x = religion)) + 
        geom_histogram(binwidth = .5, position = "dodge") + 
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), 
                           labels = c("Agnostic", "Christian", "Atheist", "Jewish",
                                      "Hindu", "Muslim", "Prefer not to say",
                                      "Other")) + 
        labs(x = "Religion",
             y = "Number of Students") + 
        coord_flip() + 
        theme_classic()
    )
  )
  
  
  output$sexualOrientation <-renderPlotly(
    ggplotly(
      ggplot(data, aes(x = sexual_orientation)) + 
        geom_histogram(binwidth = .5, position = "dodge") + 
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), 
                           labels = c("Heterosexual", "Homosexual", "Bisexual",
                                      "Asexual", "Prefer not to Say", "Other")) + 
        labs(x = "Sexual Orientation",
             y = "Number of Students") + 
        theme_classic()
    )
  )
  
  output$ethnicities <-renderPlotly(
    ggplotly(
      ggplot(data, aes(x = ethnicity)) + 
        geom_histogram(binwidth = .5, position = "dodge") + 
        scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), 
                           labels = c("White", "Asian", "Black", "Hispanic/LatinX", 
                                      "Middle Eastern/North African", 
                                      "Indigenous/Native American", "Prefer not to say",
                                      "Other"
                                      )) + 
        labs(x = "Ethnicity",
             y = "Number of Students") + 
        coord_flip() + 
        theme_classic()
    )
  )
  
  
  output$suitematesPerHouse <- renderPlotly(
    ggplotly(
      ggplot(suitemates_per_house, aes(x = freshman_dorm, y =  meanSuitemates))  +
        geom_col() +
        labs(x = "Average number of Suitemates",
             y = "Number of Students") + 
        coord_flip() + 
        theme_classic()
      
    ))
  
  #GRAPHS FOR BREAKDOWN OF BLOCKING GROUPS
=======
    output$plotName3 <- renderPlot(
      pie(data2$n, labels = data2$freshman_dorm, main = "Distributions of freshman dorms")
    )
    
    output$plotName4 <- renderPlot(
      pie(data3$n, labels = data3$what_house_were_you_placed_in, main = "Distributions of house placements")
    )
>>>>>>> eliotdmin-master
  

  output$legaciesByBlockingGroup <- renderPlotly(
    ggplotly(
      ggplot(legacies_per_block, aes(x = legacies)) +
               geom_bar() + 
        labs(x = "Legacy Students per Blocking Group",
             y = "Number of Blocking Groups") + 
        theme_classic()
      ))
  
  
  output$blockSizes <- renderPlotly(
    ggplotly(
      ggplot(how_many_each_size, aes(x = as.factor(group_size), y =  numberOfGroups)) +
        geom_col() + 
        labs(x = "Blocking Group Size",
             y = "Number of Groups") + 
        theme_classic()
    )
  )
  
  
  output$blockingVarsity <- renderPlotly(
    ggplotly(
      ggplot(athletes_per_block, aes(x = n)) + 
        geom_bar() +
        labs(x = "Number of Varsity Athletes in Blocking Group",
             y = "Number of Blocking Groups") +
        theme_classic()
    )
  )
}

shinyApp(ui, server)
