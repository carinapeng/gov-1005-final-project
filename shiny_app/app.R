
library(shiny)
library(plotly)
library(ggthemes)
library(tidyverse)
library(shinythemes)
library(skimr)
library(patchwork)
library(shinycssloaders)
library(cowplot)

# READING IN DATA FILES

simplified <- readRDS("simplified.RDS")

base_data <- readRDS("base_data.RDS") %>% ungroup()
base_data_pivoted <- base_data %>% 
  pivot_longer(-data, names_to = "community", values_to = "demographics")

#CREATING COMMUNITIES THAT ARE FILTERED BY HOUSE TO MAKE THE PROCESS QUICKER

pfoho <- simplified %>% filter(community == "pfoho") %>% unnest(demographics) %>% ungroup()
currier <- simplified %>% filter(community == "currier") %>% unnest(demographics) %>% ungroup()
cabot <- simplified %>% filter(community == "cabot") %>% unnest(demographics) %>% ungroup()
mather <- simplified %>% filter(community == "mather") %>% unnest(demographics) %>% ungroup()
dunster <- simplified %>% filter(community == "dunster") %>% unnest(demographics) %>% ungroup()
leverett <- simplified %>% filter(community == "leverett") %>% unnest(demographics) %>% ungroup()
quincy <- simplified %>% filter(community == "quincy") %>% unnest(demographics) %>% ungroup()
adams <- simplified %>% filter(community == "adams") %>% unnest(demographics) %>% ungroup()
lowell <- simplified %>% filter(community == "lowell") %>% unnest(demographics) %>% ungroup()
eliot <- simplified %>% filter(community == "eliot") %>% unnest(demographics) %>% ungroup()
kirkland <- simplified %>% filter(community == "kirkland") %>% unnest(demographics) %>% ungroup()
winthrop <- simplified %>% filter(community == "winthrop") %>% unnest(demographics) %>% ungroup()

base_pfoho <- base_data %>% select(pforzheimer) %>% unnest(pforzheimer)
base_currier <- base_data %>% select(currier) %>% unnest(currier)
base_cabot <- base_data %>% select(cabot) %>% unnest(cabot)
base_mather <- base_data %>% select(mather) %>% unnest(mather)
base_dunster <- base_data %>% select(dunster) %>% unnest(dunster)
base_leverett <- base_data %>% select(leverett) %>% unnest(leverett)
base_quincy <- base_data %>% select(quincy) %>% unnest(quincy)
base_adams <- base_data %>% select(adams) %>% unnest(adams)
base_lowell <- base_data %>% select(lowell) %>% unnest(lowell)
base_eliot <- base_data %>% select(eliot) %>% unnest(eliot)
base_kirkland <- base_data %>% select(kirkland) %>% unnest(kirkland)
base_winthrop <- base_data %>% select(winthrop) %>% unnest(winthrop)

ethnicity <- readRDS("ethnicity_results.RDS")
gender <- readRDS("gender_results.RDS")

#Confidence interval function

#LIMIT TO THE VARIABLES WE'RE CHOOSING TO DO

confidence_interval_pivoted <- function(section, lower_percentile = 0.025, median = 0.5, upper_percentile = 0.975){
  
  selected <- simplified %>% filter(community == section)
  
  percentiles <- tibble(
    percentile = c(lower_percentile, median, upper_percentile),
    
    prop_international = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_international),
                                  c(lower_percentile, median, upper_percentile), na.rm = TRUE),
    prop_varsity = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_varsity),
                            c(lower_percentile, median, upper_percentile), na.rm = TRUE),   
    prop_legacy = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_legacy),
                           c(lower_percentile, median, upper_percentile), na.rm = TRUE),
    prop_financial_aid = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_financial_aid),
                                  c(lower_percentile, median, upper_percentile), na.rm = TRUE),
    prop_group_size = quantile(selected %>% ungroup() %>% select(demographics) %>% unnest(demographics) %>% pull(prop_group_size),
                               c(lower_percentile, median, upper_percentile), na.rm = TRUE)
  )
  
  percentiles
  
}

pull_desired <- function(data, variable){
  
  pull(data, case_when(
    variable == "prop_international" ~ prop_international,
    variable == "prop_varsity" ~ prop_varsity,
    variable == "prop_legacy" ~ prop_legacy,
    variable == "prop_financial_aid" ~ prop_financial_aid,
    variable == "prop_group_size" ~ prop_group_size))
  
}


ui <- navbarPage(theme = shinytheme("darkly"),
                 "Blocking Project",
                 tabPanel("Comparisons",
                          navlistPanel(
                            tabPanel("Comparisons Across Neighborhoods",
                                     selectInput("neighborhood_1", 
                                                 label = "Graph 1",
                                                 choices = c("River West" = "river_west",
                                                             "River Central" = "river_central",
                                                             "River East" = "river_east",
                                                             "River" = "river",
                                                             "Quad" = "quad"),
                                                 selected = "river_east",
                                     ),
                                     selectInput("neighborhood_2",
                                                 label = "Graph 2",
                                                 choices = c("River West" = "river_west",
                                                             "River Central" = "river_central",
                                                             "River East" = "river_east",
                                                             "River" = "river",
                                                             "Quad" = "quad")),
                                     selectInput("variable", 
                                                 label = "Variable Displayed",
                                                 choices = c("International" = "prop_international",
                                                             "Varsity" = "prop_varsity",
                                                             "Legacy" = "prop_legacy",
                                                             "Financial Aid" = "prop_financial_aid",
                                                             "Blocking Group Size" = "prop_group_size")),
                                     mainPanel(
                                       plotOutput("graphsTogether", width = "150%") %>%
                                         withSpinner(color="#0dc5c1")
                                     )),
                            tabPanel("Comparisons Across Houses",
                                     selectInput("variable2",
                                                 label = "Variable Displayed",
                                                 choices = c("International" = "prop_international",
                                                             "Varsity" = "prop_varsity",
                                                             "Legacy" = "prop_legacy",
                                                             "Financial Aid" = "prop_financial_aid",
                                                             "Blocking Group Size" = "prop_group_size")
                                     ),
                                     mainPanel(
                                       plotOutput("allHouses", width = "140%") %>%
                                         withSpinner(color="#0dc5c1")
                                     ))
                            )),
                tabPanel("Trends",
                         titlePanel("Self Segregation"),
                         p("We wanted to investigate whether students self-segregated during the blocking process. Our first analysis, conducted below, shows that there is some degree of self-segregation. Of all the blocking groups that contained at least one Asian student, more than twenty percent of them were comprised entirely of Asian students. On the other hand, less than ten percent of the blocking groups that contained white students were entirely white."),
                         plotOutput("segregationGraphs") %>%
                  withSpinner(color="#0dc5c1"),
                  p("We also investigated Gender and found segregation occured in that realm also. 40 percent of blocking groups that contained a member of one gender were comprised entirely of that gender, a trend that was found in both the male and female genders."),
                  plotOutput("genderGraphs") %>%
                    withSpinner(color="#0dc5c1")),
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
  
  
  
  output$graphsTogether <- renderPlot({
    
    xscale <- case_when(
      input$variable == "prop_international" ~ c(.05, .25),
      input$variable == "prop_varsity" ~ c(.10, .3),
      input$variable == "prop_legacy" ~ c(.1, .25),
      input$variable == "prop_financial_aid" ~ c(.5, .8),
      input$variable == "prop_group_size" ~ c(5, 7)
    )
    
    xlabel <- case_when(
      input$variable == "prop_international" ~ "Percentage of International Students",
      input$variable == "prop_varsity" ~ "Percentage of Varsity Students",
      input$variable == "prop_legacy" ~ "Percentage of Legacy Students",
      input$variable == "prop_financial_aid" ~ "Percentage of Students on Financial Aid",
      input$variable == "prop_group_size" ~ "Average blocking group size"
      )
    
    filtered1 <- simplified %>%
      filter(community == input$neighborhood_1) %>%
      mutate(prop = map(demographics, ~pull_desired(., input$variable))) %>%
      unnest(prop) %>%
      ungroup(replicate)
    
    filtered2 <- simplified %>%
      filter(community == input$neighborhood_2) %>%
      mutate(prop = map(demographics, ~pull_desired(., input$variable))) %>%
      unnest(prop) %>%
      ungroup(replicate)
    
    conf.int1 <- confidence_interval_pivoted(input$neighborhood_1) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable)
    
    conf.int2 <- confidence_interval_pivoted(input$neighborhood_2) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable)
    
    base_value1 <- base_data_pivoted %>% 
      filter(community == input$neighborhood_1) %>%
      unnest(demographics) %>%
      pull_desired(., input$variable)
    
    base_value2 <- base_data_pivoted %>% 
      filter(community == input$neighborhood_2) %>%
      unnest(demographics) %>%
      pull_desired(., input$variable)
    
    graph1 <- 
      ggplot(filtered1, aes(x = prop)) +
      geom_histogram(aes(x = prop, y = ..density..), binwidth = case_when(
        input$variable != "prop_group_size" ~ .01,
        TRUE ~ .25)) +
      geom_vline(xintercept  = conf.int1[1]) + 
      geom_vline(xintercept  = conf.int1[2]) +
      geom_vline(xintercept = base_value1,
                 color = "blue") +
      labs(title = paste("Showing Data for", case_when(
        input$neighborhood_1 == "quad" ~ "The Quad",
        input$neighborhood_1 == "river" ~ "The River",
        input$neighborhood_1 == "river_east" ~ "River East",
        input$neighborhood_1 == "river_west" ~ "River West",
        input$neighborhood_1 == "river_central" ~ "River Central")
      ),
      x = xlabel,
      y = "Replicates",
      subtitle = "Bars represent confidence intervals") + 
      theme_classic()
    
    if(input$variable != "prop_group_size"){
      graph1 <- graph1 + scale_x_continuous(limits = xscale, labels = scales::percent)
    }
    else{
      graph1 <- graph1 + scale_x_continuous(limits = xscale)
    }
    
    
    graph2 <- ggplot(filtered2, aes(x = prop)) +
      geom_histogram(aes(x = prop, y = ..density..), binwidth = case_when(
        input$variable != "prop_group_size" ~ .01,
        TRUE ~ .25)) +
      geom_vline(xintercept  = conf.int2[1]) + 
      geom_vline(xintercept  = conf.int2[2]) + 
      geom_vline(xintercept = base_value2,
                 color = "blue") +
      labs(title = paste("Showing Data for", case_when(
        input$neighborhood_2 == "quad" ~ "The Quad",
        input$neighborhood_2 == "river" ~ "The River",
        input$neighborhood_2 == "river_east" ~ "River East",
        input$neighborhood_2 == "river_west" ~ "River West",
        input$neighborhood_2 == "river_central" ~ "River Central")
      ),
      x = xlabel,
      y = "Replicates",
      subtitle = "Bars represent confidence intervals") + 
      theme_classic()
    
    if(input$variable != "prop_group_size"){
      graph2 <- graph2 + scale_x_continuous(limits = xscale, labels = scales::percent)
    }
    else{
      graph2 <- graph2 + scale_x_continuous(limits = xscale)
    }
    
    
    plot_grid(graph1, graph2)
    
  })
  
  
#CREATING A 4X3 GRID OF ALL THE HOUSES
  
  output$allHouses <- renderPlot({
  
    xlabel <- case_when(
      input$variable2 == "prop_international" ~ "Percentage of International Students",
      input$variable2 == "prop_varsity" ~ "Percentage of Varsity Students",
      input$variable2 == "prop_legacy" ~ "Percentage of Legacy Students",
      input$variable2 == "prop_financial_aid" ~ "Percentage of Students on Financial Aid",
      input$variable2 == "prop_group_size" ~ "Average Blocking Group Size"
    )
    
    ylabel <- "Replicates"
    
    xscale <- case_when(
      input$variable2 == "prop_international" ~ c(0, .3),
      input$variable2 == "prop_varsity" ~ c(0, .3),
      input$variable2 == "prop_legacy" ~ c(.05, .3),
      input$variable2 == "prop_financial_aid" ~ c(.4, .8),
      input$variable2 == "prop_group_size" ~ c(4, 8)
    )
    
    binvalue <- case_when(
      input$variable2 != "prop_group_size" ~ .01,
      TRUE ~ .25
      )
    
    pfoho_conf.int <- confidence_interval_pivoted("pfoho") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    pfoho_graph <- ggplot(pfoho, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
        geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel,
           y = ylabel,
           title = "Pfoho") + 
      geom_vline(xintercept = pfoho_conf.int[1]) + 
      geom_vline(xintercept = pfoho_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_pfoho, input$variable2), 
                 color = "blue") +
        theme_classic()
    
    currier_conf.int <- confidence_interval_pivoted("currier") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    currier_graph <- ggplot(currier, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Currier") + 
      geom_vline(xintercept = currier_conf.int[1]) + 
      geom_vline(xintercept = currier_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_currier, input$variable2), 
                 color = "blue") +
        theme_classic()
    
    cabot_conf.int <- confidence_interval_pivoted("cabot") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    cabot_graph <- ggplot(cabot, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Cabot") + 
      geom_vline(xintercept = cabot_conf.int[1]) + 
      geom_vline(xintercept = cabot_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_cabot, input$variable2), 
                 color = "blue") +
        theme_classic()
    
    mather_conf.int <- confidence_interval_pivoted("mather") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    mather_graph <- ggplot(mather, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Mather") + 
      geom_vline(xintercept = mather_conf.int[1]) + 
      geom_vline(xintercept = mather_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_mather, input$variable2), 
                 color = "blue") +
        theme_classic()
    
    leverett_conf.int <- confidence_interval_pivoted("leverett") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    leverett_graph <- ggplot(leverett, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Leverett") + 
      geom_vline(xintercept = leverett_conf.int[1]) + 
      geom_vline(xintercept = leverett_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_leverett, input$variable2), 
                 color = "blue") +
        theme_classic()
      
    dunster_conf.int <- confidence_interval_pivoted("dunster") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
   dunster_graph <- ggplot(dunster, aes(x = case_when(
     input$variable2 == "prop_international" ~ prop_international,
     input$variable2 == "prop_varsity" ~ prop_varsity,
     input$variable2 == "prop_legacy" ~ prop_legacy,
     input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
     input$variable2 == "prop_group_size" ~ prop_group_size))) +
     geom_histogram(binwidth = binvalue) + 
     labs(x = xlabel, 
          y = ylabel,
          title = "Dunster") + 
     geom_vline(xintercept = dunster_conf.int[1]) + 
     geom_vline(xintercept = dunster_conf.int[2]) +
     geom_vline(xintercept = pull_desired(base_dunster, input$variable2), 
                color = "blue") +
      theme_classic()
    
    eliot_conf.int <- confidence_interval_pivoted("eliot") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    eliot_graph <- ggplot(eliot, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Eliot") + 
      geom_vline(xintercept = eliot_conf.int[1]) + 
      geom_vline(xintercept = eliot_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_eliot, input$variable2), 
                 color = "blue") +
      theme_classic()
    
    kirkland_conf.int <- confidence_interval_pivoted("kirkland") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    kirkland_graph <- ggplot(kirkland, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Kirkland") + 
      geom_vline(xintercept = kirkland_conf.int[1]) + 
      geom_vline(xintercept = kirkland_conf.int[2]) + 
      geom_vline(xintercept = pull_desired(base_kirkland, input$variable2), 
                 color = "blue") +
      theme_classic()

    winthrop_conf.int <- confidence_interval_pivoted("winthrop") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)

    winthrop_graph <- ggplot(winthrop, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Winthrop") +
      geom_vline(xintercept = winthrop_conf.int[1]) + 
      geom_vline(xintercept = winthrop_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_winthrop, input$variable2), 
                 color = "blue") +
      theme_classic()

    adams_conf.int <- confidence_interval_pivoted("adams") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    adams_graph <- ggplot(adams, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel, 
           y = ylabel,
           title = "Adams") + 
      geom_vline(xintercept = adams_conf.int[1]) + 
      geom_vline(xintercept = adams_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_adams, input$variable2), 
                 color = "blue") +
      theme_classic()
    
    lowell_conf.int <- confidence_interval_pivoted("lowell") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    lowell_graph <- ggplot(lowell, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      geom_vline(xintercept = lowell_conf.int[1]) + 
      geom_vline(xintercept = lowell_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_lowell, input$variable2), 
                 color = "blue") +
      labs(x = xlabel, 
           y = ylabel,
           title = "Lowell") + 
      theme_classic()
    
    
    quincy_conf.int <- confidence_interval_pivoted("quincy") %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull_desired(., input$variable2)
    
    
    quincy_graph <- ggplot(quincy, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) +
      geom_histogram(binwidth = binvalue) + 
      labs(x = xlabel,
           y = ylabel,
           title = "Quincy") + 
      geom_vline(xintercept = quincy_conf.int[1]) + 
      geom_vline(xintercept = quincy_conf.int[2]) +
      geom_vline(xintercept = pull_desired(base_quincy, input$variable2), 
                 color = "blue") +
      theme_classic()
    
    if(input$variable2 != "prop_group_size"){
      
      cabot_graph <- cabot_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      currier_graph <- currier_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      pfoho_graph <- pfoho_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      adams_graph <- adams_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      lowell_graph <- lowell_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      quincy_graph <- quincy_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      eliot_graph <- eliot_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      winthrop_graph <- winthrop_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      kirkland_graph <- kirkland_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      mather_graph <- mather_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      dunster_graph <- dunster_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      leverett_graph <- leverett_graph + scale_x_continuous(limits = xscale, labels = scales::percent)
      
    }
    
    else{
      cabot_graph <- cabot_graph + scale_x_continuous(limits = xscale)
      currier_graph <- currier_graph + scale_x_continuous(limits = xscale)
      pfoho_graph <- pfoho_graph + scale_x_continuous(limits = xscale)
      adams_graph <- adams_graph + scale_x_continuous(limits = xscale)
      lowell_graph <- lowell_graph + scale_x_continuous(limits = xscale)
      quincy_graph <- quincy_graph + scale_x_continuous(limits = xscale)
      eliot_graph <- eliot_graph + scale_x_continuous(limits = xscale)
      winthrop_graph <- winthrop_graph + scale_x_continuous(limits = xscale)
      kirkland_graph <- kirkland_graph + scale_x_continuous(limits = xscale)
      mather_graph <- mather_graph + scale_x_continuous(limits = xscale)
      dunster_graph <- dunster_graph + scale_x_continuous(limits = xscale)
      leverett_graph <- leverett_graph + scale_x_continuous(limits = xscale)
    }
    
    plot_grid(currier_graph, cabot_graph, pfoho_graph,
              eliot_graph, kirkland_graph, winthrop_graph,
              mather_graph, dunster_graph, leverett_graph,
              adams_graph, quincy_graph, lowell_graph, nrow = 4, ncol = 3)
    
  })
  
  
  output$segregationGraphs <- renderPlot({
    
    asians <- ggplot(ethnicity%>%filter(prop_asian >0) %>% count(prop_asian), aes(x = prop_asian, y = n/46)) +
      geom_col(width = .05) +
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .23), labels = scales::percent) +
      labs(x = "Percentage of Asian students within blocking group", 
           y = "Percentage of Blocking Groups",
           title = "Composition of Blocking Groups containing Asian students",
           subtitle = "46 blocking groups contained at least one Asian student") +
      theme_classic()
    
    
    whites <- ggplot(ethnicity%>%filter(prop_white >0) %>% count(prop_white), aes(x = prop_white, y = n/57)) +
      geom_col(width = .05) +
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .23), labels = scales::percent) +
      labs(x = "Percentage of White students within blocking group", 
           y = "Percentage of Blocking Groups",
           title = "Composition of Blocking Groups containing White students",
           subtitle = "57 blocking groups contained at least one White student") +
      theme_classic()
    
    plot_grid(asians, whites)
    
    
  })
  
  output$genderGraphs <- renderPlot({
    
    females <- ggplot(gender %>% filter(prop_female > 0) %>% count(prop_female), aes(x=prop_female, y = n/55)) + 
      geom_col(width = .05) + 
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .5), labels = scales::percent) +
      labs(x = "Percentage of female students within blocking group", 
           y = "Percentage of Blocking Groups",
           title = "Composition of Blocking Groups containing female students",
           subtitle = "55 blocking groups contained at least one female student") +
      theme_classic() 
    
    males <- ggplot(gender %>% filter(prop_male > 0) %>% count(prop_male), aes(x=prop_male, y = n/49)) + 
      geom_col(width = .05) + 
      scale_x_continuous(limits = c(.1, 1.1), breaks = c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1), labels = scales::percent) +
      scale_y_continuous(limits = c(0, .5), labels = scales::percent) +
      labs(x = "Percentage of male students within blocking group", 
           y = "Percentage of Blocking Groups",
           title = "Composition of Blocking Groups containing male students",
           subtitle = "49 blocking groups contained at least one mmale student") +
      theme_classic() 
    
    plot_grid(females, males)
                    
  })
  
  
  
  }
  
  
  
  

shinyApp(ui, server)

