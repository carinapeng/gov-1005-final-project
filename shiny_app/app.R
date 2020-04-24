
library(shiny)
library(plotly)
library(ggthemes)
library(tidyverse)
library(shinythemes)
library(skimr)
library(patchwork)
library(cowplot)

# must install plotly package

full_data <- readRDS("updated_fake_data.RDS")
full_data_pivoted <- full_data %>% select(-data, -assigned) %>%
  pivot_longer(-replicate, names_to = "community", values_to = "demographics")

base_data <- readRDS("base_data.RDS") %>% ungroup()
base_data_pivoted <- base_data %>% 
  pivot_longer(-data, names_to = "community", values_to = "demographics")

pfoho <- full_data %>% select(replicate, pfoho) %>% unnest(pfoho)
currier <- full_data %>% select(replicate, currier) %>% unnest(currier)
cabot <- full_data %>% select(replicate, cabot) %>% unnest(cabot)
mather <- full_data %>% select(replicate, mather) %>% unnest(mather)
dunster <- full_data %>% select(replicate, dunster) %>% unnest(dunster)
leverett <- full_data %>% select(replicate, leverett) %>% unnest(leverett)
quincy <- full_data %>% select(replicate, quincy) %>% unnest(quincy)
adams <- full_data %>% select(replicate, adams) %>% unnest(adams)
lowell <- full_data %>% select(replicate, lowell) %>% unnest(lowell)
eliot <- full_data %>% select(replicate, eliot) %>% unnest(eliot)
kirkland <- full_data %>% select(replicate, kirkland) %>% unnest(kirkland)
winthrop <- full_data %>% select(replicate, winthrop) %>% unnest(winthrop)

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


#Confidence interval function

confidence_interval <- function(community, lower_percentile = 0.025, median = 0.5, upper_percentile = 0.975){
  
  percentiles <- tibble(
    percentile = c(lower_percentile, median, upper_percentile),
    prop_international = quantile(community %>% ungroup() %>% pull(prop_international), c(lower_percentile, median, upper_percentile)),
    prop_varsity = quantile(community %>% ungroup() %>% pull(prop_varsity), c(lower_percentile, median, upper_percentile)),
    prop_legacy = quantile(community %>% ungroup() %>% pull(prop_legacy), c(lower_percentile, median, upper_percentile)),
    prop_financial_aid = quantile(community %>% ungroup() %>% pull(prop_financial_aid), c(lower_percentile, median, upper_percentile)),
    prop_group_size = quantile(community %>% ungroup() %>% pull(prop_group_size), c(lower_percentile, median, upper_percentile))   
  )
  percentiles
}

#LIMIT TO THE VARIABLES WE'RE CHOOSING TO DO

confidence_interval_pivoted <- function(section, lower_percentile = 0.025, median = 0.5, upper_percentile = 0.975){
  
  selected <- full_data_pivoted %>% filter(community == section)
  
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
                                       plotOutput("graphsTogether")
                                     )))),
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
                   plotOutput("allHouses")
                 )),
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
      input$variable2 == "prop_international" ~ c(.01, .04),
      input$variable2 == "prop_varsity" ~ c(.1275, .135),
      input$variable2 == "prop_legacy" ~ c(.01, .02),
      input$variable2 == "prop_financial_aid" ~ c(.07, .09),
      input$variable2 == "prop_group_size" ~ c(6.1, 6.3)
    )
    
    xlabel <- case_when(
      input$variable2 == "prop_international" ~ "Percentage of International Students",
      input$variable2 == "prop_varsity" ~ "Percentage of Varsity Students",
      input$variable2 == "prop_legacy" ~ "Percentage of Legacy Students",
      input$variable2 == "prop_financial_aid" ~ "Percentage of Students on Financial Aid",
      input$variable2 == "prop_group_size" ~ "Average blocking group size")
    
    filtered1 <- full_data_pivoted %>%
      filter(community == input$neighborhood_1) %>%
      mutate(prop = map(demographics, ~pull(., case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size)))) %>%
      unnest(prop) %>%
      ungroup(replicate)
    
    filtered2 <- full_data_pivoted %>%
      filter(community == input$neighborhood_2) %>%
      mutate(prop = map(demographics, ~pull(., case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size)))) %>%
      unnest(prop) %>%
      ungroup(replicate)
    
    conf.int1 <- confidence_interval_pivoted(input$neighborhood_1) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable == "prop_international" ~ prop_international,
        input$variable == "prop_varsity" ~ prop_varsity,
        input$variable == "prop_legacy" ~ prop_legacy,
        input$variable == "prop_financial_aid" ~ prop_financial_aid,
        input$variable == "prop_group_size" ~ prop_group_size))
    
    conf.int2 <- confidence_interval_pivoted(input$neighborhood_2) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable == "prop_international" ~ prop_international,
        input$variable == "prop_varsity" ~ prop_varsity,
        input$variable == "prop_legacy" ~ prop_legacy,
        input$variable == "prop_financial_aid" ~ prop_financial_aid,
        input$variable == "prop_group_size" ~ prop_group_size))
    
    
    base_value1 <- base_data_pivoted %>% 
      filter(community == input$neighborhood_1) %>%
      unnest(demographics) %>%
      pull(case_when(
        input$variable == "prop_international" ~ prop_international,
        input$variable == "prop_varsity" ~ prop_varsity,
        input$variable == "prop_legacy" ~ prop_legacy,
        input$variable == "prop_financial_aid" ~ prop_financial_aid,
        input$variable == "prop_group_size" ~ prop_group_size))
    
    base_value2 <- base_data_pivoted %>% 
      filter(community == input$neighborhood_2) %>%
      unnest(demographics) %>%
      pull(case_when(
        input$variable == "prop_international" ~ prop_international,
        input$variable == "prop_varsity" ~ prop_varsity,
        input$variable == "prop_legacy" ~ prop_legacy,
        input$variable == "prop_financial_aid" ~ prop_financial_aid,
        input$variable == "prop_group_size" ~ prop_group_size))
    
    graph1 <- 
      ggplot(filtered1, aes(x = prop)) +
      geom_histogram(aes(x = prop, y = ..density..), binwidth = case_when(
        input$variable != "prop_group_size" ~ .0003,
        TRUE ~ .0075)) +
      geom_density(aes(x = prop, y = ..density..)) +
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
        input$variable != "prop_group_size" ~ .0003,
        TRUE ~ .0075)) +
      geom_density(aes(x = prop, y = ..density..)) +
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
  
  output$allHouses <- renderPlot({
    
    #READYING GRAPHS. MIGHT NEED TO DO THIS SEPARATELY
  
    xlabel <- case_when(
      input$variable2 == "prop_international" ~ "Percentage of International Students",
      input$variable2 == "prop_varsity" ~ "Percentage of Varsity Students",
      input$variable2 == "prop_legacy" ~ "Percentage of Legacy Students",
      input$variable2 == "prop_financial_aid" ~ "Percentage of Students on Financial Aid",
      input$variable2 == "prop_group_size" ~ "Average Blocking Group Size"
    )
      
    pfoho_conf.int <- confidence_interval(pfoho) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    pfoho_graph <- ggplot(pfoho, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
        geom_histogram() + 
      labs(x = xlabel, 
           title = "Pfoho") + 
        theme_classic()
    
    currier_conf.int <- confidence_interval(currier) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    currier_graph <- ggplot(currier, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
        geom_histogram() + 
      labs(x = xlabel, 
           title = "Currier") + 
        theme_classic()
    
    cabot_conf.int <- confidence_interval(cabot) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    cabot_graph <- ggplot(cabot, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
        geom_histogram() + 
      labs(x = xlabel, 
           title = "Cabot") + 
        theme_classic()
    
    mather_conf.int <- confidence_interval(mather) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    mather_graph <- ggplot(mather, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
        geom_histogram() + 
      labs(x = xlabel, 
           title = "Mather") + 
        theme_classic()
    
    leverett_conf.int <- confidence_interval(leverett) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    leverett_graph <- ggplot(leverett, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
        geom_histogram() + 
      labs(x = xlabel, 
           title = "Leverett") + 
        theme_classic()
      
    dunster_conf.int <- confidence_interval(dunster) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
   dunster_graph <- ggplot(dunster, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
      geom_histogram() + 
     labs(x = xlabel, 
          title = "Dunster") + 
      theme_classic()
    
    eliot_conf.int <- confidence_interval(eliot) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    eliot_graph <- ggplot(eliot, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
      geom_histogram() + 
      labs(x = xlabel, 
           title = "Eliot") + 
      theme_classic()
    
    kirkland_conf.int <- confidence_interval(kirkland) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    kirkland_graph <- ggplot(kirkland, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
      geom_histogram() + 
      labs(x = xlabel, 
           title = "Kirkland") + 
      theme_classic()
    
    # 
    # winthrop_conf.int <- confidence_interval(winthrop) %>%
    #   filter(percentile %in% c(.025, .975)) %>%
    #   pull(case_when(
    #     input$variable == "prop_international" ~ prop_international,
    #     input$variable == "prop_varsity" ~ prop_varsity,
    #     input$variable == "prop_legacy" ~ prop_legacy,
    #     input$variable == "prop_financial_aid" ~ prop_financial_aid,
    #     input$variable == "prop_group_size" ~ prop_group_size))
    # 
    # winthrop_graph <- ggplot(winthrop, aes(x = case_when(
    #   input$variable == "prop_international" ~ prop_international,
    #   input$variable == "prop_varsity" ~ prop_varsity,
    #   input$variable == "prop_legacy" ~ prop_legacy,
    #   input$variable == "prop_financial_aid" ~ prop_financial_aid,
    #   input$variable == "prop_group_size" ~ prop_group_size))) + 
    #   geom_histogram() + 
    #   theme_classic()
    # 
    adams_conf.int <- confidence_interval(adams) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    adams_graph <- ggplot(adams, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
      geom_histogram() + 
      labs(x = xlabel, 
           title = "Adams") + 
      theme_classic()
    
    lowell_conf.int <- confidence_interval(lowell) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    lowell_graph <- ggplot(lowell, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
      geom_histogram() + 
      labs(x = xlabel, 
           title = "Lowell") + 
      theme_classic()
    
    
    quincy_conf.int <- confidence_interval(quincy) %>%
      filter(percentile %in% c(.025, .975)) %>%
      pull(case_when(
        input$variable2 == "prop_international" ~ prop_international,
        input$variable2 == "prop_varsity" ~ prop_varsity,
        input$variable2 == "prop_legacy" ~ prop_legacy,
        input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
        input$variable2 == "prop_group_size" ~ prop_group_size))
    
    
    quincy_graph <- ggplot(quincy, aes(x = case_when(
      input$variable2 == "prop_international" ~ prop_international,
      input$variable2 == "prop_varsity" ~ prop_varsity,
      input$variable2 == "prop_legacy" ~ prop_legacy,
      input$variable2 == "prop_financial_aid" ~ prop_financial_aid,
      input$variable2 == "prop_group_size" ~ prop_group_size))) + 
      geom_histogram() + 
      labs(x = xlabel,
           title = "Quincy") + 
      theme_classic()
    
    plot_grid(currier_graph, cabot_graph, pfoho_graph,
              eliot_graph, kirkland_graph,
              mather_graph, dunster_graph, leverett_graph,
              adams_graph, quincy_graph, lowell_graph, scale)
  })
  
}

shinyApp(ui, server)

