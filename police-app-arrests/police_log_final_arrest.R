library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)
library(plotly)
library(DT)
library(gcookbook)
library(hrbrthemes)
library(RColorBrewer)

umd_arrest = readRDS("./umd_arrest.rds")
umd_arrest_combined = readRDS("./arrest_combined.rds")
#umd_incident = readRDS("C:/Users/nicho/Documents/GitHub/university_police_logs/police-app/umd_incident.rds")
umd_arrest_list = unique(umd_arrest$type)
all_incident = "All Incident Types"
umd_arrest_list = c(all_incident, umd_arrest_list)
ls_years = unique(umd_arrest$year)
min_year = min(ls_years)
max_year = max(ls_years)

color_scheme = "Dark2"
single_color = "#1b9e77"


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")),
  dashboardPage(
    dashboardHeader(disable = FALSE),
    dashboardSidebar(disable = TRUE,
      sidebarMenu(
        menuItem("UMD Arrest Reports", tabName = "umd_arrest", icon = icon("dashboard")),
        menuItem("Download the Data", tabName = "download", icon = icon("th"))
        # conditionalPanel(
        #   'input.sidebarid' == "umd_arrest",
        #   selectInput(inputId = "select_incident",
        #               label = "Choose incident",
        #               umd_arrest_list)
        # )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "umd_arrest",
                tabsetPanel(
                  tabPanel(title = "Plots",
                           br(),
                           fluidRow(
                             column(6, selectInput(inputId = "select_incident",
                                                   label = "Choose incident",
                                                   umd_arrest_list) ),
                             column(6,checkboxInput("checkbox_race", "Aggregrate Years", value = FALSE) )
                             
                           )
                           ,
                     fluidRow(
                       column(6, plotOutput("umd_arrest_year_graph")),
                       column(6, plotOutput("umd_arrest_race_graph")), 
                       
                     ),
                     br(),
                     fluidRow(
                       column(12, plotlyOutput("umd_arrest_time_graph"))
                     ),
                     br()
                     ),
                  
                  
                  tabPanel(title = "Data Table",
                           br(),
                           selectInput("vars", "Variables", names(umd_arrest), multiple = TRUE),
                           textOutput("text1"),
                           
                           fluidRow(
                             column(12, DTOutput("umd_arrest_table2"))
                             
                           )
                  )
                  
                  )
                ) 
        
        ,
        tabItem(tabName = "download",
                fluidRow(
                  selectInput("dataset", "Choose a dataset:", choices = c("University of Maryland", "Other")),
                  downloadButton("downloadData", "Download"),
                  tableOutput("table")
                  ) # closes fluidrow
                ) #closes tabitem
              )#loses tab items
            )#closs dashboard body
    )# closes dasbhboard page
  )
  

server <- function(input, output){
  ########################################## arrest ##########################################
  # UMD arrest ------------------------

  
  
  
  
 df_umd_arrest_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == all_incident){
      umd_arrest_combined
      
    }
   
    else{
      result_umd_arrest = umd_arrest_combined[umd_arrest_combined$type == input$select_incident,]
      
      
    }
   
    
  })

  
  
  # UMD arrest Graph
  output$umd_arrest_year_graph = renderPlot({
    
    if(input$select_incident == all_incident){
      ggplot(df_umd_arrest_year(), aes(x = year, y = number, fill = final_type))+
        geom_bar(stat = "identity")+
        labs(x = "Years", y = "Number of UMPD Cases",
             title = paste0("UMD Arrests/Citations:  ", input$select_incident),
             subtitle = "Broken Down By Top 3 Incidents Per Year",
              fill ="Top 3 Incident Types")+
        scale_fill_brewer(palette = color_scheme) +
        theme_ipsum(grid="Y")+
        scale_x_continuous( breaks = ls_years)
      
    }
    
    else{
      ggplot(df_umd_arrest_year(), aes(x = year, y = number, fill = single_color))+
        geom_bar(stat = "identity")+
        labs(x = "Years", y = "Number of UMPD Cases",
             title = paste0("UMD Arrests/Citations:  ", input$select_incident))+
        scale_fill_brewer(palette = color_scheme) +
        theme_ipsum(grid="Y")+
        scale_x_continuous( breaks = ls_years)+
        theme(legend.position = "none")
      
      
    }
    
    
  
})
  
  # UMD Time arrest ---------------------------------
  
  
  df_umd_arrest_time = reactive({
    
    req(input$select_incident)
    if(input$select_incident == all_incident){
      
      result_umd_arrest_time = umd_arrest %>% 
        distinct(year, umpd_case_number, .keep_all = TRUE) %>% 
        group_by(time_hour) %>% 
        count()
      
      
    }
    
    else{
      
      result_umd_arrest_time = umd_arrest[umd_arrest$type == input$select_incident,]  %>% 
        group_by(time_hour) %>% 
        count()
      
      
      
    }
    })
  
  output$umd_arrest_time_graph =renderPlotly({
    
    ggplot(df_umd_arrest_time(), aes(x=time_hour, y=`n`, fill = single_color)) +
      geom_bar(stat="identity") +
      labs(x = "Time", y = "Number of Arrest/Citations",
           title = paste0("UMD Arrests/Citations:  ", input$select_incident),
           subtitle = paste0("By Hour, Aggregated from ", toString(min_year), "-", toString(max_year)))+
      scale_fill_brewer(palette = color_scheme) +
      theme_ipsum(grid="Y")+
      scale_x_discrete( labels = c("12 a.m.", "1 a.m.", "2 a.m.", "3 a.m.", "4 a.m.", "5 a.m.", "6 a.m.", "7 a.m.","8 a.m.", "9 a.m.", "10 a.m.", "11 a.m.",
                                   "12 p.m.", "1 p.m.", "2 p.m.", "3 p.m.", "4 p.m.", "5 p.m.", "6 p.m.", "7 p.m.","8 p.m.", "9 p.m.", "10 p.m.", "11 p.m."))+
      theme(legend.position = "none")
    
    
    
    
  })
  
  
  ##############  Race ################
  
  race_grouping <- reactive({
    if(input$checkbox_race == FALSE){
      
    }
    
    
    
  })
  
  df_umd_arrest_race_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == "All Incident Types"){
      
      if(input$checkbox_race == FALSE){
        result_umd_arrest_race_year = umd_arrest  %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(year, race) %>% 
          summarise(num_people = n())
        
      }
      
      else{
        
        result_umd_arrest_race_year = umd_arrest  %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(race) %>% 
          summarise(num_people = n())
        
        
      }
      
      
        
      
    }
    
    else{
      if(input$checkbox_race == FALSE){
        
        result_umd_arrest_race_year = umd_arrest[umd_arrest$type == input$select_incident,]  %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(year, race) %>% 
          summarise(num_people = n())
        
        
      }
      
      else{
        result_umd_arrest_race_year = umd_arrest[umd_arrest$type == input$select_incident,]  %>% 
          distinct(year, umpd_case_number, arrest_number, race, type) %>% 
          group_by(race) %>% 
          summarise(num_people = n())
        
        
      }
      
      
      
      
    }
    
    
  })
  
  
  
  output$umd_arrest_race_graph = renderPlot({
    
    if(input$checkbox_race == FALSE){
    
    ggplot(df_umd_arrest_race_year(), aes(x = year, y = num_people, fill = race))+
      geom_bar(stat = "identity", position = "dodge")+
      labs(x = "Years", y = "Number of People",
           title = paste0("UMD Arrests/Citations:  ", input$select_incident),
           subtitle = "Broken Down By Race",
           fill ="Race")+
      scale_fill_brewer(palette = color_scheme) +
      theme_ipsum(grid="Y")+
      scale_x_continuous( breaks = ls_years)
    }
    
    else{
      ggplot(df_umd_arrest_race_year(), aes(x = race, y = num_people, fill = race))+
        geom_bar(stat = "identity", position = "dodge")+
        labs(x = "Years", y = "Number of People",
             title = paste0("UMD Arrests/Citations:  ", input$select_incident),
             subtitle = "Broken Down By Race",
             fill ="Race")+
        scale_fill_brewer(palette = color_scheme) +
        theme_ipsum(grid="Y")
        
      
      
      
    }
    
    
  })
  
  
  ###############Table######################
  
  
  df_arrest_table = reactive({
    
    req(input$select_incident)
    if(input$select_incident == "All Incidents"){
      
      result_umd_arrest_table = umd_arrest
    }
    
    else{
      
      result_umd_arrest_table = umd_arrest[umd_arrest$type == input$select_incident,]
      
      
    }
    
    
  })
  
  
  
  output$umd_arrest_table <- renderDT(df_arrest_table(), 
                                        filter = "top",
                                        options = list(
                                          pageLength = 25
                                        )
    
  )
  
  
  x <- reactive(length(list(input$vars)))
  
  
  output$text1 <- renderText({
    paste("Captured text:", x())
  }) 
  
  output$umd_arrest_table2 <- renderDT(
    
    
    
     umd_arrest %>% 
      distinct(arrest_number, .keep_all = TRUE) %>% 
      group_by(across(all_of(input$vars))) %>% 
      summarise(count = n(), .groups = "drop"),
    
    
                                      filter = "top",
                                      options = list(
                                        pageLength = 25
                                      )
                                      
  )
  
  
}



shinyApp(ui = ui, server = server)