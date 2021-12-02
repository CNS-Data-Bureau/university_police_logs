library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)
library(plotly)
library(DT)

umd_arrest = readRDS("./umd_arrest.rds")
#umd_incident = readRDS("C:/Users/nicho/Documents/GitHub/university_police_logs/police-app/umd_incident.rds")
umd_arrest_list = unique(umd_arrest$type)
umd_arrest_list = c("All Incidents", umd_arrest_list)


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")),
  dashboardPage(
    dashboardHeader(disable = FALSE),
    dashboardSidebar(
      sidebarMenu(
        menuItem("UMD Arrest Reports", tabName = "umd_arrest", icon = icon("dashboard")),
        menuItem("Download the Data", tabName = "download", icon = icon("th")),
        conditionalPanel(
          'input.sidebarid' == "umd_arrest",
          selectInput(inputId = "select_incident",
                      label = "Choose incident",
                      umd_arrest_list)
        )
      ) 
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "umd_arrest",
                tabsetPanel(
                  tabPanel(title = "Plots",
                           br(),    
                     fluidRow(
                       column(12, plotOutput("umd_arrest_year_graph"))
                     ),
                     br(),
                     fluidRow(
                       column(12, plotlyOutput("umd_arrest_time_graph"))
                     ),
                     br(),
                     
                     fluidRow(
                       column(2, checkboxInput("checkbox_race", "Aggregrate Years", value = FALSE)),
                       column(10, plotOutput("umd_arrest_race_graph"))
                     )
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
    
    if(input$select_incident == "All Incidents"){
      result_umd_arrest = umd_arrest  %>% 
        distinct(year, umpd_case_number) %>% 
        group_by(year) %>% 
        summarise(number_arrests = n())
      
    }
   
    else{
      result_umd_arrest = umd_arrest[umd_arrest$type == input$select_incident,]  %>% 
        distinct(year, umpd_case_number) %>% 
        group_by(year) %>% 
        summarise(number_arrests = n())
      
      
    }
   
    
  })

  
  
  # UMD arrest Graph
  output$umd_arrest_year_graph = renderPlot({
    
    ggplot(df_umd_arrest_year(), aes(x = year, y = number_arrests))+
      #geom_bar(stat = "identity")+
      geom_line()+
      geom_point()+
      theme(legend.position = "none")+
      ggtitle(paste0("UMD ", input$select_incident, " Arrests"))
  
})
  
  # UMD Time arrest ---------------------------------
  
  
  df_umd_arrest_time = reactive({
    
    req(input$select_incident)
    if(input$select_incident == "All Incidents"){
      
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
  
  output$umd_arrest_time_graph = renderPlotly({
    p <- ggplot(data=df_umd_arrest_time(), aes(x=time_hour, y=n, group= 1, text = paste("Time of day: ", time_hour,"00 hours","<br>Total arrests reported: ", n))) +
      geom_bar(stat="identity") +
      
      ggtitle("Arrest Cases by Time of Day")+
      xlab("Time of day")+
      ylab("Total number of arrests reported")
    
    
    fig <- ggplotly(p, tooltip = "text")
    
    fig
    
    
    
  })
  
  
  ##############  Race ################
  
  race_grouping <- reactive({
    if(input$checkbox_race == FALSE){
      
    }
    
    
    
  })
  
  df_umd_arrest_race_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == "All Incidents"){
      
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
      ggtitle(paste0("UMD ", input$select_incident, " People Arrested or Cited by Race"))
    }
    
    else{
      ggplot(df_umd_arrest_race_year(), aes(x = race, y = num_people, fill = race))+
        geom_bar(stat = "identity", position = "dodge")+
        ggtitle(paste0("UMD ", input$select_incident, " People Arrested or Cited by Race"))  
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