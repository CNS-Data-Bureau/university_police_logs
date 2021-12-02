library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)
library(plotly)
library(DT)

umd_arrest = readRDS("C:/Users/nicho/Documents/GitHub/university_police_logs/police-app/umd_arrest.rds")
umd_incident = readRDS("C:/Users/nicho/Documents/GitHub/university_police_logs/police-app/umd_incident.rds")
umd_incident_list = unique(umd_incident$type)
umd_incident_list = c("All Incidents", umd_incident_list)


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")),
  dashboardPage(
    dashboardHeader(disable = FALSE),
    dashboardSidebar(
      sidebarMenu(
        menuItem("UMD Incident Reports", tabName = "umd_incident", icon = icon("dashboard")),
        menuItem("Download the Data", tabName = "download", icon = icon("th")),
        conditionalPanel(
          'input.sidebarid' == "umd_incident",
          selectInput(inputId = "select_incident",
                      label = "Choose incident",
                      umd_incident_list)
        )
      ) 
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "umd_incident",
                tabsetPanel(
                  tabPanel(title = "Plots",
                           br(),    
                     fluidRow(
                       column(12, plotOutput("umd_incident_year_graph"))
                     ),
                     br(),
                     fluidRow(
                       column(12, plotlyOutput("umd_incident_time_graph"))
                     )
                     ),
                  
                  
                  tabPanel(title = "Data Table",
                           br(),
                           fluidRow(
                             column(12, DTOutput("umd_incident_table"))
                             
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
  ########################################## INCIDENT ##########################################
  # UMD Incident ------------------------
 df_umd_incident_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == "All Incidents"){
      result_umd_incident = umd_incident  %>% 
        group_by(year) %>% 
        summarise(number_incidents = n())
      
    }
   
    else{
      result_umd_incident = umd_incident[umd_incident$type == input$select_incident,]  %>% 
        group_by(year) %>% 
        summarise(number_incidents = n())
      
      
    }
   
    
  })

  
  
  # UMD Incident Graph
  output$umd_incident_year_graph = renderPlot({
    
    ggplot(df_umd_incident_year(), aes(x = year, y = number_incidents))+
      #geom_bar(stat = "identity")+
      geom_line()+
      geom_point()+
      theme(legend.position = "none")+
      ggtitle(paste0("UMD ", input$select_incident, " Incidents"))
  
})
  
  # UMD Time Incident ---------------------------------
  
  
  df_umd_incident_time = reactive({
    
    req(input$select_incident)
    if(input$select_incident == "All Incidents"){
      
      result_umd_incident_time = umd_incident %>% 
        group_by(time_hour) %>% 
        count()
      
      
    }
    
    else{
      
      result_umd_incident_time = umd_incident[umd_incident$type == input$select_incident,]  %>% 
        group_by(time_hour) %>% 
        count()
      
      
      
    }
    
    
    
    
    
    
    
    })
  
  output$umd_incident_time_graph = renderPlotly({
    p <- ggplot(data=df_umd_incident_time(), aes(x=time_hour, y=n, group= 1, text = paste("Time of day: ", time_hour,"00 hours","<br>Total incidents reported: ", n))) +
      geom_bar(stat="identity") +
      
      ggtitle("Incidents reported by time of day")+
      xlab("Time of day")+
      ylab("Total number of incidents reported")
    
    
    fig <- ggplotly(p, tooltip = "text")
    
    fig
    
    
    
  })
  
  
  df_incident_table = reactive({
    
    req(input$select_incident)
    if(input$select_incident == "All Incidents"){
      
      result_umd_incident_table = umd_incident
    }
    
    else{
      
      result_umd_incident_table = umd_incident[umd_incident$type == input$select_incident,]
      
      
    }
    
    
  })
  
  output$umd_incident_table <- renderDT(df_incident_table(), 
                                        filter = "top",
                                        options = list(
                                          pageLength = 25
                                        )
    
  )
  
  
}



shinyApp(ui = ui, server = server)