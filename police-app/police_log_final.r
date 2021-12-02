library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)
library(plotly)

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
        menuItem("Dashbard", tabName = "umd", icon = icon("dashboard")),
        menuItem("Download the Data", tabName = "download", icon = icon("th"))
      ) 
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "umd",
                fluidRow(
                  column(3, 
                         selectInput(inputId = "select_incident",
                                     label = "Choose incident",
                                     umd_incident_list)
                  ),
                  column(9, plotOutput("umd_incident_graph"))
                ),
                
                fluidRow(
                  column(12, plotlyOutput("umd_incident_time_graph"))
                  
                  
                )
              ), # closes tabItem = Dashboard
        
        tabItem(tabName = "umd2",
                
                fluidRow(
                  column(4,selectInput(inputId = "select_x",
                                       label = "Choose incident",
                                       list("year", "race"))),
                  column(4,selectInput(inputId = "select_y",
                                       label = "Choose incident",
                                       umd_incident_list)),
                  column(4,selectInput(inputId = "select_grouping",
                                       label = "Choose incident",
                                       umd_incident_list))
                  
                  
                )
                
                
                
                ),
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
  
  # UMD Incident ------------------------
 df_umd_incident <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_incident)
    
    if(input$select_incident == "All Incidents"){
      result_umd_incident = umd_incident  %>% 
        filter(year>=2014) %>% 
        group_by(year) %>% 
        summarise(number_incidents = n())
      
    }
   
    else{
      result_umd_incident = umd_incident[umd_incident$type == input$select_incident,]  %>% 
        filter(year>=2014) %>% 
        group_by(year) %>% 
        summarise(number_incidents = n())
      
      
    }
   
    
  })

  
  
  # UMD Incident Graph
  output$umd_incident_graph = renderPlot({
    
    ggplot(df_umd_incident(), aes(x = year, y = number_incidents))+
      geom_bar(stat = "identity")+
      theme(legend.position = "none")+
      ggtitle(paste0("UMD ", input$select_incident, " Incidents"))
  
})
  
  # UMD Time Incident ---------------------------------
  
  
  umd_incident_time = umd_incident %>% 
    group_by(time_hour) %>% 
    count()
  
  output$umd_incident_time_graph = renderPlotly({
    p <- ggplot(data=umd_incident_time, aes(x=time_hour, y=n, group= 1, text = paste("Time of day: ", time_hour,"00 hours","<br>Total incidents reported: ", n))) +
      geom_bar(stat="identity") +
      ggtitle("Incidents reported by time of day")+
      xlab("Time of day")+
      ylab("Total number of incidents reported")
    
    
    fig <- ggplotly(p, tooltip = "text")
    
    fig
    
    
    
  })
  
  
}



shinyApp(ui = ui, server = server)