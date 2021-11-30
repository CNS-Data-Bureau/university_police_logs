library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)

umd_arrest = read_rds("umd_arrest.rds")
umd_incident = read_rds("umd_incident.rds")



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
                                     list("Vandalism", "Burglary"))
                  ),
                  column(9, plotOutput("umd_incident_graph"))
                )
              ), # closes tabItem = Dashboard
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
    result_umd_incident = umd_incident[umd_incident$type == input$select_incident,]  %>% 
      filter(year>=2014) %>% 
      group_by(year) %>% 
      summarise(number_incidents = n())
  })
  
  
  
  # UMD Incident Graph
  output$umd_incident_graph = renderPlot({
    
    ggplot(df_umd_incident(), aes(x = year, y = number_incidents))+
      geom_bar(stat = "identity")+
      theme(legend.position = "none")+
      ggtitle(paste0("UMD ", input$select_incident, " Incidents"))
    
    
    
    
  
})
  
}



shinyApp(ui = ui, server = server)