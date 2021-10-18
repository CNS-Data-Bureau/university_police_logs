library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

etl_umd_police_arrest_data = read_rds("../data/processed/etl_umd_police_arrest_data.rds")
#etl_umd_police_incident_data = read_rds("../data/processed/etl_umd_police_incident_data.rds")
etl_gwu_police_incident_log = read_rds("../data/processed/etl_gwu_incident_log.rds")

View(etl_gwu_police_incident_log)

# first_page <- 
#   # First tab content
#   tabItem(tabName = "dashboard",
#           ## second row in first tab
#           fluidRow(
#             column(4, 
#                    varSelectInput("variable", "Grouping variable", etl_umd_police_arrest_data)l
#             )
#           ), 
#           
#           fluidRow(
#             column(4, 
#                    selectInput("race", "Filter by race", etl_umd_police_arrest_data$race)
#             )
#           ),
#           DT:: dataTableOutput("table2")
#   )


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")
  ),
  titlePanel("tktk"),
  tags$hr(),
  tags$div(  class = "info",tags$p(
                      "dkfjlkjdslfkj ldksjflkdsjflkjdslkfjlksjfdlkdsjflkjsdlkfjlkdsjflkdsjflkjdsklfjklsdajfkldsjflkdsjflkjdsklfjldslkfjkdsjfjlsdkjflkjsflkjdsklfjdslkfjkldsjfljdsfkjsdflkjaslkfjkldsjflkdsjfklsjdflkjsdlkfjl", collapse = TRUE
  )
  ),# closes div
  dashboardPage(
    dashboardHeader(disable = FALSE),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashbard", tabName = "dashbard", icon = icon("dashboard")),
        menuItem("George Washington University", tabName = "gw", icon = icon("dashboard")),
        menuItem("George Town University", tabName = "gt", icon = icon("dashboard")),
        menuItem("Howard University", tabName = "hd", icon = icon("dashboard")),
        menuItem("University of Maryland", tabName = "umd", icon = icon("dashboard")),
        menuItem("Download the Data", tabName = "download", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashbard", 
          fluidRow(
            column(4, 
                   varSelectInput("variable", "Grouping variable", etl_umd_police_arrest_data)
                  )
                ), 
          fluidRow(
            column(4, 
                   selectInput("race", " umd -- Filter by race", etl_umd_police_arrest_data$race)
              )
            ),
          DT:: dataTableOutput("table2")
        ), # closes tabItem = Dashboard
        tabItem(tabName = "gw", #gw
                fluidRow(
                  column(4, 
                         varSelectInput("variable2", "Grouping variable", etl_gwu_police_incident_log)
                  )
                ), 
                fluidRow(
                  column(4, 
                         selectInput("nature_classification", " GWU -- Filter by type of crime", etl_gwu_police_incident_log$nature_classification)
                  )
                ),
                DT:: dataTableOutput("table3")
        ), # closes tabItem = Dashboard
        tabItem(tabName = "gt", 
                fluidRow(
                  column(4, 
                         "HI"
                  )
                  )
                ),
        tabItem(tabName = "hd", 
                fluidRow(
                  column(4, 
                         "HI"
                  )
                )
        ), 
        tabItem(tabName = "umd", 
                fluidRow(
                  column(4, 
                         "HI"
                  )
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
  )#closes fluiod page -- last one
  # sidebarLayout(
  #   sidebarPanel(
  #     sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
  #   ),
  #   mainPanel(plotOutput("distPlot"))
# ))

server <- function(input, output){
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    
    etl_umd_police_arrest_data <- etl_umd_police_arrest_data[etl_umd_police_arrest_data$race == input$race, ]
    
    #%>% 
    # filter(etl_umd_police_arrest_data$race == input$race)
    
    temp <- etl_umd_police_arrest_data %>% 
      group_by(!!input$variable) %>% 
      count()
    
    temp
  }))
  
  output$table3 <- DT::renderDataTable(DT::datatable({
    
    etl_gwu_police_incident_log <- etl_gwu_police_incident_log[etl_gwu_police_incident_log$nature_classification == input$nature_classification, ]
    
    #%>% 
    # filter(etl_umd_police_arrest_data$race == input$race)
    
    temp <- etl_gwu_police_incident_log %>% 
      group_by(!!input$variable2) %>% 
      count()
    
    temp
  }))
  
}

# server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#   })
# }

shinyApp(ui = ui, server = server)