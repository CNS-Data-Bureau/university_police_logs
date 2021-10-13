library(shiny)

etl_umd_police_arrest_data = read_rds("../data/processed/etl_umd_police_arrest_data.rds")
etl_umd_police_incident_data = read_rds("../data/processed/etl_umd_police_incident_data.rds")

# first_page <- 
#   # First tab content
#   tabItem(tabName = "dashboard",
#           ## second row in first tab
#           fluidRow(
#             column(4, 
#                    varSelectInput("variable", "Grouping Variable", etl_umd_police_arrest_data)
#             )
#           ), 
#           
#           fluidRow(
#             column(4, 
#                    selectInput("race", "raceFilter", etl_umd_police_arrest_data$race)
#             )
#           ),
#           DT:: dataTableOutput("table2")
#   )


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")
  ),
  titlePanel("tktk"),
  dashboardPage(
    dashboardHeader(title = "Police Dashboard"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        
        menuItem("Download the Data", tabName = "download", icon = icon("th"))
        
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "dashboard", 
          fluidRow(
            column(4, 
                   varSelectInput("variable", "Grouping Variable", etl_umd_police_arrest_data)
                  )
                ), 
          
          fluidRow(
            column(4, 
                   selectInput("race", "raceFilter", etl_umd_police_arrest_data$race)
              )
            ),
          DT:: dataTableOutput("table2")
        ), # closes tabItem = Dashboard
        tabItem(tabName = "download",
                fluidRow(
                  selectInput("dataset", "Choose a dataset:", choices = c("University of Maryland", "Other")),
                  downloadButton("downloadData", "Download"),
                  tableOutput("table")
                  ) # closes fluidrow
                ) #closes tabitem
              )#loses tab item
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
  
}

# server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#   })
# }

shinyApp(ui = ui, server = server)