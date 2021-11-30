library(shiny)

library(shinydashboard)
library(tidyverse)
library(DT)
library(rsconnect)
library(janitor)

etl_umd_police_arrest_data = read_rds("etl_umd_police_arrest_data.rds")
#etl_umd_police_incident_data = read_rds("../data/processed/etl_umd_police_incident_data.rds")
etl_gwu_police_incident_log = read_rds("etl_gwu_incident_log.rds")
# howard
etl_howard_police_arrest_data = read_rds("etl_howard_incident_log.rds")
etl_gt_police_arrest_data = read_rds("etl_gt_incident_log.rds")
enrollment = read_rds("enrollment.rds")

enrollment = enrollment %>% 
  clean_names()

# "/home/nickmcmillan/Code/university_police_logs/viz"

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
    tags$link(rel = "stylesheet", type = "text/css", href = "dark_mode.css")),
  dashboardPage(
    dashboardHeader(disable = FALSE),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashbard", tabName = "dashbard", icon = icon("dashboard")),
        menuItem("George Washington University", tabName = "gwu", icon = icon("dashboard")),
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
          fluidRow(
            column(9, plotOutput("enrollment_chart"))
          ),
          fluidRow(
            sidebarPanel(
              table("obs",
                          "Number of observations:",
                          min = 0,
                          max = 1000,
                          value = 500)
            ),
          ),
          DT:: dataTableOutput("table2")
        ), # closes tabItem = Dashboard
        tabItem(tabName = "gwu", 
        fluidRow(
          column(3, 
                 selectInput(inputId = "select_crime_gwu",
                             label = "Choose description",
                             list("Unlawful Entry", "Liquor Law Violation"))
          ),
          
          column(9, plotOutput("gwu_plot"))
        )), # closes tabItem = Dashboard
        tabItem(tabName = "gt", 
                fluidRow(
                  column(3, 
                         selectInput(inputId = "select_crime_gt",
                                     label = "Choose description",
                                     list("theft", "drug violation"))
                  ),
                  column(9, plotOutput("gt_graph"))
                )
        ), 
        tabItem(tabName = "hd", 
                fluidRow(
                  column(3, 
                         selectInput(inputId = "select_crime_hu",
                                     label = "Choose description",
                                     list("Student Misconduct", "Stalking"))
                  ),
                  
                  column(9, plotOutput("howard_graph"))
                )
        ), 
        tabItem(tabName = "umd", 
                fluidRow(
                  column(3, 
                         selectInput(inputId = "select_crime",
                                     label = "Choose description",
                                     list("CDS:Possession Marijuana L/T 10 grams", "Underage Possession: Alcoholic Beverage"))
                ), 
                
                column(9, 
                       plotOutput("umd_plot"))
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
  
  # Output 1
  output$table2 <- DT::renderDataTable(DT::datatable({
    
    etl_umd_police_arrest_data <- etl_umd_police_arrest_data[etl_umd_police_arrest_data$race == input$race, ]
    
    #%>% 
    # filter(etl_umd_police_arrest_data$race == input$race)
    
    temp <- etl_umd_police_arrest_data %>% 
      group_by(!!input$variable) %>% 
      count()
    
    temp
  }))
  
  
  
  # Output 2
  # output$table3 <- DT::renderDataTable(DT::datatable({
  #   
  #   etl_gwu_police_incident_log <- etl_gwu_police_incident_log[etl_gwu_police_incident_log$nature_classification == input$nature_classification, ]
  #   
  #   #%>% 
  #   # filter(etl_umd_police_arrest_data$race == input$race)
  #   
  #   temp <- etl_gwu_police_incident_log %>% 
  #     group_by(!!input$variable2) %>% 
  #     count()
  #   
  #   temp
  # }))
  
  # output$table4 <- DT::renderDataTable(DT::datatable({
  #   
  #   etl_howard_police_arrest_data <- etl_howard_police_arrest_data[etl_howard_police_arrest_data$natures_of_crime == input$natures_of_crime, ]
  #   
  #   #%>% 
  #   # filter(etl_umd_police_arrest_data$race == input$race)
  #   
  #   temp <- etl_howard_police_arrest_data %>% 
  #     group_by(!!input$variable3) %>% 
  #     count()
  #   
  #   temp
  # }))
  # etl_howard_police_arrest_data <- etl_howard_police_arrest_data[etl_howard_police_arrest_data$natures_of_crime == input$natures_of_crime]
  
  ## GWU plots output
  etl_gwu_test <- reactive({
    #print(input$select_crime_gwu) 
    req(input$select_crime_gwu)
    gwu_graph = etl_gwu_police_incident_log[etl_gwu_police_incident_log$nature_classification == input$select_crime_gwu,]  %>% 
      group_by(date_reported, nature_classification) %>% 
      summarise(number_crimes = n())
  })
  
  # creative a plot base don the reactive dataframe
  output$gwu_plot = renderPlot({
    gwu_graph<- ggplot(etl_gwu_test(), aes(x = date_reported, y = nature_classification))+
      geom_bar(stat = "identity")+
      theme(legend.position = "none")+
      ggtitle(paste0("GWU over the years: ", input$select_crime_gwu))
    
    gwu_graph
  })

  # enrollment chart ---->
  
  output$enrollment_chart <- renderPlot({
    enrollment_for_graph = enrollment %>% 
      clean_names() %>% 
      select(-unit_id) %>% 
      pivot_longer(!institution_name, names_to = "variables", values_to =  "counts") %>% 
    filter(variables  == "grand_total_men_effy2020_all_students_total"  | variables == "grand_total_women_effy2020_all_students_total") %>% 
      ggplot(aes(x = institution_name, y = counts, fill = variables))+
      geom_bar(stat = "identity")+
      labs(y="Enrollment count", x = "")+
      # guides(fill = guide_legend(title = "legen"))+
      labs(fill = "Condition")+
      
      scale_fill_discrete(labels = c("Men", "Women"))+
      labs(fill = "Legend")+
      ggtitle(paste0("Student enrollment by gender"))+
      theme(
        axis.title.x = 
          element_text(
            # color = "blue", 
            # size = 14, 
            # face = "bold"
          ),
        axis.title.y = 
          element_text(
            # color = "#993333", 
            # size = 14, 
            # face = "bold"
          ),
        axis.text.x = 
          element_text(
            # face="bold", 
            # color="#993333", 
            size=10,
            angle= 90,
          ),
        axis.text.y = 
          element_text(
            # face="bold", 
            # color="#993333", 
            # size=14, 
            # angle=90
          ),
      )
    enrollment_for_graph
  })
  
  
  ## UMD plots output
  ### create ractive dataframe
  umd_type_crime_by_year <- reactive({
    #print(input$select_crime)
    req(input$select_crime)
    umd_graph = etl_umd_police_arrest_data[etl_umd_police_arrest_data$description == input$select_crime,]  %>% 
      group_by(year, description) %>% 
      summarise(number_crimes = n())
  })
  
  # creative a plot base don the reactive dataframe
  output$umd_plot = renderPlot({
    g<- ggplot(umd_type_crime_by_year(), aes(x = year, y = number_crimes))+
      geom_bar(stat = "identity")+
      theme(legend.position = "none")+
      ggtitle(paste0("UMD over the years: ", input$select_crime))
    
    g
  })
  
  # Howard Output
  
  #etl_howard_police_arrest_data <- etl_howard_police_arrest_data[etl_howard_police_arrest_data$natures_of_crime == input$natures_of_crime]
  

  
  ### create a howard reactive df
  howard_typea_crime_by_year <- reactive({
    #print(input$select_crime_hu) 
    req(input$select_crime_hu)
    howard_graph = etl_howard_police_arrest_data[etl_howard_police_arrest_data$natures_of_crimes == input$select_crime_hu,]  %>% 
      group_by(year_reported, natures_of_crimes) %>% 
      summarise(number_crimes = n())
  })
  
  
  
    # cnow plot it~~~
  output$howard_graph = renderPlot({
    hu_graph<- ggplot(howard_type_crime_by_year(), aes(x = year_reported, y = natures_of_crimes))+
      geom_bar(stat = "identity")+
      theme(legend.position = "none")+
      ggtitle(paste0("Howard ocver years: ", input$select_crime_hu))
    
    hu_graph
    
    
  })
  
  
  ### create a gt reactive df
  gt_type_crime_by_year <- reactive({
    #print(input$select_crime_gt) 
    req(input$select_crime_gt)
    howard_graph = etl_gt_police_arrest_data[etl_gt_police_arrest_data$incident == input$select_crime_gt,]  %>% 
      group_by(year_occured, incident) %>% 
      summarise(number_crimes = n())
  })
  
  
  
  # gt plot it~~~
  output$gt_graph = renderPlot({
    gt_graph<- ggplot(gt_type_crime_by_year(), aes(x = year_occured, y = incident))+
      geom_bar(stat = "identity")+
      theme(legend.position = "none")+
      ggtitle(paste0("Georgetown ocver years: ", input$select_crime_gt))
    
    gt_graph
    
    
  })
  
}

# server <- function(input, output) {
#   output$distPlot <- renderPlot({
#     x    <- faithful[, 2]
#     bins <- seq(min(x), max(x), length.out = input$bins + 1)
#     hist(x, breaks = bins, col = 'darkgray', border = 'white')
#   })
# }

shinyApp(ui = ui, server = server)