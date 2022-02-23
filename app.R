# libraries ----
library(DT)
library(magrittr)
library(dplyr)
library(stats)
library(ggplot2)
library(shiny)

#----------------
# action button module 
#---------------
actionBtn_UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("btn"), "Filter")
  )
}

actionBtn_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    button1 <- reactive(input$btn)
  })
  
}


#----------------
# date range module 
#---------------
dateRange_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    dateRangeInput(
      ns("date_range"),
      label="Please enter a date",
      start = Sys.Date() - 600,
      end = Sys.Date(),
      min = Sys.Date() - 3500,
      max = Sys.Date(), 
      format = "yyyy-mm-dd")
  )
}

dateRange_server <- function(id){
  moduleServer(id, function(input, output, session) {
    list(
      min_date = reactive({as.Date(input$date_range[1])}), # note: not "<-"
      max_date = reactive({as.Date(input$date_range[2])})
    )
  })
}


#----------------
#  filter mpay payment module 
# filtering and summarising mpay payment table
#---------------

filter_Payment_UI <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}


filter_Payment_server <- function(id, dataset, date_range2) 
{
  moduleServer(id, function(input, output, session) {
    data_summary <- reactive ({
      req(date_range2$min_date(), date_range2$max_date())
      if(date_range2$min_date() <= date_range2$max_date()) {
        dataset %>%
          filter(only_date >= date_range2$min_date() & only_date <= date_range2$max_date()) %>%
          group_by(only_date) %>%
          summarise(number_of_transactions = length(unique(id)),
                    trans_volume = sum(service_amount),
                    trans_revenue = sum(collected_service_charge)) %>%
          ungroup()
      } else {
        stop("Please make sure your date selection makes sense")
      }
    })
  })
}

#=================================================================

#----------------
# plot transaction volume module 
#---------------
transVolumeChart_UI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shinydashboard::tabBox(width=12, title=id, 
                           tabPanel(icon("chart-line"),
                                    plotOutput(ns("chart")) 
                           ),
                           tabPanel(icon("table"),
                                    DT::dataTableOutput(ns("mod_table"))
                           )
    )
  )
}


transVolumeChart_Server <- function(id, btnTwo, data_to_plot){
  moduleServer(id, function(input, output, session) { 
    output$chart <- renderPlot({
      req(btnTwo())
      isolate(
        data_to_plot() %>%
          ggplot(aes(x = only_date)) +  
          geom_line(aes(y = trans_volume), size = 1, color = "darkblue") +
          theme_minimal() +
          theme(plot.title=element_text(size=15,
                                        #face="bold",
                                        family="Comic San MS",
                                        color="black",
                                        hjust=0.5,
                                        lineheight=1.2),  # title
                panel.background = element_rect(fill = 'white'),  # Background color white gray97, gray95
                plot.subtitle=element_text(size=9,
                                           family="American Typewriter",
                                           face="bold",
                                           hjust=0.5),  # subtitle
                plot.caption=element_text(size=12),  # caption
                axis.title.x=element_text(vjust= 2,
                                          #hjust = -15,
                                          size=13.5),  # X axis title
                axis.title.y=element_text(size=13.5,
                                          vjust= 3),  # Y axis title
                axis.text.x=element_text(size=11.5,
                                         hjust = 0.5, # X axis text position (h = horizontal)
                                         vjust = 1,
                                         angle = 0),  
                axis.text.y=element_text(size=11.5,
                                         hjust = 1,
                                         vjust = 1), # Y axis text
                
                #legend.position = c(.83, .9) # legend position
                
                legend.position=c(0,-1), legend.justification=c(0,-6)
          ))
    })
    
    output$mod_table <- renderDataTable({
      req(btnTwo())
      transaction_volume <- data_to_plot() %>%
        select(only_date, trans_volume)
      isolate(
        DT::datatable(transaction_volume)
      )
    })
  }) 
}

#===========================================================================
ui_try4 <- fluidPage(
  
  shinyjs::useShinyjs(),
  tagList(
            dateRange_UI("test-body"),
            actionBtn_UI("test-btn"),
            br(),
            filter_Payment_UI("filter_Payment"),
            br(),
            transVolumeChart_UI("trans_Volume")
        )
)

server_try4 <- function(input, output, session) {
  
  btn_Val <- actionBtn_server("test-btn")
  
  selected_dates <- dateRange_server("test-body")
  
  data_for_plot <- filter_Payment_server(id="filter_Payment", 
                                             dataset = transaction_data,
                                             date_range2 = selected_dates)
  
  transVolumeChart_Server(id="trans_Volume", btnTwo = btn_Val, 
                          data_to_plot = data_for_plot)
  
}
shinyApp(ui = ui_try4, server = server_try4)
