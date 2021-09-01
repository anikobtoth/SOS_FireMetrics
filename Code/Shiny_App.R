library(shiny)
source("Helper_functions.R")


##### assume input is multi-year file with assessment year specified in column ####

# Use Expected_data_input.csv for input data

###### Shiny app ##########

### User interface 
ui <- fluidPage(
  titlePanel("Ecosystem condition assessment fire metric"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "data", label = "please insert a csv file formatted according to specifications", multiple = FALSE, accept = c(".csv")), 
      numericInput("tslfMIN", "Min TSLF", value = 6),
      numericInput("tslfMEAN", "Mean TSLF", value = 7),
      numericInput("tslfMAX", "Max TSLF", value = 15), 
      width = 2),
    mainPanel(
      tabsetPanel(
        tabPanel("Summaries",
                 fluidRow(
                   column(5, 
                          plotOutput(outputId = "freqCurves", width = "500px", height = "600px")),
                   column(3, 
                          tableOutput(outputId = "Summary"), 
                          downloadButton("download1", "Download .tsv")),
                   column(4, 
                          plotOutput(outputId = "areaGraph", width = "500px", height = "280px"), 
                          plotOutput(outputId = "ribbonPlot", width = "500px", height = "280px"))
                 )
        ),
        tabPanel("Detailed table", 
                 tableOutput(outputId = "outTableB"), 
                 downloadButton("download2", "Download .tsv"))
      ),
      width = 10), 
    position = "left"
  ),
  
  h5("For more information on the calculation of this Fire State metric please see the peer-reviewed paper"),
  h5("Tulloch, A.I.T., McDonald, J., Cosier, P., Sbrocchi, C., Stein, J., Lindenmayer, D., Possingham, H.P., 2018. Using ideal distributions of the time since habitat was disturbed to build metrics for evaluating landscape condition. Ecological Applications 28, 709-720."),
  uiOutput("link")
 )

server <- function(input, output){
  
  url <- a("Access paper", href="https://www.researchgate.net/publication/323461408_Using_ideal_distributions_of_the_time_since_habitat_was_disturbed_to_build_metrics_for_evaluating_landscape_condition")
  output$link <- renderUI({
    tagList("URL link:", url)
  })
  
  dataInput1 <- reactive({  ## reads in the data and combines into one table
    req(input$data)
    firereturn <- c(MIN = input$tslfMIN, MEAN = input$tslfMEAN, MAX = input$tslfMAX)
    raw <- input$data$datapath[1] %>% read_csv() %>%
      expected_fire(firereturn)
    return(raw)
  })
  
  output$freqCurves <- renderPlot({
    dataInput1() %>% freq_curves(yr = 2020)})
  
  dataOutputB <- reactive({  ## calculate shortfalls
    data <- dataInput1()
    firereturn <- c(MIN = input$tslfMIN, MEAN = input$tslfMEAN, MAX = input$tslfMAX)
    shortfalls(data, firereturn)
  })
  
  
  outTable <- reactive({
    dataOutputB() %>% select(group, gi, interval, contains("Shortfall")) %>%
      rename_with(fix_names, contains("Shortfall")) %>%
      separate(group, into = c("Year", "TSLF"))
  })
  
  output$outTableB <- renderTable ({
   outTable()
  }, caption = "Fire State Metric [Similarity to ideal fire state (100 – summed shortfall)] for each Time-Since-Last-Fire Geometric Interval and Assessment Year", 
  caption.placement = "top")
  
  output$Summary <- renderTable ({
    outTable() %>% group_by(Year, TSLF) %>% 
      summarise(across(where(is.double), sum))
  }, caption =  "Final Fire State Metric [Similarity to ideal fire state (100 – summed shortfall)] by Time-Since-Last Fire (TSLF) and Assessment Year", 
  caption.placement = "top")
  
  output$areaGraph <- renderPlot({
    dataOutputB() %>% area_graph()
  })
  
  output$ribbonPlot <- renderPlot({
    dataOutputB() %>% ribbon_plot()
  })
  
  output$download1 <- downloadHandler(
    filename = function() {
       "Fire_State_Summary_Table.tsv"
    },
    content = function(file) {
      vroom::vroom_write(outTable() %>% group_by(Year, TSLF) %>% 
                           summarise(across(where(is.double), sum)), file)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function() {
      "Fire_State_Detailed_Table.tsv"
    },
    content = function(file) {
      vroom::vroom_write(outTable(), file)
    }
  )
}


### RUN ###
shinyApp(ui, server)




    