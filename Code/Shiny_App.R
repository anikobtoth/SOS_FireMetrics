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
                          tableOutput(outputId = "Summary")),
                   column(4, 
                          plotOutput(outputId = "areaGraph", width = "500px", height = "280px"), 
                          plotOutput(outputId = "ribbonPlot", width = "500px", height = "280px"))
                 )
        ),
        tabPanel("Detailed table", 
                 tableOutput(outputId = "outTableB"))
      ),
      width = 10), 
    position = "left"
  )
)

server <- function(input, output){
  
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
  
  
  output$outTableB <- renderTable ({
    dataOutputB() %>% select(group, gi, interval, contains("Shortfall")) %>%
      rename_with(fix_names, contains("Shortfall")) %>%
      separate(group, into = c("Year", "TSLF"))
  }, caption = "Shortfalls by tslf and assessment year")
  
  output$Summary <- renderTable ({
    dataOutputB() %>% select(group, gi, interval, contains("Shortfall")) %>% 
      group_by(group) %>% summarise(across(contains("Shortfall"), sum)) %>% 
      separate(group, into = c("Year", "TSLF")) %>%
      rename_with(fix_names, contains("Shortfall"))
  }, caption = "Total shortfall metric by tslf and assessment year")
  
  output$areaGraph <- renderPlot({
    dataOutputB() %>% area_graph()
  })
  
  output$ribbonPlot <- renderPlot({
    dataOutputB() %>% ribbon_plot()
  })
}


### RUN ###
shinyApp(ui, server)




    