library(tidyverse)
library(shiny)
library(cowplot)
library(viridis)

##### assume input is multi-year file with assessment year specified in column ####

# Use Expected_data_input.csv for input data

###### Shiny app ##########

## Global functions ####
TSLF_intervals <- function(dat, tslf){
  GI <- rep(1:10, 2^(seq(0,9)))[1:nrow(dat)]
  intervals <- rep(paste(c("", which(diff(GI)>0)+1), c(which(diff(GI)>0),""), sep = "-") %>% str_replace("^-", "") %>% str_replace("-$", "+"), times = table(GI))
  
  dat <- dat %>% mutate(gi           = GI,
                        interval     = intervals)
  
  
  geointervals <- dat %>% group_by(gi, interval) %>% 
    summarise(across(contains("Area"), sum)) %>%
    mutate(across(contains("Obs"), list(d = ~ExpectedArea-.x))) %>% 
    mutate(across(ends_with("_d"), list(Shortfall = ~ifelse(.x < 0, 0, .x)))) %>%
    mutate(group = paste(dat$yrofas[1], tslf, sep = "_"))
  
  return(geointervals)
}

fix_names <- function(chr){
  strsplit(chr, split = "_") %>% 
    reduce(rbind) %>% data.frame() %>% 
    select_if(~n_distinct(.x)>1) %>% 
    apply(1, paste, collapse = "_") %>% 
    setNames(chr)
}

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
      raw <- input$data$datapath[1] %>% read_csv() %>%
      group_by(Assessment_Year) %>% group_split() %>%
       map(mutate, across(contains("Area"), list(Pct = ~100*.x/sum(.x))))

     oldestyr <- 1950
    
    calc1 <- list()
    for(i in seq_along(raw)){
      yoa <- raw[[i]]$Assessment_Year[1]
      raw[[i]] <- raw[[i]] %>% mutate(LastFire  = ifelse(LastFire == 0, oldestyr-1, LastFire))
      calc1[[i]] <- data.frame(yrofas = yoa, TSLF = 1:(yoa-oldestyr+1)) %>%
        mutate(LastFire = yrofas-TSLF,
               ExpectedPctMIN = 100* ((1-(1/input$tslfMIN))^(TSLF-1)) * (1/input$tslfMIN),
               ExpectedPctMEAN = 100* ((1-(1/input$tslfMEAN))^(TSLF-1)) * (1/input$tslfMEAN),
               ExpectedPctMAX = 100* ((1-(1/input$tslfMAX))^(TSLF-1)) * (1/input$tslfMAX)) %>%
        full_join(raw[[i]], by = "LastFire") %>%
        mutate_if(is.numeric,coalesce,0)
    }

    return(calc1)
    })
  
  output$freqCurves <- renderPlot({
    dataInput1() %>% last() %>%  ## plot for most recent assessment year
      pivot_longer(cols = contains("Pct"), names_to = "variable") %>% 
      ggplot(aes(x = TSLF, y = value, col = variable)) + geom_line(lwd = 1.3) + 
      labs(y = "Percent of ecosystem extent") +
      ggtitle("Frequency curves for most recent assessment year") + 
      scale_color_manual(values = c("orange3", "orangered3", "red3" ,"midnightblue", "cyan4")) +
      theme(plot.title = element_text(size = 16, face = "bold"), legend.position = c(.2, 0.85))
  })
  
  dataOutputB <- reactive({  ## calculate shortfalls
    data <- dataInput1()
    firereturn <- c(MIN = input$tslfMIN, MEAN = input$tslfMEAN, MAX = input$tslfMAX)
    grid <- expand_grid(1:length(data), 1:length(firereturn)) %>% t() %>% data.frame()
    
    purrr::map(grid, ~TSLF_intervals(data[[.x[1]]] %>% select(yrofas, LastFire, TSLF, ends_with("_Pct"), ExpectedArea = ends_with(names(firereturn[.x[2]]))), 
                                     firereturn[.x[2]])) %>% 
      setNames(map_chr(grid, ~paste(names(data)[.x[1]], firereturn[.x[2]], sep = "_tslf"))) %>%
      bind_rows()
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
    outC <- dataOutputB() %>% select(group, gi, interval, contains("Area"), -contains("_d")) %>% 
      separate(group, into = c("Year", "TSLF")) %>% 
      pivot_longer(contains("Area")) %>% 
      mutate(x = ifelse(name == "ExpectedArea", as.numeric(TSLF), as.numeric(Year)), 
             interval = recode(interval, `32+` = "32-63")) %>%
      distinct(Year, gi, interval, name, value, .keep_all = TRUE) %>% 
      filter(!(name == "ExpectedArea" & Year != 2020))
    
    p1 <- ggplot(outC %>% filter(name != "ExpectedArea"), aes(x = x, y = value, fill = as.factor(gi), group = TSLF)) + 
      geom_col(width = 3) + 
      facet_grid(~name, scales = "free") +
      scale_fill_viridis(discrete = T, name = "", labels = unique(outC$interval), option = "B") +
      labs(x = "Observed year", y = "Percent burned (%)") + 
      theme(plot.margin = unit(c(0.2, 0, 0.2, 0.2), "cm")) 
    
    p2 <- ggplot(outC %>% filter(name == "ExpectedArea"), aes(x = as.factor(x), y = value, fill = as.factor(gi), group = TSLF)) +
      geom_col(width = 0.6) +
      facet_grid(~name, scales = "free") +
      scale_fill_viridis(discrete = T, name = "Interval", labels = unique(outC$interval), option = "B") +
      labs(x = "TSLF Benchmark", y = "") + 
      theme(plot.margin = unit(c(.2,.2,.2, 0), "cm"), axis.text.y = element_blank())
    
    title <- ggdraw() + 
      draw_label(
        "Observed and expected percent of areas burned",
        size = 16,
        fontface = 'bold',
        x = 0,
        hjust = -.1
      )
    plot_grid(title, 
              plot_grid(p1 + theme(legend.position = "none"), p2, nrow = 1, rel_widths = c(3,2)),
              ncol = 1,
              rel_heights = c(0.1, 1)) 
    
  })
  
  
  output$ribbonPlot <- renderPlot({
    outD <- dataOutputB() %>% select(group, gi, interval, contains("Shortfall")) %>% 
      group_by(group) %>% summarise(across(contains("Shortfall"), sum)) %>%
      separate(group, into = c("Year", "TSLF")) %>% 
      pivot_longer(contains("Shortfall")) %>% 
      mutate(Year = as.numeric(Year), TSLF = as.numeric(TSLF)) %>%
      mutate(TSLF = ifelse(TSLF == min(TSLF), "Min", ifelse(TSLF == max(TSLF), "Max", "Mean"))) %>%
      pivot_wider(names_from = TSLF, values_from = value)
    if(n_distinct(outD$name)>1){
      outD <- outD %>% 
        mutate(name = recode(name, !!!fix_names(unique(outD$name))))
    }

    ggplot(outD, aes(x = Year, fill = name)) + 
      geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.5, lwd = 0) +
      geom_point(aes(y = Mean, col = name), pch = 16) + 
      geom_line(aes(y = Mean, col = name), lwd = 1) +
      labs(y = "Shortfall metric", x = "Year of condition assessment", fill = "", col = "") + 
      scale_fill_manual(values = c("midnightblue", "cyan4")) +
      scale_color_manual(values = c("midnightblue", "cyan4")) +
      ggtitle("Ecosystem total shortfall") + 
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    
  })
}

  

### RUN ###
shinyApp(ui, server)



    