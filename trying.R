## data set 
try_set <- read_csv("~/Documents/conversion_reporting/data_tot.csv")
x <- try_set

ui <- fluidPage(
  
  # App title ----
  titlePanel("Conversion"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select the random distribution type ----
      selectInput(inputId= "country", 
                  label= "Country:",
                  choices =
                    c("Italy" = "Italy",
                      "Germany" = "Germany",
                      "Spain" = "Spain",
                      "Romania" = "Romania",
                      "Poland" = "Poland",
                      "Belgium" = "Belgium",
                      "Australia" = "Australia")),
      
      # Input: Select the random distribution type ----
      selectInput(inputId = "Category", 
                  label ="Category:",
                  choices =
                    c("Mortgages" = "Mortgages",
                      "Current accounts" = "Current accounts",
                      "Savings" = "Savings",
                      "Investment products" = "Investment products",
                      "Unsecured lending" = "Unsecured lending",
                      "Insurances" = "Insurances"))
      ,
      
            radioButtons(inputId = "rank_order", 
                         label = "Product_rank:",
                         choices =
                           c("1" = "1",
                             "2" = "2",
                             "3" = "3")),
            
            radioButtons(inputId = "NewvsCurrent", 
                         label = "NewvsCurrent:",
                         choices = 
                           c("N2B" = "N2B",
                             "Customer" = "Customer",
                             "NA" = "NA")),
      
           selectInput(inputId = "quarter", 
                        label = "quarter:",
                        choices = 
                          c("2021 Q1" = "2021 Q1",
                            "2021 Q2" = "2021 Q2",
                            "2021 Q3" = "2021 Q3",
                            "2021 Q4" = "2021 Q4",
                            "2022 Q1" = "2022 Q1",
                            "2022 Q2" = "2022 Q2",
                            "2022 Q3" = "2022 Q3"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Funnel plot", plotOutput("plot1")),
                  tabPanel("Conversion 1", plotOutput("plot2")),
                  tabPanel("Conversion 2", plotOutput("plot3")),
                  tabPanel("Traffic", plotOutput("plot4"))
      )
      
    )
  )
)

server <- function(input, output) {
   
    My_Uploaded_Data <- data.table(x) 
      
  
    
    My_Filtered_Data <- reactive({
      My_Filtered_Data <- My_Uploaded_Data %>%
      My_Filtered_Data[country %in% input$country]
    })
    
    My_Filtered_Data2 <- reactive({
      My_Filtered_Data2 <- My_Filtered_Data() %>%
      My_Filtered_Data2[Category %in% input$Category]
    })  
    
    My_Filtered_Data3 <- reactive({
      My_Filtered_Data3 <- My_Filtered_Data2() %>%
      My_Filtered_Data3[rank_order %in% input$rank_order]
    })  
    
    My_Filtered_Data4 <- reactive({
      My_Filtered_Data4 <- My_Filtered_Data3() %>%
      My_Filtered_Data4[quarter %in% input$rank_order]
    }) 
    
  # plotly object
  output$plot1 <- renderPlot({
    My_Filtered_Data4 () %>%
    plot_ly() %>%
      add_trace(
        type = "funnel",
        name= "Mobile",
        y = c("PP", "SF", "FF", "AO"),
        x = c(
          round(x$`Product page`[x$Device=="Mobile"], digits=2), 
          round(x$`Start application`[x$Device=="Mobile"], digits=2), 
          round(x$`Finish application`[x$Device=="Mobile"], digits=2),
          round(x$`Account opening`[x$Device=="Mobile"], digits=2)),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)",
                                "rgb(82, 81, 153)", "rgb(82, 81, 153)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
      
      add_trace(
        type = "funnel",
        name = 'Desktop',
        y = c("PP", "SF", "FF", "AO"),
        x = c(
          round(x$`Product page`[x$Device=="Desktop"], digits=2), 
          round(x$`Start application`[x$Device=="Desktop"], digits=2), 
          round(x$`Finish application`[x$Device=="Desktop"], digits=2),
          round(x$`Account opening`[x$Device=="Mobile"], digits=2)),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)",
                                "rgb(255,098,000)", "rgb(82, 81, 153)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
      
      layout(yaxis = list(categoryarray = c("PP", "SF", "FF", "AO"),
                          tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



server <- function(input, output){
  
  df_dat <- reactive({
    
    # make sure inputs are not NULL
    req(input$country, input$Category, input$rank_order, input$NewvsCurrent, input$quarter) 
    
    # filter data according to inputs made by the user
    df_dat <- filter(try_set, Category %in% input$Category, 
                     country %in% input$country, 
                     rank_order %in% input$rank_order,
                     input$NewvsCurrent %in% input$NewvsCurrent) 
    
    return(df_dat)
  })
  
  # Ensures that our filter works properly
  observe(print(str(df_dat())))
  
  # create a graph 
  output$graph1 <- renderPlot({
    
    # use to debug:
    # browser()
    
    # make sure filtered data is not NULL
    req(df_dat())
    
    # plot filtered data
    ggplot(df_dat(), aes(x = Year, y = Amount)) +
      geom_line(aes(colour = Country)) +
      geom_point()
  })
  
}

shinyApp(ui = ui, server = server)




server <- function(input, output, session) {
  
  country <- 
    reactive({
      switch(input$country, 
             "Italy" = country$Italy,
             "Australia" = country$Australia,
             "Germany" = country$Germany,
             "Romania" = country$Romania,
             "Poland" = country$Poland,
             "Belgium" = country$Belgium,
             "Spain" = country$Spain)
    })
  
  Category <- 
    reactive({
      switch(input$Category, 
             "Current accounts" = Category$`Current accounts`,
             "Mortgages" = Category$Mortgages,
             "Savings" = Category$Savings,
             "Unsecured lending" = Category$`Unsecured lending`,
             "Insurances" = Category$Insurances,
             "Investment products" = Category$`Investment products`)
    })
  
  #   rank_order <- 
  #      reactive({
  #      switch(input$rank_order, 
  #                                "0" =  rank_order$`0`,
  #                                "1" =  rank_order$`1`,
  #                                "2" =  rank_order$`2`,
  #                                "3" =  rank_order$`3`,
  #                                "4" =  rank_order$`4`)
  #    })
  
  #    NewvsCurrent <- 
  #      reactive({
  #      switch(input$NewvsCurrent, 
  #                                "N2B" = NewvsCurrent$N2B,
  #                                "Customer" = NewvsCurrent$Customer)
  #                                 #,
  #                                 #"NA" = NewvsCurrent$NA
  #    })
  
  #    quarter <- 
  #      reactive({
  #      switch(input$quarter, 
  #                               "2021 Q1" = quarter$`2021 Q1`,
  #                               "2021 Q2" = quarter$`2021 Q2`,
  #                               "2021 Q3" = quarter$`2021 Q3`,
  #                               "2021 Q4" = quarter$`2021 Q4`,
  #                               "2022 Q1" = quarter$`2022 Q1`,
  #                               "2022 Q2" = quarter$`2022 Q2`,
  #                               "2022 Q3" = quarter$`2022 Q3`)
  #    })
  
  # plotly object
  output$plot1 <- renderPlot({
    
    plot_ly(x) %>%
      dplyr::filter(country == input$country) %>%
      dplyr::filter(country == input$country) %>%
      add_trace(
        type = "funnel",
        name= "Mobile",
        y = c("PP", "SF", "FF", "AO"),
        x = c(
          round(x$`Product page`[x$Device=="Mobile"], digits=2), 
          round(x$`Start application`[x$Device=="Mobile"], digits=2), 
          round(x$`Finish application`[x$Device=="Mobile"], digits=2),
          round(x$`Account opening`[x$Device=="Mobile"], digits=2)),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)",
                                "rgb(82, 81, 153)", "rgb(82, 81, 153)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
      
      add_trace(
        type = "funnel",
        name = 'Desktop',
        y = c("PP", "SF", "FF", "AO"),
        x = c(
          round(x$`Product page`[x$Device=="Desktop"], digits=2), 
          round(x$`Start application`[x$Device=="Desktop"], digits=2), 
          round(x$`Finish application`[x$Device=="Desktop"], digits=2),
          round(x$`Account opening`[x$Device=="Mobile"], digits=2)),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)",
                                "rgb(255,098,000)", "rgb(82, 81, 153)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white", "white"))) %>%
      
      layout(yaxis = list(categoryarray = c("PP", "SF", "FF", "AO"),
                          tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

