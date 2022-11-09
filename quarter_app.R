try_set <- read_csv("~/Documents/conversion_reporting/data_tot.csv")

x <- as.data.frame(try_set) %>%
  dplyr::mutate(
    quarter = as.yearqtr(quarter))

#x <- as.data.frame(try_set) %>%
#  dplyr::mutate(
#    quarter = as.yearqtr(quarter)) %>%
#  dplyr::mutate(
#    month = case_when(
#      str_detect(quarter, "Q1") ~ 1,
#      str_detect(quarter, "Q2") ~ 4,
#      str_detect(quarter, "Q3") ~ 7,
#      str_detect(quarter, "Q4") ~ 10
#    )
#  ) %>%
#  dplyr::mutate(
#    day = 1
#  ) %>%
#  dplyr::mutate(
#    date2 = paste(day, month, sep="-")
#  ) %>%
#  dplyr::mutate(
#    Year = year(quarter)
#  ) %>%
#  dplyr::mutate(
#    date3 = as.Date(paste(date2, Year, sep ="-"))
#  )
  

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
                  choices = unique(x$country)),
      
      # Input: Select the random distribution type ----
      selectInput(inputId = "Category", 
                  label ="Category:",
                  choices = unique(x$Category)),
      
      radioButtons(inputId = "rank_order", 
                   label = "Product_rank:",
                   choices = unique(x$rank_order)),
      
      radioButtons(inputId = "NewvsCurrent", 
                   label = "NewvsCurrent:",
                   choices = unique(x$NewvsCurrent)),
      
      selectInput(inputId = "quarter", 
                  label = "quarter:",
                  choices = unique(x$quarter))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Funnel plot", plotlyOutput("plot1")),
                  tabPanel("Conversion 1", plotlyOutput("plot2")),
                  tabPanel("Conversion 2", plotlyOutput("plot3")),
                  tabPanel("Traffic", plotlyOutput("plot4"))
      )
      
    )
  )
) 


server <- function(input, output, session) {
  
  # create graph 1
  output$plot1 <- renderPlotly({
    
    df_dat <- filter(x, country == input$country, 
                     Category ==input$Category,
                     NewvsCurrent== input$NewvsCurrent,
                     rank_order== input$rank_order,
                     quarter ==input$quarter)
    
    plot_ly() %>%
      add_trace(
        type = "funnel",
        name= "Mobile",
        y = c("Product page", "Start flow", "Finishing flow", "Account opening"),
        x = c(
          round(df_dat$`Product page`[df_dat$Device=="Mobile"], digits=2), 
          round(df_dat$`Start application`[df_dat$Device=="Mobile"], digits=2), 
          round(df_dat$`Finish application`[df_dat$Device=="Mobile"], digits=2),
          round(df_dat$`Account opening`[df_dat$Device=="Mobile"], digits=2)),
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
        y = c("Product page", "Start flow", "Finishing flow", "Account opening"),
        x = c(
          round(df_dat$`Product page`[df_dat$Device=="Desktop"], digits=2), 
          round(df_dat$`Start application`[df_dat$Device=="Desktop"], digits=2), 
          round(df_dat$`Finish application`[df_dat$Device=="Desktop"], digits=2),
          round(df_dat$`Account opening`[df_dat$Device=="Mobile"], digits=2)),
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
  
  # create graph 2
  output$plot2 <- renderPlotly ({
    
    df_dat <- filter(x, country == input$country, 
                     Category ==input$Category,
                     NewvsCurrent== input$NewvsCurrent,
                     rank_order== input$rank_order)
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$quarter))) %>%
      add_trace(y = ~df_dat$con_1[df_dat$Device=="Desktop"], 
                type='bar', 
                name="con_1_Desktop",
                text = df_dat$con_1[df_dat$Device=="Desktop"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)')) %>%
      add_trace(y =~df_dat$con_1[df_dat$Device=="Mobile"], 
                type='bar', 
                name="con_1_Mobile",
                text = df_dat$con_1[df_dat$Device=="Mobile"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)')) %>%
      add_trace(y =~df_dat$conv_tot[df_dat$Device=="Desktop"], 
                type='bar', 
                name="con_tot_Desktop",
                text = df_dat$conv_tot[df_dat$Device=="Desktop"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(52,150,81)')) %>%
      add_trace(y =~df_dat$conv_tot[df_dat$Device=="Mobile"], 
                type='bar', 
                name="con_tot_Mobile",
                text = df_dat$conv_tot[df_dat$Device=="Mobile"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(82,81,153)')) %>%
      layout(title = "",
             xaxis = list(title = "",
                          textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                          showline = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             yaxis = list(title = "",
                          range =c(0,100),
                          showticklabels = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          # autotick = TRUE,
                          # dtick = 10,
                          ticks = 'inside',
                          tickcolor = 'rgb(204, 204, 204)',
                          # tickwidth = 1,
                          # ticklen = 10,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             barmode = 'group',
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
  
  # create graph 3
  output$plot3 <- renderPlotly ({
    
    df_dat <- filter(x, country == input$country, 
                     Category ==input$Category,
                     NewvsCurrent== input$NewvsCurrent,
                     rank_order== input$rank_order)
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$quarter))) %>%
      add_trace(y = ~df_dat$con_2[df_dat$Device=="Desktop"], 
                type='bar', 
                name="con_2_Desktop",
                text = df_dat$con_2[df_dat$Device=="Desktop"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)')) %>%
      add_trace(y =~df_dat$con_2[df_dat$Device=="Mobile"], 
                type='bar', 
                name="con_2_Mobile",
                text = df_dat$con_2[df_dat$Device=="Mobile"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)')) %>%
      layout(title = "",
             xaxis = list(title = "",
                          textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                          showline = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             yaxis = list(title = "",
                          range =c(0,100),
                          showticklabels = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          # autotick = TRUE,
                          # dtick = 10,
                          ticks = 'inside',
                          tickcolor = 'rgb(204, 204, 204)',
                          # tickwidth = 1,
                          # ticklen = 10,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             barmode = 'group',
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
  
  # create graph 4
  output$plot4 <- renderPlotly ({
    
    df_dat <- filter(x, country == input$country, 
                     Category ==input$Category,
                     NewvsCurrent== input$NewvsCurrent,
                     rank_order== input$rank_order)
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$quarter))) %>%
      add_trace(y = ~df_dat$`Product page`[df_dat$Device=="Desktop"], 
                type='scatter', 
                mode ='lines',
                name="visits_desktop",
                text = df_dat$`Product page`[df_dat$Device=="Desktop"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      add_trace(y =~df_dat$`Product page`[df_dat$Device=="Mobile"], 
                type='scatter', 
                mode ='lines',
                name="visits_mobile",
                text = df_dat$`Product page`[df_dat$Device=="Mobile"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)'),
                line = list(color = 'rgb(96,166,218)')) %>%
      layout(title = "",
             xaxis = list(title = "",
                          textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                          showline = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             yaxis = list(title = "",
                          #range =c(0,100),
                          showticklabels = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          # autotick = TRUE,
                          # dtick = 10,
                          ticks = 'inside',
                          tickcolor = 'rgb(204, 204, 204)',
                          # tickwidth = 1,
                          # ticklen = 10,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
  })
  
}

shinyApp(ui = ui, server = server)