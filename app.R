########## Load necessary libraries dynamically ########## 

library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(readr)
library(leaflet.extras)
library(ggplot2)
library(plotly)
library(tidyr)
library(data.table)
library(highr)
library(ggthemes)
library(highcharter)
library(DT)
library(tidytext)
library(wordcloud)
library(RColorBrewer)


########## Loading and processing data ########## 

data <- read.csv("./Data/updated_odddata_Jun17.csv")

data <- data %>%
  mutate(
    Created.Date = as.Date(Created.Date, format = "%Y-%m-%d"),
    Closed.Date = as.Date(Closed.Date, format = "%Y-%m-%d"),
    Response.Times = abs(round(difftime(Closed.Date, Created.Date, units = "hour")))
  )

data = data %>%
  mutate(
    Borough = case_when(
      Borough %in% c("BROOKLYN") ~ "Brooklyn",
      Borough %in% c("MANHATTAN") ~ "Manhattan",
      Borough %in% c("QUEENS") ~ "Queens",
      Borough %in% c("BRONX") ~ "Bronx",
      Borough %in% c("STATEN ISLAND") ~ "Staten Island"    )
  ) %>%
  filter(!is.na(Borough))



# Renaming and shortening Community Board names
data$Community.Board = gsub("QUEENS", "QN", data$Community.Board)
data$Community.Board = gsub("MANHATTAN", "MN", data$Community.Board)
data$Community.Board = gsub("BRONX", "BX", data$Community.Board)
data$Community.Board = gsub("BROOKLYN", "BK", data$Community.Board)
data$Community.Board = gsub("STATEN ISLAND", "SI", data$Community.Board)


##### CREATE DATAFRAME FOR CD DATA TABLE
CD.DT.data = data %>%
  group_by(Community.Board) %>%
  summarize(Borough = unique(Borough[1]),
            Neighborhoods = unique(Neighborhoods),
            Environmental_calls = sum(Complaint.Category=="Environmental concerns"),
            Housing_calls = sum(Complaint.Category=="Housing concerns"),
            Noise_calls = sum(Complaint.Category=="Noise-related complaints"),
            Transportation_calls = sum(Complaint.Category=="Transportation problems"),
            Sanitation_calls = sum(Complaint.Category=="Sanitation issues"),
            Other_calls = sum(Complaint.Category=="Others"),
            Safety_calls = sum(Complaint.Category=="Safety and security"))



########## CODE FOR R SHINY APP ##########


########## Define UI

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Visualizing NYC 311 Noise Complaints"),
  tags$h4("Choose your level of analysis using the various tabs!", class = "text-muted"),
  
  tabsetPanel(
    
    tabPanel("New York City Level", 
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("complaintCategory", "Select Complaint Category:",
                                    c("Noise-related complaints",
                                      "Transportation problems",
                                      "Environmental concerns",
                                      "Housing concerns",
                                      "Safety and security",
                                      "Sanitation issues",
                                      "Others"
                                    ),
                                    selected = "Noise-related complaints"),
                 checkboxGroupInput("borough", "Select Borough:",
                                    choices = unique(data$Borough),
                                    selected = unique(data$Borough))
               ),
               mainPanel(
                 leafletOutput("map"),
                 hr(),
                 h3("An Overview of Complaints"),
                 plotlyOutput("type_plot"),
                 plotlyOutput("borough_plot"),
                 plotlyOutput("time_plot")
               )
             )
    ),
    
    tabPanel("Borough Level",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("complaintCategory", "Select Complaint Category:",
                                    c("Noise-related complaints",
                                      "Transportation problems",
                                      "Environmental concerns",
                                      "Housing concerns",
                                      "Safety and security",
                                      "Sanitation issues",
                                      "Others"
                                    ),
                                    selected = "Noise-related complaints")
               ),
               mainPanel(
                 uiOutput("maps")
               )
             )
    ),
    
    tabPanel("NYC Community Districts Level", 
             fluidRow(
               column(12, selectInput("CD_bar_borough", "Select Borough",
                                      choices = unique(data$Borough),
                                      selected = "Manhattan")
               ),
               column(12, plotlyOutput("CD_Manhattan")),
               column(12,dataTableOutput("CD_NYC311_table"))
             )
    ),
    
    tabPanel("Agency Level",
             sidebarLayout(
               sidebarPanel(
                 selectInput("selectedAgency", "Select Agency:", choices = unique(data$Agency)),
                 selectInput("AgencycomplaintCategory", "Select Complaint Category:",
                             choices = c("All", "Noise-related complaints", "Safety and security", "Transportation problems", "Environmental concerns", "Housing concerns", "Sanitation issues", "Others")),
                 dateRangeInput("dateRange", "Select Date Range:", start = min(data$Created.Date, na.rm = TRUE), end = max(data$Created.Date, na.rm = TRUE))
               ),
               mainPanel(
                 h3("Section 1: Overview of Complaints and Response Times by Agency"),
                 plotlyOutput("agency_timeSeriesPlot"),
                 plotlyOutput("agency_responseTimePlot"),
                 h3("Section 2: Most Commonly Used Words received in Complaints by Agency"),
                 plotOutput("agency_wordCloud"),
                 plotlyOutput("agency_wordFreqPlot"))
             )
    )
    
  )
)





########## Define server logic

server <- function(input, output) {

  filteredDataNYC <- reactive({
    data %>%
      filter(Complaint.Category %in% input$complaintCategory,
             Borough %in% input$borough,
             !is.na(Longitude),  # Exclude missing longitude
             !is.na(Latitude),   # Exclude missing latitude
             Longitude != 0,     # Exclude invalid longitude
             Latitude != 0       # Exclude invalid latitude
      )
  })
  
  filteredDataTS <- reactive({
    data %>%
      filter(Complaint.Type %in% input$complaintTypeTS)
  })
  
  CD_table_data = CD.DT.data
  
  
  output$CD_NYC311_table = renderDataTable({
    
    datatable(CD_table_data,
              rownames = FALSE, 
              colnames = c("Community Board", 
                           "Borough", 
                           "Neighborhoods Included", 
                           "Calls About Environmental Concerns",
                           "Calls About Housing Issues",
                           "Calls About Noise Complaints", 
                           "Calls About Transportation Issues",
                           "Calls About Sanitation Concerns",
                           "Other Miscellaneous Concerns",
                           "Calls About Safety Issues"),
              filter = list(position="top"), 
              options = list(
                dom = "Bfrtip",
                buttons = I("colvis"),
                language = list(sSearch = "Filter:"),
                pageLength = 10,
                lengthMenu = c(10, 15, 25)
              ),
              extensions = c("Buttons", "Responsive"))
  })
  
  CD_Manhattan_data = reactive({
    data[data$Borough == input$CD_bar_borough, ]
  })
  
  output$CD_Manhattan = renderPlotly({
    ggplot(CD_Manhattan_data(), aes(x = Community.Board, fill = Complaint.Category)) +
      geom_bar(position = "stack") +
      labs(title = "Breakdown Of NYC311 By Community District",
           x = "Community Districts",
           y = "Number of NYC311 Calls") +
      scale_fill_manual(values = c("Noise-related complaints" = "orange1", 
                                   "Transportation problems" = "dodgerblue",
                                   "Environmental concerns" = "lawngreen",
                                   "Sanitation issues" = "black",
                                   "Housing concerns" = "yellow",
                                   "Safety and security" = "indianred1",
                                   "Others" = "grey")) +
      theme_minimal()
  })
  
  
  output$map <- renderLeaflet({
    leaflet(data = filteredDataNYC()) %>%
      addTiles() %>%
      addHeatmap(lng = ~Longitude, lat = ~Latitude, intensity = ~1,
                 blur = 20, max = 0.05, radius = 15)
  })
  
  output$type_plot <- renderPlotly({
    type_summary <- data %>%
      group_by(Complaint.Category) %>%
      summarise(Total_Complaints = n()) %>%
      arrange(desc(Total_Complaints))  # Sort in descending order of complaints
    
    p <- ggplot(type_summary, aes(x = reorder(Complaint.Category, -Total_Complaints), y = Total_Complaints, text = paste("Total Complaints:", Total_Complaints))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Total Complaints by Complaint Category", x = "Complaint Type", y = "Total Complaints") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")  # Make it interactive with tooltips
  })
  
  # Render interactive plot for total complaints by borough
  output$borough_plot <- renderPlotly({
    borough_summary <- data %>%
      group_by(Borough) %>%
      summarise(Total_Complaints = n()) %>%
      arrange(desc(Total_Complaints))  # Ensure data is sorted in descending order
    
    p <- ggplot(borough_summary, aes(x = reorder(Borough, -Total_Complaints), y = Total_Complaints, text = paste("Total Complaints:", Total_Complaints))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Total Complaints by Borough", x = "Borough", y = "Total Complaints") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")  # Make it interactive with tooltips
  })
  
  output$time_plot <- renderPlotly({
    data_summary <- data %>%
      group_by(Created.Date) %>%
      summarise(complaint_count = n())
    
    plot_ly(data_summary, x = ~Created.Date, y = ~complaint_count, type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text',
            text = ~paste('Date:', Created.Date, '<br>Number of Complaints:', complaint_count),
            marker = list(size = 10, color = 'dodgerblue')) %>%
      layout(
        title = list(text = "Number of Complaints Over Time", x = 0),  # Left justify the title
        xaxis = list(title = "Date"),  # Set the x-axis range
        yaxis = list(title = "Number of Complaints"),
        hovermode = 'closest')
  })
  
  boroughs <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
  
  # Create a map output for each borough
  output$maps <- renderUI({
    tabs <- lapply(boroughs, function(b) {
      tabPanel(b,
               leafletOutput(outputId = paste0("map_", b))
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  # Generate maps based on the selected complaint category and borough
  for (borough in boroughs) {
    local({
      b <- borough
      output[[paste0("map_", b)]] <- renderLeaflet({
        leaflet(data %>% filter(Borough == b, Complaint.Category == input$complaintCategory)) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addMarkers(
            lng = ~Longitude,
            lat = ~Latitude,
            popup = ~paste(Complaint.Type, Descriptor, sep = "<br/>"),
            clusterOptions = markerClusterOptions()
          )
      })
    })
  }
  
  
  agency_filtered_data <- reactive({
    if (input$AgencycomplaintCategory == "All") {
      data %>%
        filter(Agency == input$selectedAgency, Created.Date >= input$dateRange[1], Created.Date <= input$dateRange[2])
    } else {
      data %>%
        filter(Agency == input$selectedAgency, Complaint.Category == input$AgencycomplaintCategory, Created.Date >= input$dateRange[1], Created.Date <= input$dateRange[2])
    }
  })
  
  output$agency_timeSeriesPlot <- renderPlotly({
    req(nrow(agency_filtered_data()) > 0)
    title_text <- sprintf("%s Total Number of Complaints per Month", input$selectedAgency)
    p <- ggplot(agency_filtered_data(), aes(x = Created.Date, fill = Complaint.Type)) +
      geom_histogram(binwidth = 30, position = "identity", alpha = 0.5) +
      labs(title = title_text, x = "Date", y = "Count of Complaints") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$agency_wordCloud <- renderPlot({
    data <- agency_filtered_data()
    req(nrow(data) > 0)
    words <- data %>%
      unnest_tokens(word, Descriptor) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      top_n(50, n)
    if (nrow(words) > 0) {
      wordcloud(words$word, words$n, max.words = 100, random.order = FALSE, rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
    } else {
      print("No words to display in word cloud.")
    }
  })
  
  output$agency_wordFreqPlot <- renderPlotly({
    data <- agency_filtered_data()
    req(nrow(data) > 0)
    words <- data %>%
      unnest_tokens(word, Descriptor) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      top_n(50, n)
    p <- ggplot(words, aes(x = reorder(word, n), y = n)) +
      geom_col() +
      labs(title = sprintf("Top 50 most common words received in complaints by %s", input$selectedAgency), x = "Words", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$agency_responseTimePlot <- renderPlotly({
    req(nrow(agency_filtered_data()) > 0)
    df <- agency_filtered_data() %>%
      group_by(Complaint.Type) %>%
      summarise(AverageResponse = mean(Response.Times, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(AverageResponse))
    title_text <- sprintf(" Average Response Time by Complaint Type", input$AgencycomplaintCategory)
    p <- ggplot(df, aes(x = reorder(Complaint.Type, -AverageResponse), y = AverageResponse)) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = title_text, x = "Complaint Type", y = "Average Response Time (hours)")
    ggplotly(p)
  })
  
}

########## Run the application

shinyApp(ui = ui, server = server)




