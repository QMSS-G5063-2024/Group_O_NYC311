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

# removing some data that has wrong CD labels
filter = (data$Borough=="Manhattan" & data$Community.Board == "08 BX") | 
  (data$Borough=="Bronx" & data$Community.Board == "01 QN")

data = data[! filter, ]

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
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Poppins:400,500,600,700"),
    tags$style(HTML("
      body {
        font-family: 'Poppins', sans-serif;
        background-color: #f8f9fa;
        color: #343a40;
      }
      .header {
        padding: 20px 0;
        text-align: center;
        background: linear-gradient(135deg, #3399ff, #0000cd);
        color: #fff;
        margin-bottom: 20px;
        border-radius: 5px;
      }
    "))
  ),
  tags$div(class = "header",
           h1(HTML("<strong>Analyzing NYC311 Complaint Data</strong>")),  
           p(HTML("<i>Providing A Snapshot Of The Daily Lived Experiences Of New Yorkers</i>"),
             HTML("<br>"),
             HTML("<i>From Different Neighborhoods Across The City.</i>"))
  ),
  
  tags$h2(HTML("<strong><i>Introduction</i></strong>"), 
          style = "text-align: center;"),
  
  tags$h5("NYC311 is a platform where residents in New York City can report, inquire, and seek help on a vast array of issues covering areas including but not limited to public transport, environmental concerns, sanitation, and safety issues. Data from all calls made to NYC311 are made publicly available and provide an opportunity to understand what challenges residents of New York City go through on a daily basis. Data from NYC311 can be accessed here:",
          tags$a("NYC311 Data", href="https://data.cityofnewyork.us/Social-Services/311-Service-Requests-from-2010-to-Present/erm2-nwe9/about_data"),
          style = "text-align: justify;"),
  
  tags$h5("By exploring and visualizing key aspects of the NYC311 dataset, we hope to provide a deeper understanding on the types of issues and concerns that residents have, and also reveal underlying patterns and trends related to the NYC311 calls and their reported issues.",
          style = "text-align: justify;"),
  
  tags$h5("Below, we provide several options to visualize the NYC311 data across several dimensions, including at the city-wide level, the borough-level, the community district-level, and at the NYC agency level. Click on the respective tabs below to view visualizations of the data along these dimensions.",
          style = "text-align: justify;"),
  
  hr(),
  
  tags$h4(HTML("<i>Choose your level of analysis using the various tabs below!</i>"), class = "text-muted",
          style = "text-align: center;"),
  
  tags$br(),  # Add an empty line space
  
  
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
                 h2(HTML("<strong>City-Level Analysis</strong>")),
                 tags$h4("This tab provides a broad overview of all the NYC311 call complaints in New York City within the month of June 2017. You can use the options in the side panel to select the complaint categories you want to include, as well as which boroughs you want to include in the visualizations below.",
                         style = "text-align: justify;"),
                 hr(),
                 h3(HTML("<strong>Heat Map Of NYC311 Complaints</strong>")),
                 tags$h4("The heat map below displays the concenration and variation in the number of NYC311 call across the entire city. The concentrations range from low (blue) to high (red).",
                         style = "text-align: justify;"),
                 leafletOutput("map"),
                 hr(),
                 h3(HTML("<strong>An Overview of Complaints</strong>")),
                 tags$h4("Displayed below are visualizations describing trends in NYC311 data for call complaints in June 2017. These visualizations provide an overview of the types of issues New Yorkers encountered, the distribution of NYC311 calls across the five boroughts, as well as temporal trends in NYC311 call numbers. The original data contained 187 different unique complaint types which we have grouped into these 7 broad categories.",
                         style = "text-align: justify;"),
                 tags$br(),
                 tags$h4("The graph below shows the breakdown of NYC311 calls by their complaint categories. The most frequently encountered type of issue by New Yorkers was noise-related incidences and disriptions. Meanwhile, the least frequently reported type of complaints were those related to safety and security concerns.",
                         style = "text-align: justify;"),
                 plotlyOutput("type_plot"),
                 tags$br(),
                 tags$h4("The graph below shows the distribution of NYC311 call numbers across the five boroughs in New York City. We can see that the largest number of NYC311 calls were made in Brooklyn, while Staten Island had the lowest number of NYC311 calls.",
                         style = "text-align: justify;"),
                 tags$br(),
                 plotlyOutput("borough_plot"),
                 tags$br(),
                 tags$h4("The graph below shows the temporal trends in the total number of NYC311 calls in the month of June 2017.",
                         style = "text-align: justify;"),
                 tags$br(),
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
                 h2(HTML("<strong>Borough-Level Cluster Map Of NYC311 Calls</strong>")),
                 tags$h4("Moving on from the city-level analysis, this tab presents cluster maps to visualize the geographical concentrations and locations of NYC311 calls within each of the five boroughs in New York City.",
                         style = "text-align: justify;"),
                 hr(),
                 tags$h4("Use the sub-tabs below to select the borough that you wish to explore. You can also select which kind of NYC311 issues and complaints you want to include in your exploration in the side bar on the left.",
                         style = "text-align: justify;"),
                 uiOutput("maps"),
                 tags$br(),
                 tags$br(),
                 tags$br(),
                 tags$br()
               )
             )
    ),
    
    tabPanel("NYC Community Districts Level", 
             fluidRow(
               h2(HTML("<strong>Community District-Level Analysis</strong>")),
               h4("New York City comprises of 59 Community Districts, each of them containing different neighborhoods in the city. This tab provides details of NYC311 calls at the Community-District Level and is especially useful for understanding variations in NYC311 issues encountered by residents across the different neighborhoods.",
                  style = "text-align: justify;"),
               hr(),
               h4("This segmented bar chart shows the variation in the total number of NYC311 complaints in each Community Distirct, as well as the breakdown of NYC311 calls by complaint categoreis within each Community District. Use the dropdown menu below to view the information on the Community Districts within each borough."),
               br(),
               column(12, selectInput("CD_bar_borough", "Select The Borough You Want To Explore",
                                      choices = unique(data$Borough),
                                      selected = "Manhattan")
               ),
               column(12, plotlyOutput("CD_Manhattan")),
               h4("     "),
               h4("     "),
               h4("This interactive datable below displays the borough and neighborhoods contained within each of the 59 Community Districts in NYC and reports the number of NYC311 calls under each complaint category. You can sort and filter the results by any of the columns. If you want to view details on a specific neighborhood, you can enter its name in the filter search bar below."),
               br(),
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
                 h2(HTML("<strong>Analysing NYC311 Calls By Government Agency</strong>")),
                 h4("On this tab, you can explore the types and volumes of NYC311 complaints that each agency responded to in June 2017. This provides a deeper understanding of how government agencies are responding to the issues encountered by residents in New York City.",
                    style = "text-align: justify;"),
                 h4("Select which agency you would like to explore using the dropdown menu in the side panel. You can also filter further by complaint category as well as a specific subset of dates within June 2017.",
                    style = "text-align: justify;"),
                 hr(),
                 h3(HTML("<strong>Overview of Complaints and Response Times by Agency</strong>")),
                 h4("The multi-layered graph below displaus the number and type of NYC311 complaints that agencies responded to."),
                 plotlyOutput("agency_timeSeriesPlot"),
                 h4("The graph below shows the breakdown of average response time in hours that the agency spent to respond to each type of complaint."),
                 plotlyOutput("agency_responseTimePlot"),
                 hr(),
                 tags$br(),
                 h3(HTML("<strong>Text Analysis Of Most Commonly Words In NYC311 Complaints By Agency</strong>")),
                 tags$br(),
                 h4("The word cloud below displays the most commonly used words within the pool of complaints encountered by each agency. This provides an idea of what specific issues and problems the NYC311 calls pertain to."),
                 plotOutput("agency_wordCloud"),
                 h4("The graph below displays the frequency counts of the 30 most common words within the pool of NYC311 call complaints each agency received. This gives us more information on the nature of concerns that residents in New York have, which each agency oversees and responds to."),
                 tags$br(),
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
      labs(title = "Breakdown Of NYC311 Call Numbers By Community District",
           x = NULL,
           y = NULL) +
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
    
    p <- ggplot(type_summary, aes(y = reorder(Complaint.Category, -Total_Complaints), x = Total_Complaints, fill = Complaint.Category, text = paste("Total Complaints:", Total_Complaints))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Noise-related complaints" = "orange1", 
                                   "Transportation problems" = "dodgerblue",
                                   "Environmental concerns" = "lawngreen",
                                   "Sanitation issues" = "black",
                                   "Housing concerns" = "yellow",
                                   "Safety and security" = "indianred1",
                                   "Others" = "grey"))+
      theme_minimal() +
      labs(title = "Total Complaints by Complaint Category", 
           x = NULL,
           y = NULL) +
      theme(axis.text.x = element_text(hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    ggplotly(p, tooltip = "text")  # Make it interactive with tooltips
  })
  
  # Render interactive plot for total complaints by borough
  output$borough_plot <- renderPlotly({
    borough_summary <- data %>%
      group_by(Borough) %>%
      summarise(Total_Complaints = n()) %>%
      arrange(desc(Total_Complaints))  # Ensure data is sorted in descending order
    
    p <- ggplot(borough_summary, aes(x = reorder(Borough, -Total_Complaints), 
                                     y = Total_Complaints, 
                                     text = paste("Total Complaints:", Total_Complaints))) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Total Complaints by Borough",
           x = NULL,
           y = NULL) +
      theme(axis.text.x = element_text(hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
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
        yaxis = list(title = ""), 
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
      labs(title = title_text, x = "Date", y = NULL) +
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
      top_n(30, n)
    p <- ggplot(words, aes(y = reorder(word, n), x = n)) +
      geom_col() +
      labs(title = sprintf("Top 30 most common words received in complaints by %s", input$selectedAgency), x = "Words", y = "Frequency") +
      theme_minimal() 
    ggplotly(p)
  })
  
  output$agency_responseTimePlot <- renderPlotly({
    req(nrow(agency_filtered_data()) > 0)
    df <- agency_filtered_data() %>%
      group_by(Complaint.Type) %>%
      summarise(AverageResponse = mean(Response.Times, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(AverageResponse))
    title_text <- sprintf(" Average Response Time (Hours) By Complaint Type", input$AgencycomplaintCategory)
    p <- ggplot(df, aes(y = reorder(Complaint.Type, -AverageResponse), x = AverageResponse)) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      labs(title = title_text, y = NULL, x = NULL)
    ggplotly(p)
  })
  
}

########## Run the application

shinyApp(ui = ui, server = server)




