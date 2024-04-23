# Load necessary libraries dynamically
packages <- c(
  "shiny", "shinythemes", "dplyr", "readr",
  "ggplot2", "lubridate", "plotly", "tidytext", "wordcloud", "RColorBrewer"
)

lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x)
    library(x, character.only = TRUE)
  }
})

# Read and preprocess data
data <- read.csv("/Users/nicholaschoong/Downloads/updated_df_17_aprtojun.csv")
data <- data %>%
  mutate(
    Created.Date = as.Date(Created.Date, format = "%Y-%m-%d"),
    Closed.Date = as.Date(Closed.Date, format = "%Y-%m-%d"),
    Response.Times = round(difftime(Closed.Date, Created.Date, units = "mins"))
  )

# Categorize complaint types
data$Complaint.Category <- sapply(data$Complaint.Type, function(complaint_type) {
  if (grepl("Noise|Disorderly", complaint_type)) "Noise-related complaints"
  else if (grepl("Sanitation|Waste|Dirty|Litter|Garbage|Sewer", complaint_type)) "Sanitation issues"
  else if (grepl("Building|HEAT|ELECTRIC|UNSANITARY|Boiler|Cooling|Mold", complaint_type)) "Housing concerns"
  else if (grepl("Taxi|Traffic|Illegal Parking|Bridge|Parking|Bus", complaint_type)) "Transportation problems"
  else if (grepl("Safety|Rodent|Mosquitoes|Drug|Emergency|Illegal", complaint_type)) "Safety and security"
  else if (grepl("Water|Air Quality|Tree|Condition", complaint_type)) "Environmental concerns"
  else "Others"
})

# Shiny UI
ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("Visualizing NYC 311 Noise Complaints"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedAgency", "Select Agency:", choices = unique(data$Agency)),
      selectInput("complaintCategory", "Select Complaint Category:",
                  choices = c("All", "Noise-related complaints", "Safety and security", "Transportation problems", "Environmental concerns", "Housing concerns", "Sanitation issues", "Others")),
      dateRangeInput("dateRange", "Select Date Range:", start = min(data$Created.Date, na.rm = TRUE), end = max(data$Created.Date, na.rm = TRUE))
    ),
    mainPanel(
      h3("Section 1: Total Number of Complaints by Agency"),
      plotlyOutput("timeSeriesPlot"),
      plotlyOutput("responseTimePlot"),
      h3("Section 2: Most Frequent Words Used in Complaints"),
      plotOutput("wordCloud"),
      plotlyOutput("wordFreqPlot")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  filtered_data <- reactive({
    if (input$complaintCategory == "All") {
      data %>%
        filter(Agency == input$selectedAgency, Created.Date >= input$dateRange[1], Created.Date <= input$dateRange[2])
    } else {
      data %>%
        filter(Agency == input$selectedAgency, Complaint.Category == input$complaintCategory, Created.Date >= input$dateRange[1], Created.Date <= input$dateRange[2])
    }
  })
  
  output$timeSeriesPlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    title_text <- sprintf("%s Total Number of Complaints per Month", input$selectedAgency)
    p <- ggplot(filtered_data(), aes(x = Created.Date, fill = Complaint.Type)) +
      geom_histogram(binwidth = 30, position = "identity", alpha = 0.5) +
      labs(title = title_text, x = "Date", y = "Count of Complaints") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$wordCloud <- renderPlot({
    data <- filtered_data()
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
  
  output$wordFreqPlot <- renderPlotly({
    data <- filtered_data()
    req(nrow(data) > 0)
    words <- data %>%
      unnest_tokens(word, Descriptor) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE) %>%
      top_n(50, n)
    p <- ggplot(words, aes(x = reorder(word, n), y = n)) +
      geom_col() +
      labs(title = "%s Top 50 Words received in Complaints", x = "Words", y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })
  
  output$responseTimePlot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    df <- filtered_data() %>%
      group_by(Complaint.Type) %>%
      summarise(AverageResponse = mean(Response.Times, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(AverageResponse))
    title_text <- sprintf(" Average Response Time by Complaint Type", input$complaintCategory)
    p <- ggplot(df, aes(x = reorder(Complaint.Type, -AverageResponse), y = AverageResponse)) +
      geom_bar(stat = "identity", fill = "dodgerblue") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = title_text, x = "Complaint Type", y = "Average Response Time (minutes)")
    ggplotly(p)
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)