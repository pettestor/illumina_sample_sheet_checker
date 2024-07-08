# Load necessary libraries
library(shiny)
library(dplyr)
library(stringdist)
library(DT)
library(janitor)

# Define the UI
ui <- fluidPage(
  titlePanel("Illumina Sample Sheet Checker"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      downloadButton("downloadData", "Download Processed CSV"),
      tags$hr(),
      h4("Instructions:"),
      p("Please ensure your CSV file has the following columns:"),
      tags$ul(
        tags$li("sample_name: The name of the sample."),
        tags$li("index1: The first index (barcode)."),
        tags$li("index2: The second index (barcode), if available."),
        tags$li("lane: The lane number.")
      ),
      p("The file should be comma-separated and include a header row.")
    ),
    
    mainPanel(
      DTOutput("contents")
    )
  )
)

# Define the server
server <- function(input, output) {
  # Reactive expression to read the CSV file
  data <- reactive({
    req(input$file)
    clean_names(read.csv(input$file$datapath))
    
  })
  
  # Function to compute Hamming distance
  compute_hamming_distance <- function(barcode1, barcode2) {
    if (nchar(barcode1) != nchar(barcode2)) return(Inf)
    sum(charToRaw(barcode1) != charToRaw(barcode2))
  }
  
  # Function to check if sample names meet Illumina's requirements
  check_sample_name <- function(sample_name, index1, index2, lane, df) {
    # Check for duplicate sample names within the same lane but different indices
    duplicates <- df %>%
      filter(sample_name == !!sample_name, lane == !!lane) %>%
      mutate(index_combo = paste(index1, index2, sep = "_"))
    
    if (nrow(duplicates) > 1 && length(unique(duplicates$index_combo)) > 1) {
      return("Duplicate sample names with different indices within the same lane.")
    } else if (grepl("^[A-Za-z_][A-Za-z0-9_-]*$", sample_name)) {
      return("OK")
    } else {
      return("Sample name does not meet Illumina's requirements.")
    }
  }
  
  # Function to find the most similar barcodes and their Hamming distances within the same lane
  find_most_similar_barcodes <- function(df) {
    df <- df %>%
      mutate(
        Most_Similar_Sample_Index1 = "",
        Hamming_Distance_Index1 = Inf,
        Most_Similar_Sample_Index2 = NA,
        Hamming_Distance_Index2 = NA,
        Sample_Name_Check = NA
      )
    
    for (i in 1:nrow(df)) {
      barcode1_index1 <- as.character(df$index1[i])
      barcode1_index2 <- as.character(df$index2[i])
      lane1 <- df$lane[i]
      sample_name1 <- df$sample_name[i]
      df$Hamming_Distance_Index2[i] <- 10000
      df$Hamming_Distance_Index1[i] <- 10000
      
      # Update Sample_Name_Check column
      df$Sample_Name_Check[i] <- check_sample_name(sample_name1, barcode1_index1, barcode1_index2, lane1, df)
      
      for (j in 1:nrow(df)) {
        if (i != j && df$lane[j] == lane1) {
          barcode2_index1 <- as.character(df$index1[j])
          barcode2_index2 <- as.character(df$index2[j])
          
          if (!is.na(barcode1_index1) && !is.na(barcode2_index1)) {
            distance_index1 <- compute_hamming_distance(barcode1_index1, barcode2_index1)
            if (distance_index1 < df$Hamming_Distance_Index1[i]) {
              df$Hamming_Distance_Index1[i] <- distance_index1
              df$Most_Similar_Sample_Index1[i] <- as.character(df$sample_name[j])
            }
          }
          
          if (!is.na(barcode1_index2) && !is.na(barcode2_index2)) {
            distance_index2 <- compute_hamming_distance(barcode1_index2, barcode2_index2)
            if (distance_index2 < df$Hamming_Distance_Index2[i]) {
              df$Hamming_Distance_Index2[i] <- distance_index2
              df$Most_Similar_Sample_Index2[i] <- as.character(df$sample_name[j])
            }
          }
        }
      }
    }
    df
  }
  
  # Display the contents of the CSV file with conditional formatting
  output$contents <- renderDT({
    req(data())
    df <- find_most_similar_barcodes(data())
    datatable(df, options = list(pageLength = 100)) %>%
      formatStyle(
        'Hamming_Distance_Index1',
        backgroundColor = styleInterval(c(2, 3), c('red', 'yellow', 'white'))
      ) %>%
      formatStyle(
        'Hamming_Distance_Index2',
        backgroundColor = styleEqual(c(NA, 2, 3), c('white', 'red', 'yellow'))
      ) %>%
      formatStyle(
        'Sample_Name_Check',
        backgroundColor = styleEqual(c("OK", "FAIL", "DUPLICATE"), c('green', 'red', 'red'))
      )
  })
  
  # Allow the user to download the processed CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Processed_", input$file$name, sep = "")
    },
    content = function(file) {
      write.csv(find_most_similar_barcodes(data()), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
