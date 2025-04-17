# Load necessary libraries
library(shiny)
library(dplyr)
library(stringdist)
library(DT)
library(janitor)
library(ggplot2)
library(reshape2)

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
        tags$li("lane: The lane number."),
        tags$li("sample_name: The name of the sample."),
        tags$li("index: The first index (barcode)."),
        tags$li("index2: The second index (barcode), if available.")
      ),
      p("The file should be comma-separated and include a header row."),
      p("Last update to this app: 2024-09-26")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sample Table", 
                 DTOutput("contents")),
        
        tabPanel("Heatmaps",
                 h4("Index Hamming Distance Heatmap"),
                 plotOutput("heatmap_index", height = "500px"),
                 br(),
                 h4("Index2 Hamming Distance Heatmap"),
                 plotOutput("heatmap_index2", height = "500px")
        )
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  # Reactive expression to read the CSV file
  data <- reactive({
    req(input$file)
    
    first_line <- readLines(input$file$datapath, n = 1)
    separator <- ifelse(grepl(";", first_line), ";", ",")
    
    cc <- clean_names(read.csv(input$file$datapath, sep = separator))
    
    if (separator == ";") {
      showNotification("Semikolon (;) identifierat som separator. Filen konverteras till komma-separerat format.", type = "warning")
      write.csv(cc, input$file$datapath, row.names = FALSE)
    }
    
    cc[is.na(cc$lane), "lane"] <- " "
    cc
  })
  
  # Function to compute Hamming distance
  compute_hamming_distance <- function(barcode1, barcode2) {
    if (nchar(barcode1) != nchar(barcode2)) return(Inf)
    sum(charToRaw(barcode1) != charToRaw(barcode2))
  }
  
  check_sample_name <- function(sample_name, index, index2, lane, df) {
    duplicates <- df %>%
      filter(sample_name == !!sample_name, lane == !!lane) %>%
      mutate(index_combo = paste(index, index2, sep = "_"))
    
    if (nrow(duplicates) > 1 && length(unique(duplicates$index_combo)) > 1) {
      return("Duplicate sample names with different indices within the same lane.")
    } else if (nchar(sample_name) > 64) {
      return("Sample name exceeds 64 characters.")
    } else if (!grepl("^[A-Za-z_][A-Za-z0-9_-]*$", sample_name)) {
      return("Sample name does not meet Illumina's requirements.")
    } else {
      return("OK")
    }
  }
  
  check_valid_index <- function(index) {
    if (is.na(index)) return(TRUE)
    return(grepl("^[ACGTN]*$", index))
  }
  
  find_most_similar_barcodes <- function(df) {
    df <- df %>%
      mutate(
        Most_Similar_Sample_index = "",
        Hamming_Distance_index = Inf,
        Most_Similar_Sample_Index2 = NA,
        Hamming_Distance_Index2 = NA,
        Sample_Name_Check = NA,
        Index_Valid = NA,
        Index2_Valid = NA
      )
    
    for (i in 1:nrow(df)) {
      barcode1_index <- as.character(df$index[i])
      barcode1_index2 <- as.character(df$index2[i])
      lane1 <- df$lane[i]
      sample_name1 <- df$sample_name[i]
      
      df$Hamming_Distance_Index2[i] <- 10000
      df$Hamming_Distance_index[i] <- 10000
      
      df$Sample_Name_Check[i] <- check_sample_name(sample_name1, barcode1_index, barcode1_index2, lane1, df)
      df$Index_Valid[i] <- ifelse(check_valid_index(barcode1_index), "Valid", "Invalid")
      df$Index2_Valid[i] <- ifelse(check_valid_index(barcode1_index2), "Valid", "Invalid")
      
      for (j in 1:nrow(df)) {
        if (i != j && df$lane[j] == lane1) {
          barcode2_index <- as.character(df$index[j])
          barcode2_index2 <- as.character(df$index2[j])
          
          if (!is.na(barcode1_index) && !is.na(barcode2_index)) {
            distance_index <- compute_hamming_distance(barcode1_index, barcode2_index)
            if (distance_index < df$Hamming_Distance_index[i]) {
              df$Hamming_Distance_index[i] <- distance_index
              df$Most_Similar_Sample_index[i] <- as.character(df$sample_name[j])
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
  
  generate_heatmap <- function(barcodes) {
    n <- length(barcodes)
    dist_matrix <- matrix(NA, nrow = n, ncol = n)
    
    for (i in 1:n) {
      for (j in 1:n) {
        dist_matrix[i, j] <- compute_hamming_distance(barcodes[i], barcodes[j])
      }
    }
    
    colnames(dist_matrix) <- barcodes
    rownames(dist_matrix) <- barcodes
    
    melted <- melt(dist_matrix)
    names(melted) <- c("Var1", "Var2", "Distance")
    
    ggplot(melted, aes(x = Var1, y = Var2, fill = Distance)) +
      geom_tile() +
      scale_fill_gradientn(
        colors = c("darkred", "red", "orange", "yellow", "green"),
        values = scales::rescale(0:8),
        breaks = 0:8, name = "Distance"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank())
  }
  
  output$contents <- renderDT({
    req(data())
    df <- find_most_similar_barcodes(data())
    datatable(df, options = list(pageLength = 100)) %>%
      formatStyle(
        'Hamming_Distance_index',
        backgroundColor = styleInterval(c(2, 3), c('red', 'yellow', 'white'))
      ) %>%
      formatStyle(
        'Hamming_Distance_Index2',
        backgroundColor = styleEqual(c(NA, 2, 3), c('white', 'red', 'yellow'))
      ) %>%
      formatStyle(
        'Sample_Name_Check',
        backgroundColor = styleEqual(c("OK", "Sample name exceeds 64 characters.", "Sample name does not meet Illumina's requirements."), c('green', 'yellow', 'yellow'))
      ) %>%
      formatStyle(
        'Index_Valid',
        backgroundColor = styleEqual(c("Valid", "Invalid"), c('green', 'red'))
      ) %>%
      formatStyle(
        'Index2_Valid',
        backgroundColor = styleEqual(c("Valid", "Invalid"), c('green', 'red'))
      )
  })
  
  output$heatmap_index <- renderPlot({
    req(data())
    df <- data()
    barcodes <- df$index[!is.na(df$index) & grepl("^[ACGTN]+$", df$index)]
    barcodes <- unique(barcodes)
    if (length(barcodes) >= 2) generate_heatmap(barcodes)
  })
  
  output$heatmap_index2 <- renderPlot({
    req(data())
    df <- data()
    barcodes <- df$index2[!is.na(df$index2) & grepl("^[ACGTN]+$", df$index2)]
    barcodes <- unique(barcodes)
    if (length(barcodes) >= 2) generate_heatmap(barcodes)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Processed_", input$file$name, sep = "")
    },
    content = function(file) {
      write.csv(find_most_similar_barcodes(data()) %>% select(lane, sample_name, project_id, index, index2), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)


