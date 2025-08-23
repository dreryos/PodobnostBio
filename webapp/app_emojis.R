# ================================================================================
# BIOSIMILARITY ANALYSIS - SHINY WEB APPLICATION
# Autor: [Your Name]
# Datum: 23. srpna 2025
# Popis: Interaktivní webová aplikace pro cluster analýzu podobnosti účastníků
# ================================================================================

# Load required libraries -------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny)        # Shiny web application framework
  library(shinythemes)  # Bootstrap themes for UI styling
  library(shinyjs)      # JavaScript functionality for Shiny
  library(DT)          # Interactive data tables
  library(dplyr)        # Data manipulation and transformation
  library(cluster)      # Clustering algorithms (kmeans, agnes, diana)
  library(pheatmap)     # Enhanced heatmaps with clustering dendrograms
  library(qgraph)       # Network visualization and force-directed graphs
  library(Rtsne)        # t-SNE dimensionality reduction algorithm
  library(plotly)       # Interactive plotting
})

# Set locale for proper character encoding
Sys.setlocale("LC_ALL", Sys.getenv("LANG"))

# Configuration parameters -------------------------------------------------------
CLUSTERING_GROUPS <- 3          # Number of clusters to create
RANDOM_SEED <- 42              # Fixed seed for reproducible results
TSNE_MAX_ITER <- 1000          # Maximum iterations for t-SNE algorithm

# Data Processing Functions ------------------------------------------------------

#' Load and preprocess participant data from uploaded CSV file
load_and_prepare_data <- function(file_path) {
  # Load data from CSV file with semicolon separator
  participant_data <- read.csv(file_path, header = TRUE, stringsAsFactors = TRUE, sep = ";")
  
  # Debug: Print column names
  cat("Available columns:", paste(colnames(participant_data), collapse = ", "), "\n")
  
  # Find timestamp column
  timestamp_cols <- grep("Časová.*značka|timestamp", colnames(participant_data), ignore.case = TRUE, value = TRUE)
  cat("Found timestamp columns:", paste(timestamp_cols, collapse = ", "), "\n")
  
  # Find participant name column
  name_cols <- grep("Jméno.*příjmení|jmeno.*prijmeni|name", colnames(participant_data), ignore.case = TRUE, value = TRUE)
  cat("Found name columns:", paste(name_cols, collapse = ", "), "\n")
  
  if (length(name_cols) == 0) {
    stop("Sloupec s jmény účastníků nebyl nalezen. Očekávám sloupec obsahující 'Jméno' a 'příjmení'.")
  }
  
  # Clean and prepare data using pipeline approach
  cleaned_data <- participant_data
  
  # Remove timestamp columns if found
  if (length(timestamp_cols) > 0) {
    cleaned_data <- cleaned_data %>% select(-all_of(timestamp_cols))
  }
  
  # Rename participant name column for consistency
  names(cleaned_data)[names(cleaned_data) == name_cols[1]] <- "participant_name"
  
  # Remove duplicates based on participant name
  cleaned_data <- cleaned_data %>%
    distinct(participant_name, .keep_all = TRUE)
  
  # Separate numeric data for analysis
  numeric_data <- cleaned_data %>%
    # Remove the participant name column
    select(-participant_name) %>%
    # Convert all columns to numeric
    mutate(across(everything(), as.numeric))
  
  # Set row names for clustering algorithms
  rownames(numeric_data) <- cleaned_data$participant_name
  
  return(list(
    original_data = cleaned_data,
    numeric_data = numeric_data,
    participant_names = cleaned_data$participant_name
  ))
}

#' Calculate Euclidean distance matrix
calculate_distance_matrix <- function(numeric_data) {
  euclidean_distances <- dist(numeric_data, method = "euclidean")
  distance_matrix <- as.matrix(euclidean_distances)
  
  return(list(
    distances = euclidean_distances,
    matrix = distance_matrix
  ))
}

#' Perform clustering analysis
perform_clustering_analysis <- function(numeric_data, distance_obj) {
  # Set seed for reproducible results
  set.seed(RANDOM_SEED)
  
  # K-means clustering
  kmeans_result <- kmeans(numeric_data, centers = CLUSTERING_GROUPS, nstart = 25)
  
  # Evaluate clustering methods
  clustering_methods <- c("average", "single", "complete", "ward")
  evaluate_clustering_method <- function(method) {
    agnes(numeric_data, method = method)$ac
  }
  
  method_scores <- sapply(clustering_methods, evaluate_clustering_method)
  best_method <- names(which.max(method_scores))
  
  # Hierarchical clustering
  hierarchical_clusters <- list(
    agnes_result = agnes(numeric_data, method = "ward"),
    diana_result = diana(numeric_data),
    hclust_result = hclust(distance_obj, method = "ward.D2")
  )
  
  # Extract cluster assignments
  cluster_assignments <- cutree(hierarchical_clusters$hclust_result, k = CLUSTERING_GROUPS)
  
  return(list(
    kmeans = kmeans_result,
    hierarchical = hierarchical_clusters,
    cluster_assignments = cluster_assignments,
    method_scores = method_scores,
    best_method = best_method
  ))
}

#' Perform t-SNE analysis
perform_tsne_analysis <- function(numeric_data, cluster_assignments) {
  set.seed(RANDOM_SEED)
  
  # Calculate optimal perplexity
  sample_count <- nrow(numeric_data)
  optimal_perplexity <- min(30, floor((sample_count - 1) / 3))
  
  # Execute t-SNE algorithm for 3D visualization
  tsne_result <- Rtsne(
    numeric_data,
    dims = 3,                        # 3D output for 3D visualization
    perplexity = optimal_perplexity,
    verbose = FALSE,
    max_iter = TSNE_MAX_ITER,
    check_duplicates = FALSE
  )
  
  # Create coordinates data frame with 3D coordinates
  tsne_coordinates <- data.frame(
    tsne_x = tsne_result$Y[, 1],
    tsne_y = tsne_result$Y[, 2],
    tsne_z = tsne_result$Y[, 3],     # Add Z coordinate for 3D
    participant_name = rownames(numeric_data),
    cluster_id = cluster_assignments,
    stringsAsFactors = FALSE
  )
  
  return(list(
    coordinates = tsne_coordinates,
    perplexity = optimal_perplexity
  ))
}

#' Generate abbreviation legend
generate_abbreviation_legend <- function(participant_names) {
  abbreviations <- abbreviate(participant_names, minlength = 3)
  
  legend_data <- data.frame(
    full_name = participant_names,
    abbreviation = abbreviations,
    stringsAsFactors = FALSE
  ) %>%
    arrange(abbreviation)
  
  return(legend_data)
}

#' Create formatted abbreviation legend page for PDF output
create_abbreviation_page <- function(legend_data) {
  # Initialize new plot page with blank canvas
  plot.new()
  
  # Set up page margins (bottom, left, top, right)
  par(mar = c(1, 1, 3, 1))
  
  # Add main title with enhanced formatting
  title(main = "Abbreviation Legend for Participant Names", 
        cex.main = 1.5,  # Large title text
        font.main = 2)   # Bold font
  
  # Calculate layout parameters for multi-column display
  n_participants <- nrow(legend_data)
  n_cols <- 3  # Number of columns for optimal space utilization
  n_rows <- ceiling(n_participants / n_cols)  # Calculate required rows
  
  # Calculate evenly distributed text positions across the page
  x_positions <- rep(seq(0.05, 0.75, length.out = n_cols), length.out = n_participants)
  y_positions <- rep(seq(0.9, 0.1, length.out = n_rows), each = n_cols)[1:n_participants]
  
  # Add legend entries in formatted rows and columns
  for (i in 1:n_participants) {
    # Format each legend entry as "ABR = Full Name"
    legend_text <- paste0(legend_data$abbreviation[i], " = ", legend_data$full_name[i])
    
    # Place text at calculated position
    text(x = x_positions[i], 
         y = y_positions[i], 
         labels = legend_text,
         adj = c(0, 0.5),  # Left-aligned, vertically centered
         cex = 0.8,        # Readable text size
         font = 1)        # Normal font weight
  }
  
  # Add informative footer with metadata
  text(x = 0.5, y = 0.05, 
       labels = paste("Total participants:", n_participants, "| Generated:", Sys.Date()),
       adj = c(0.5, 0.5),  # Center-aligned
       cex = 0.7,          # Smaller footer text
       font = 3,           # Italic font style
       col = "gray50")     # Gray color for subtle appearance
}

#' Generate comprehensive PDF report (adapted for Shiny)
generate_main_pdf <- function(clustering_results, distance_data, tsne_results, participant_names, temp_file) {
  # Initialize main PDF document with landscape A4 orientation
  pdf(temp_file, paper = "a4r", width = 11.69, height = 8.26)
  
  # Page 1: Hierarchical clustering dendrogram
  par(mar = c(0, 0, 0, 0) + 0.1)
  plot(clustering_results$hierarchical$hclust_result,
       cex = 0.7,
       main = NULL,
       xlab = '',
       ylab = NULL,
       sub = '',
       axes = FALSE)
  
  # Page 2: Distance heatmap with hierarchical clustering
  pheatmap(distance_data$matrix,
           display_numbers = TRUE,
           clustering_method = "ward.D2",
           fontsize = 5,
           fontsize_number = 3,
           number_format = "%.1f",
           legend = FALSE)
  
  # Page 3: Force-directed network graph
  inverse_distances <- 1 / distance_data$matrix
  qgraph(inverse_distances,
         layout = 'spring',
         vsize = 3,
         mar = c(1, 1, 1, 1))
  
  # Page 4: t-SNE visualization (2D for PDF compatibility)
  par(mar = c(4, 4, 2, 1))
  
  # Generate distinct colors for each cluster
  cluster_colors <- rainbow(max(clustering_results$cluster_assignments))
  
  # Create main t-SNE scatter plot (use only first 2 dimensions)
  plot(tsne_results$coordinates$tsne_x,
       tsne_results$coordinates$tsne_y,
       main = "",
       xlab = "",
       ylab = "",
       col = cluster_colors[tsne_results$coordinates$cluster_id],
       pch = 19,
       cex = 1.2)
  
  # Add participant labels
  text(tsne_results$coordinates$tsne_x,
       tsne_results$coordinates$tsne_y,
       labels = abbreviate(tsne_results$coordinates$participant_name, 3),
       pos = 3,
       cex = 0.9,
       col = "black")
  
  # Add cluster legend
  legend("topright",
         legend = paste("Cluster", 1:max(clustering_results$cluster_assignments)),
         col = cluster_colors,
         pch = 19,
         cex = 0.8)
  
  # Page 5: Abbreviation legend
  legend_data <- generate_abbreviation_legend(participant_names)
  create_abbreviation_page(legend_data)
  
  # Close PDF document
  dev.off()
}

#' Generate detailed heatmap PDF
generate_heatmap_pdf <- function(distance_data, temp_file) {
  # Generate high-resolution heatmap PDF
  pdf(temp_file, width = 60, height = 60)
  pheatmap(distance_data$matrix,
           display_numbers = TRUE,
           clustering_method = "ward.D2")
  dev.off()
}

# User Interface (UI) ------------------------------------------------------------
ui <- fluidPage(
  # Use a clean theme
  theme = shinytheme("flatly"),
  
  # Initialize shinyjs
  useShinyjs(),
  
  # Application title
  titlePanel("🧬 Biosimilarity Analysis - Cluster analýza podobnosti účastníků"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for file upload and controls
    sidebarPanel(
      width = 3,
      
      # File upload input
      fileInput("file", "📁 Nahrát CSV soubor:",
                accept = c(".csv"),
                buttonLabel = "Procházet...",
                placeholder = "Žádný soubor nebyl vybrán"),
      
      # Information about file format
      helpText("📋 Očekávaný formát:",
               tags$ul(
                 tags$li("CSV soubor s oddělovačem ; (středník)"),
                 tags$li("Sloupec 'Jméno a příjmení:' s jmény účastníků"),
                 tags$li("Numerické sloupce pro analýzu"),
                 tags$li("Sloupec 'Časová značka' bude automaticky odstraněn")
               )),
      
      # Configuration options
      h4("⚙️ Nastavení analýzy"),
      
      numericInput("clusters", "Počet clusterů:", 
                   value = CLUSTERING_GROUPS, 
                   min = 2, max = 10),
      
      numericInput("seed", "Random seed (pro reprodukovatelnost):", 
                   value = RANDOM_SEED, 
                   min = 1, max = 1000),
      
      hr(),
      
      # Analysis status
      h4("📊 Stav analýzy"),
      verbatimTextOutput("status")
    ),
    
    # Main panel with tabbed output
    mainPanel(
      width = 9,
      
      # Loading bar and progress indicator
      div(id = "loading_container",
        br(),
        div(
          style = "text-align: center; padding: 50px;",
          h3("📊 Připravte svůj CSV soubor"),
          p("Nahrajte CSV soubor v levém panelu pro spuštění analýzy.", 
            style = "color: #7f8c8d; font-size: 1.1em;"),
          icon("upload", style = "font-size: 3em; color: #3498db;")
        )
      ),
      
      # Progress bar (hidden initially)
      div(id = "progress_container", style = "display: none;",
        br(),
        div(
          style = "text-align: center; padding: 30px;",
          h3("🔄 Zpracovávání dat..."),
          div(class = "progress progress-striped active", style = "width: 80%; margin: 20px auto;",
            div(class = "progress-bar progress-bar-info", 
                style = "width: 100%; animation: progress-bar-stripes 1s linear infinite;")
          ),
          p(id = "progress_text", "Nahrávání a zpracování dat...", 
            style = "color: #2c3e50; font-weight: bold;"),
          tags$style(HTML("
            @keyframes progress-bar-stripes {
              from { background-position: 40px 0; }
              to { background-position: 0 0; }
            }
            .progress {
              height: 25px;
              background-color: #f5f5f5;
              border-radius: 4px;
              box-shadow: inset 0 1px 2px rgba(0,0,0,.1);
            }
            .progress-bar {
              height: 100%;
              background-color: #3498db;
              background-image: linear-gradient(45deg,rgba(255,255,255,.15) 25%,transparent 25%,transparent 50%,rgba(255,255,255,.15) 50%,rgba(255,255,255,.15) 75%,transparent 75%,transparent);
              background-size: 40px 40px;
              border-radius: 4px;
              transition: width .6s ease;
            }
          "))
        )
      ),
      
      div(id = "results_tabs_container", style = "display: none;",
        tabsetPanel(id = "results_tabs",
        # Data overview tab
        tabPanel("📋 Přehled dat", 
          br(),
          h3("Nahrané údaje"),
          DT::dataTableOutput("data_table"),
          br(),
          h3("Souhrn dat"),
          verbatimTextOutput("data_summary")
        ),
        
        # Dendrogram tab
        tabPanel("🌳 Dendrogram", 
          br(),
          h3("Hierarchical clustering - dendrogram"),
          plotOutput("dendrogram", height = "600px"),
          br(),
          h4("Hodnocení metod clusteringu"),
          DT::dataTableOutput("clustering_scores")
        ),
        
        # Heatmap tab
        tabPanel("🔥 Heatmap", 
          br(),
          h3("Matice vzdáleností mezi účastníky"),
          plotOutput("heatmap", height = "700px"),
          br(),
          helpText("💡 Tmavší barvy = menší vzdálenost = větší podobnost")
        ),
        
        # K-means tab
        tabPanel("📊 K-means", 
          br(),
          h3("K-means clustering"),
          plotOutput("kmeans_plot", height = "600px"),
          br(),
          h4("Výsledky K-means"),
          verbatimTextOutput("kmeans_summary")
        ),
        
        # Network graph tab
        tabPanel("🕸️ Síťový graf", 
          br(),
          h3("Force-directed network graph"),
          plotOutput("network_plot", height = "700px"),
          br(),
          helpText("💡 Uzly představují účastníky, hrany sílu podobnosti")
        ),
        
        # t-SNE tab
        tabPanel("🎯 t-SNE", 
          br(),
          h3("t-SNE 3D vizualizace"),
          plotlyOutput("tsne_plot", height = "700px"),
          br(),
          h4("Parametry t-SNE"),
          verbatimTextOutput("tsne_info"),
          br(),
          helpText("💡 Interaktivní 3D vizualizace - můžete rotovat, přibližovat a vzdalovat graf pomocí myši")
        ),
        
        # Legend tab
        tabPanel("📝 Legenda", 
          br(),
          h3("Zkratky jmen účastníků"),
          DT::dataTableOutput("legend_table"),
          br(),
          downloadButton("download_legend", "💾 Stáhnout legendu jako CSV",
                        class = "btn-primary")
        ),
        
        # PDF Export tab
        tabPanel("📄 PDF Export", 
          br(),
          h3("Stažení PDF reportů"),
          p("Vygenerujte a stáhněte kompletní PDF reporty obsahující všechny analýzy a vizualizace."),
          br(),
          fluidRow(
            column(6,
              wellPanel(
                h4("📊 Kompletní analýza"),
                p("PDF obsahuje dendrogram, heatmapu, síťový graf, t-SNE (2D) a legendu zkratek."),
                downloadButton("download_main_pdf", "💾 Stáhnout hlavní PDF",
                              class = "btn-primary btn-lg", style = "width: 100%;"),
                br(), br(),
                p("Formát: A4 na šířku | Stránky: 5", style = "color: #7f8c8d; font-size: 0.9em;")
              )
            ),
            column(6,
              wellPanel(
                h4("🔍 Detailní heatmapa"),
                p("Vysoké rozlišení pro podrobnou analýzu vzdáleností mezi účastníky."),
                downloadButton("download_heatmap_pdf", "💾 Stáhnout heatmap PDF",
                              class = "btn-info btn-lg", style = "width: 100%;"),
                br(), br(),
                p("Formát: Velký formát | Vysoké rozlišení", style = "color: #7f8c8d; font-size: 0.9em;")
              )
            )
          ),
          br(),
          h4("📋 Informace o PDF exportu"),
          helpText(
            "• PDF soubory jsou generovány v reálném čase na základě nahraných dat",
            br(),
            "• Všechny vizualizace jsou optimalizovány pro tisk",
            br(), 
            "• t-SNE v PDF je ve 2D verzi pro lepší kompatibilitu s tiskem",
            br(),
            "• Legenda zkratek je zahrnuta pro snadnou identifikaci účastníků"
          ),
          br(),
          div(
            style = "background-color: #e8f4fd; padding: 15px; border-radius: 5px; border-left: 4px solid #3498db;",
            h5("💡 Tip", style = "margin-top: 0;"),
            p("Pro nejlepší výsledky doporučujeme tisknout na A4 v orientaci na šířku.", 
              style = "margin-bottom: 0;")
          )
        )
      )
    )
  ) # End of tabsetPanel
      ), # End of results_tabs_container div
  
  # Footer
  hr(),
  fluidRow(
    column(12, 
      p("🧬 Biosimilarity Analysis | Vytvořeno pomocí Shiny | © 2025", 
        style = "text-align: center; color: #7f8c8d;")
    )
  )
)

# Server Logic -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive values for storing analysis results
  values <- reactiveValues(
    data_results = NULL,
    distance_results = NULL,
    clustering_results = NULL,
    tsne_results = NULL,
    legend_data = NULL,
    analysis_complete = FALSE
  )
  
  # Update clustering parameters when inputs change
  observe({
    CLUSTERING_GROUPS <<- input$clusters
    RANDOM_SEED <<- input$seed
  })
  
  # File upload and data processing
  observeEvent(input$file, {
    req(input$file)
    
    # Reset analysis state
    values$analysis_complete <- FALSE
    
    # Show loading UI
    hide("loading_container")
    show("progress_container")
    hide("results_tabs_container")
    
    tryCatch({
      # Update progress text
      runjs("document.getElementById('progress_text').innerHTML = '� Nahrávání CSV souboru...';")
      Sys.sleep(0.5)  # Brief pause for UI update
      
      # Debug: Show column names from CSV
      temp_data <- read.csv(input$file$datapath, header = TRUE, sep = ";", nrows = 1)
      cat("CSV column names:", paste(colnames(temp_data), collapse = ", "), "\n")
      
      # Step 1: Load and process data
      runjs("document.getElementById('progress_text').innerHTML = '🔍 Zpracování a čištění dat...';")
      values$data_results <- load_and_prepare_data(input$file$datapath)
      Sys.sleep(0.3)
      
      # Step 2: Calculate distances
      runjs("document.getElementById('progress_text').innerHTML = '📏 Výpočet vzdáleností mezi účastníky...';")
      values$distance_results <- calculate_distance_matrix(values$data_results$numeric_data)
      Sys.sleep(0.3)
      
      # Step 3: Perform clustering
      runjs("document.getElementById('progress_text').innerHTML = '🔗 Clusterová analýza...';")
      values$clustering_results <- perform_clustering_analysis(
        values$data_results$numeric_data, 
        values$distance_results$distances
      )
      Sys.sleep(0.3)
      
      # Step 4: Perform t-SNE analysis
      runjs("document.getElementById('progress_text').innerHTML = '🎯 t-SNE analýza (může chvíli trvat)...';")
      values$tsne_results <- perform_tsne_analysis(
        values$data_results$numeric_data, 
        values$clustering_results$cluster_assignments
      )
      Sys.sleep(0.3)
      
      # Step 5: Generate legend
      runjs("document.getElementById('progress_text').innerHTML = '📝 Generování legendy...';")
      values$legend_data <- generate_abbreviation_legend(values$data_results$participant_names)
      Sys.sleep(0.3)
      
      # Step 6: Finalize
      runjs("document.getElementById('progress_text').innerHTML = '✅ Dokončování analýzy...';")
      Sys.sleep(0.5)
      
      # Mark analysis as complete
      values$analysis_complete <- TRUE
      
      # Show results and hide loading
      hide("progress_container")
      show("results_tabs_container")
      
      # Update status
      output$status <- renderText(paste("✅ Analýza dokončena!",
                                      "\n📊 Účastníků:", nrow(values$data_results$numeric_data),
                                      "\n📋 Proměnných:", ncol(values$data_results$numeric_data),
                                      "\n🎯 Clusterů:", input$clusters))
      
    }, error = function(e) {
      # Show error and reset UI
      hide("progress_container")
      show("loading_container")
      output$status <- renderText(paste("❌ Chyba při zpracování:", e$message))
      values$analysis_complete <- FALSE
    })
  })
  
  # Data table output
  output$data_table <- DT::renderDataTable({
    req(values$analysis_complete, values$data_results)
    DT::datatable(values$data_results$original_data, 
                  options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Data summary
  output$data_summary <- renderText({
    req(values$analysis_complete, values$data_results)
    paste("Celkem účastníků:", nrow(values$data_results$numeric_data),
          "\nCelkem proměnných:", ncol(values$data_results$numeric_data),
          "\nNazvy proměnných:", paste(colnames(values$data_results$numeric_data), collapse = ", "))
  })
  
  # Dendrogram plot
  output$dendrogram <- renderPlot({
    req(values$analysis_complete, values$clustering_results)
    par(mar = c(0, 0, 0, 0) + 0.1)
    plot(values$clustering_results$hierarchical$hclust_result,
         cex = 0.8,
         main = NULL,
         xlab = '',
         ylab = NULL,
         sub = '',
         axes = FALSE)
  })
  
  # Clustering scores table
  output$clustering_scores <- DT::renderDataTable({
    req(values$analysis_complete, values$clustering_results)
    scores_df <- data.frame(
      Metoda = names(values$clustering_results$method_scores),
      `Agglomerative Coefficient` = round(values$clustering_results$method_scores, 3),
      `Nejlepší` = names(values$clustering_results$method_scores) == values$clustering_results$best_method
    )
    DT::datatable(scores_df, options = list(dom = 't'))
  })
  
  # Heatmap plot
  output$heatmap <- renderPlot({
    req(values$analysis_complete, values$distance_results, values$data_results)
    pheatmap(values$distance_results$matrix,
             display_numbers = TRUE,
             clustering_method = "ward.D2",
             labels_row = values$data_results$participant_names,
             labels_col = values$data_results$participant_names,
             fontsize = 10,
             fontsize_number = 6,
             color = colorRampPalette(c("blue", "white", "red"))(100),
             main = "Matice vzdáleností mezi účastníky")
  })
  
  # K-means plot
  output$kmeans_plot <- renderPlot({
    req(values$analysis_complete, values$clustering_results, values$data_results)
    
    # Use first two principal components for visualization
    pca_result <- prcomp(values$data_results$numeric_data, scale. = TRUE)
    plot_data <- data.frame(
      PC1 = pca_result$x[,1],
      PC2 = pca_result$x[,2],
      Cluster = as.factor(values$clustering_results$kmeans$cluster),
      Name = values$data_results$participant_names
    )
    
    # Create plot
    colors <- rainbow(input$clusters)
    plot(plot_data$PC1, plot_data$PC2,
         col = colors[plot_data$Cluster],
         pch = 19, cex = 1.5,
         main = "",                         # No title
         xlab = "",                         # No x-axis label
         ylab = "")

    # Add labels
    text(plot_data$PC1, plot_data$PC2,
         labels = abbreviate(plot_data$Name, 3),
         pos = 3, cex = 0.7)
    
    # Add legend
    legend("topright", 
           legend = paste("Cluster", 1:input$clusters),
           col = colors, pch = 19, cex = 0.8)
  })
  
  # K-means summary
  output$kmeans_summary <- renderText({
    req(values$analysis_complete, values$clustering_results)
    cluster_sizes <- table(values$clustering_results$kmeans$cluster)
    paste("Velikosti clusterů:",
          paste(paste("Cluster", names(cluster_sizes), ":", cluster_sizes), collapse = "\n"),
          "\n\nWithin cluster sum of squares:", round(values$clustering_results$kmeans$tot.withinss, 2),
          "\nBetween cluster sum of squares:", round(values$clustering_results$kmeans$betweenss, 2),
          "\nTotal sum of squares:", round(values$clustering_results$kmeans$totss, 2))
  })
  
  # Network plot
  output$network_plot <- renderPlot({
    req(values$analysis_complete, values$distance_results)
    # Convert distances to similarities
    inverse_distances <- 1 / values$distance_results$matrix
    qgraph(inverse_distances,
           layout = 'spring',
           vsize = 4,
           mar = c(2, 2, 2, 2),
           title = "Síťový graf podobnosti účastníků")
  })
  
  # t-SNE 3D plot
  output$tsne_plot <- renderPlotly({
    req(values$analysis_complete, values$tsne_results, values$clustering_results)
    
    # Generate colors for clusters
    cluster_colors <- rainbow(max(values$clustering_results$cluster_assignments))
    
    # Prepare data for plotly
    plot_data <- values$tsne_results$coordinates
    plot_data$cluster_color <- cluster_colors[plot_data$cluster_id]
    plot_data$cluster_name <- paste("Cluster", plot_data$cluster_id)
    plot_data$abbreviation <- abbreviate(plot_data$participant_name, 3)
    
    # Create 3D scatter plot
    p <- plot_ly(
      data = plot_data,
      x = ~tsne_x,
      y = ~tsne_y, 
      z = ~tsne_z,
      color = ~cluster_name,
      colors = cluster_colors,
      text = ~participant_name,
      hovertemplate = "%{text}<extra></extra>",
      type = "scatter3d",
      mode = "markers+text",
      textposition = "top center",
      textfont = list(size = 10, color = "black"),
      marker = list(
        size = 8,
        symbol = "circle",
        line = list(width = 2, color = "black")
      )
    ) %>%
      add_text(
        text = ~abbreviation,
        textfont = list(size = 8, color = "black"),
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = "t-SNE 3D Vizualizace Clusterů",
          font = list(size = 16)
        ),
        scene = list(
          xaxis = list(title = "t-SNE Dimenze 1"),
          yaxis = list(title = "t-SNE Dimenze 2"),
          zaxis = list(title = "t-SNE Dimenze 3"),
          camera = list(
            eye = list(x = 1.5, y = 1.5, z = 1.5)
          )
        ),
        legend = list(
          orientation = "v",
          x = 1.02,
          y = 1
        ),
        margin = list(l = 0, r = 0, b = 0, t = 40)
      )
    
    return(p)
  })
  
  # t-SNE info
  output$tsne_info <- renderText({
    req(values$analysis_complete, values$tsne_results)
    paste("Použitá perplexita:", values$tsne_results$perplexity,
          "\nMaximální počet iterací:", TSNE_MAX_ITER,
          "\nRandom seed:", input$seed,
          "\nDimenze:", "3D (interaktivní)",
          "\nOvládání: Myš pro rotaci, kolečko pro zoom, pravé tlačítko pro posun")
  })
  
  # Legend table
  output$legend_table <- DT::renderDataTable({
    req(values$analysis_complete, values$legend_data)
    DT::datatable(values$legend_data, 
                  colnames = c("Celé jméno", "Zkratka"),
                  options = list(pageLength = 15, dom = 'tip'))
  })
  
  # Download legend CSV
  output$download_legend <- downloadHandler(
    filename = function() {
      paste("participant_abbreviations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$legend_data, file, row.names = FALSE)
    }
  )
  
  # Download main PDF report
  output$download_main_pdf <- downloadHandler(
    filename = function() {
      paste("biosimilarity_analysis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Require all necessary data
      req(values$clustering_results, values$distance_results, 
          values$tsne_results, values$data_results)
      
      # Generate PDF
      generate_main_pdf(
        values$clustering_results,
        values$distance_results, 
        values$tsne_results,
        values$data_results$participant_names,
        file
      )
    },
    contentType = "application/pdf"
  )
  
  # Download detailed heatmap PDF
  output$download_heatmap_pdf <- downloadHandler(
    filename = function() {
      paste("detailed_heatmap_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Require distance data
      req(values$distance_results)
      
      # Generate heatmap PDF
      generate_heatmap_pdf(values$distance_results, file)
    },
    contentType = "application/pdf"
  )
}

# Run the application ------------------------------------------------------------
shinyApp(ui = ui, server = server)

