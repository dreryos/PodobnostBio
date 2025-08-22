# ================================================================================
# BIOSIMILARITY ANALYSIS - OPTIMIZED VERSION
# Autor: [Your Name]
# Datum: 22. srpna 2025
# Popis: Cluster analýza podobnosti účastníků pomocí různých metod
# ================================================================================

# Load required libraries -------------------------------------------------------
# Suppress package startup messages for cleaner console output
suppressPackageStartupMessages({
  library(dplyr)        # Data manipulation and transformation
  library(cluster)      # Clustering algorithms (kmeans, agnes, diana)
  library(pheatmap)     # Enhanced heatmaps with clustering dendrograms
  library(qgraph)       # Network visualization and force-directed graphs
  library(Rtsne)        # t-SNE dimensionality reduction algorithm
})

# Set locale for proper character encoding
Sys.setlocale("LC_ALL", Sys.getenv("LANG"))  # Ensures proper handling of Czech characters

# Configuration parameters -------------------------------------------------------
CLUSTERING_GROUPS <- 3          # Number of clusters to create
RANDOM_SEED <- 42              # Fixed seed for reproducible results
TSNE_MAX_ITER <- 1000          # Maximum iterations for t-SNE algorithm


#' Load and preprocess participant data from CSV file
#'
#' This function reads participant data from a CSV file, cleans it by removing
#' timestamp columns, handles duplicates, and prepares numeric data for analysis.
#' It separates participant names from numeric variables and converts all
#' analysis variables to numeric format.
#'
#' @param csv_path Character string specifying the path to the CSV file.
#'                 Can be relative or absolute path.
#'
#' @return A list containing three elements:
#'         - original_data: The cleaned data with participant names
#'         - numeric_data: Matrix with only numeric variables for analysis
#'         - participant_names: Vector of participant names
#'
#' @examples
#' data_results <- load_and_prepare_data("./data/2025.csv")
#' data_results <- load_and_prepare_data("C:/path/to/data.csv")
#' names(data_results)
load_and_prepare_data <- function(csv_path) {

  cat("Loading and preprocessing data from:", csv_path, "\n")  # Progress indicator with file path
  
  # Check if file exists before attempting to load
  if (!file.exists(csv_path)) {
    stop("Error: File '", csv_path, "' not found. Please check the file path.")
  }
  
  # Load data from CSV file with semicolon separator
  participant_data <- read.csv(csv_path, header = TRUE, stringsAsFactors = TRUE, sep = ";")
  
  # Clean and prepare data using pipeline approach
  cleaned_data <- participant_data |>
    # Remove timestamp column that's not needed for analysis
    select(-matches("Časová.značka")) |>
    # Rename participant name column for consistency across the analysis
    rename(participant_name = matches("Jméno.a.příjmení.")) |>
    # Remove duplicates based on participant name to ensure unique entries
    distinct(participant_name, .keep_all = TRUE)
  
  # Separate numeric data for analysis
  numeric_data <- cleaned_data |>
    # Remove the participant name column to keep only numeric variables
    select(-participant_name) |>
    # Convert all columns to numeric (vectorized operation for efficiency)
    mutate(across(everything(), as.numeric))
  
  # Set row names for clustering algorithms that require named rows
  rownames(numeric_data) <- cleaned_data$participant_name
  
  # Print summary statistics for user feedback
  cat("Data preprocessing completed.\n")
  cat("Participants:", nrow(numeric_data), "\n")
  cat("Variables:", ncol(numeric_data), "\n\n")
  
  # Return structured list with all necessary data components
  return(list(
    original_data = cleaned_data,      # Original cleaned data with names
    numeric_data = numeric_data,       # Numeric matrix for analysis
    participant_names = cleaned_data$participant_name  # Vector of names
  ))
}


#' Calculate Euclidean distance matrix for participant similarity analysis
#'
#' This function computes pairwise Euclidean distances between all participants
#' based on their numeric variable values. The distance matrix is used for
#' clustering algorithms and visualization purposes.
#'
#' @param numeric_data A numeric data frame or matrix where rows represent
#'                     participants and columns represent variables.
#'
#' @return A list containing two elements:
#'         - distances: dist object with pairwise distances
#'         - matrix: symmetric matrix representation of distances
#'
#' @examples
#' distance_results <- calculate_distance_matrix(numeric_data)
#' summary(distance_results$distances)
calculate_distance_matrix <- function(numeric_data) {

  cat("Calculating distance matrix...\n")  # Progress indicator
  
  # Calculate Euclidean distances between all pairs of participants
  euclidean_distances <- dist(numeric_data, method = "euclidean")
  
  # Convert to matrix format for easier manipulation and visualization
  distance_matrix <- as.matrix(euclidean_distances)
  
  # Return both formats as different algorithms prefer different input types
  return(list(
    distances = euclidean_distances,  # dist object for clustering algorithms
    matrix = distance_matrix          # matrix for visualization functions
  ))
}


#' Perform comprehensive clustering analysis using multiple algorithms
#'
#' This function executes both k-means and hierarchical clustering on the
#' participant data. It evaluates different hierarchical clustering methods
#' (average, single, complete, ward) and selects the best one based on
#' agglomerative coefficient. The function also performs AGNES and DIANA
#' clustering for comparison.
#'
#' @param numeric_data A numeric data frame or matrix with participants as rows
#'                     and variables as columns.
#' @param distance_obj A dist object containing pairwise distances between
#'                     participants.
#'
#' @return A list containing clustering results:
#'         - kmeans: K-means clustering result object
#'         - hierarchical: List with AGNES, DIANA, and hclust results
#'         - cluster_assignments: Vector of cluster assignments for each participant
#'         - method_scores: Named vector of agglomerative coefficients for each method
#'
#' @examples
#' clustering_results <- perform_clustering_analysis(numeric_data, distances)
#' table(clustering_results$cluster_assignments)
perform_clustering_analysis <- function(numeric_data, distance_obj) {

  cat("Performing clustering analysis...\n")  # Progress indicator
  
  # Set seed for reproducible k-means results
  set.seed(RANDOM_SEED)
  
  # Perform k-means clustering with multiple random starts for stability
  kmeans_result <- kmeans(numeric_data, centers = CLUSTERING_GROUPS, nstart = 25)
  
  # Define hierarchical clustering methods to evaluate
  clustering_methods <- c("average", "single", "complete", "ward")
  
  # Helper function to evaluate clustering method quality
  evaluate_clustering_method <- function(method) {
    # Calculate agglomerative coefficient (higher is better)
    agnes(numeric_data, method = method)$ac
  }
  
  # Apply evaluation function to all methods (vectorized operation)
  method_scores <- sapply(clustering_methods, evaluate_clustering_method)
  best_method <- names(which.max(method_scores))  # Find best method
  
  # Report best method to user
  cat("Best hierarchical clustering method:", best_method, "with score:", max(method_scores), "\n")
  
  # Perform all three types of hierarchical clustering
  hierarchical_clusters <- list(
    # AGNES: Agglomerative nesting (bottom-up approach)
    agnes_result = agnes(numeric_data, method = "ward"),
    # DIANA: Divisive analysis (top-down approach)  
    diana_result = diana(numeric_data),
    # Standard hierarchical clustering for main analysis
    hclust_result = hclust(distance_obj, method = "ward.D2")
  )
  
  # Extract cluster assignments by cutting the dendrogram
  cluster_assignments <- cutree(hierarchical_clusters$hclust_result, k = CLUSTERING_GROUPS)
  
  # Return all clustering results in a structured list
  return(list(
    kmeans = kmeans_result,              # K-means clustering object
    hierarchical = hierarchical_clusters, # All hierarchical clustering results
    cluster_assignments = cluster_assignments,  # Final cluster assignments
    method_scores = method_scores        # Quality scores for each method
  ))
}


#' Perform t-SNE dimensionality reduction analysis
#'
#' This function applies t-distributed Stochastic Neighbor Embedding (t-SNE)
#' to reduce the high-dimensional participant data to 2D coordinates for
#' visualization. It automatically calculates optimal perplexity based on
#' sample size and combines the results with cluster assignments.
#'
#' @param numeric_data A numeric data frame or matrix with participants as rows
#'                     and variables as columns.
#' @param cluster_assignments A numeric vector indicating cluster membership
#'                            for each participant.
#'
#' @return A list containing:
#'         - coordinates: Data frame with t-SNE coordinates and participant info
#'         - perplexity: The perplexity value used for t-SNE
#'
#' @examples
#' tsne_results <- perform_tsne_analysis(numeric_data, cluster_assignments)
#' plot(tsne_results$coordinates$tsne_x, tsne_results$coordinates$tsne_y)
perform_tsne_analysis <- function(numeric_data, cluster_assignments) {

  cat("Performing t-SNE analysis...\n")  # Progress indicator
  
  # Set seed for reproducible t-SNE results
  set.seed(RANDOM_SEED)
  
  # Calculate optimal perplexity based on sample size
  # Rule of thumb: perplexity should be between 5-50 and less than n_samples/3
  sample_count <- nrow(numeric_data)
  optimal_perplexity <- min(30, floor((sample_count - 1) / 3))
  
  # Inform user about the perplexity choice
  cat("Using perplexity:", optimal_perplexity, "\n")
  
  # Execute t-SNE algorithm with optimized parameters
  tsne_result <- Rtsne(
    numeric_data,
    dims = 2,                        # 2D output for visualization
    perplexity = optimal_perplexity, # Automatically calculated perplexity
    verbose = TRUE,                  # Show progress during computation
    max_iter = TSNE_MAX_ITER,        # Maximum iterations for convergence
    check_duplicates = FALSE         # Skip duplicate checking for efficiency
  )
  
  # Create structured output data frame with t-SNE coordinates and metadata
  tsne_coordinates <- data.frame(
    tsne_x = tsne_result$Y[, 1],              # First t-SNE dimension
    tsne_y = tsne_result$Y[, 2],              # Second t-SNE dimension
    participant_name = rownames(numeric_data), # Original participant names
    cluster_id = cluster_assignments,          # Cluster assignments from hierarchical clustering
    stringsAsFactors = FALSE                   # Prevent automatic factor conversion
  )
  
  # Return structured results for further analysis and visualization
  return(list(
    coordinates = tsne_coordinates,    # Complete coordinate data frame
    perplexity = optimal_perplexity   # Perplexity used for documentation
  ))
}


#' Optimize text positioning to avoid overlapping labels in plots
#'
#' This function calculates optimal text sizes based on local point density
#' to minimize label overlap in scatter plots. Points that are closer together
#' receive smaller text to reduce visual clutter.
#'
#' @param x_coords Numeric vector of x-coordinates for text labels.
#' @param y_coords Numeric vector of y-coordinates for text labels.
#' @param labels Character vector of text labels (used for length validation).
#' @param base_cex Numeric value for base text size. Default is 0.7.
#'
#' @return Numeric vector of adjusted text sizes (cex values) for each label.
#'
#' @examples
#' text_sizes <- optimize_text_positioning(x_coords, y_coords, labels, 0.8)
#' text(x_coords, y_coords, labels, cex = text_sizes)
optimize_text_positioning <- function(x_coords, y_coords, labels, base_cex = 0.7) {

  # Create coordinate matrix for distance calculations
  coordinate_matrix <- cbind(x_coords, y_coords)
  
  # Calculate pairwise distances between all points
  point_distances <- as.matrix(dist(coordinate_matrix))
  diag(point_distances) <- Inf  # Exclude self-distances (set to infinity)
  
  # Find minimum distance to nearest neighbor for each point
  min_neighbor_distances <- apply(point_distances, 1, min)
  
  # Adjust text size based on local point density
  # Formula: smaller distances → smaller text size
  density_adjusted_cex <- pmax(
    0.4,  # Minimum text size to ensure readability
    base_cex * pmin(1, min_neighbor_distances / 0.5)  # Scale factor based on distance
  )
  
  # Return vector of adjusted text sizes
  return(density_adjusted_cex)
}


#' Generate abbreviation legend for participant names
#'
#' This function creates standardized abbreviations for participant names
#' and returns them in a structured data frame sorted by abbreviation.
#' The abbreviations are used consistently across all visualizations.
#'
#' @param participant_names Character vector of full participant names.
#'
#' @return Data frame with two columns:
#'         - full_name: Original participant names
#'         - abbreviation: Generated abbreviations (minimum 3 characters)
#'
#' @examples
#' legend_data <- generate_abbreviation_legend(c("John Doe", "Jane Smith"))
#' print(legend_data)
generate_abbreviation_legend <- function(participant_names) {

  # Create standardized abbreviations with minimum 3 characters
  abbreviations <- abbreviate(participant_names, minlength = 3)
  
  # Create structured data frame with full names and abbreviations
  legend_data <- data.frame(
    full_name = participant_names,     # Original full names
    abbreviation = abbreviations,      # Generated abbreviations
    stringsAsFactors = FALSE           # Prevent automatic factor conversion
  ) |>
    # Sort by abbreviation for easier alphabetical lookup
    arrange(abbreviation)
  
  # Return the complete legend data frame
  return(legend_data)
}


#' Create a formatted abbreviation legend page for PDF output
#'
#' This function generates a professional-looking page displaying the mapping
#' between participant abbreviations and their full names. The layout uses
#' multiple columns for efficient space utilization.
#'
#' @param legend_data Data frame containing full_name and abbreviation columns.
#'
#' @return NULL (function creates plot output directly).
#'
#' @examples
#' legend_data <- generate_abbreviation_legend(participant_names)
#' create_abbreviation_page(legend_data)
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
  
  # Calculate evenly distributed text positions across the page with smaller column spacing
  x_positions <- rep(seq(0.05, 0.75, length.out = n_cols), length.out = n_participants)  # Reduced from 0.95 to 0.75
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


#' Generate comprehensive PDF report with all analysis visualizations
#'
#' This function creates a multi-page PDF report containing dendrogram,
#' distance heatmap, force-directed network graph, t-SNE visualization,
#' and abbreviation legend. It also generates a separate high-resolution
#' heatmap PDF. All files are saved to the ./results directory.
#'
#' @param clustering_results List containing all clustering analysis results
#'                          from perform_clustering_analysis().
#' @param distance_data List containing distance matrix and dist object
#'                     from calculate_distance_matrix().
#' @param tsne_results List containing t-SNE coordinates and parameters
#'                    from perform_tsne_analysis().
#' @param participant_names Character vector of participant names for legend.
#'
#' @return NULL (function creates PDF files in ./results directory as side effect).
#'
#' @examples
#' generate_comprehensive_pdf(clustering_results, distance_data, 
#'                           tsne_results, participant_names)
generate_comprehensive_pdf <- function(clustering_results, distance_data, tsne_results, participant_names) {

  cat("Generating comprehensive PDF report...\n")  # Progress indicator
  
  # Initialize main PDF document with landscape A4 orientation
  pdf('./results/biosimilarity_analysis.pdf', paper = "a4r", width = 11.69, height = 8.26)
  
  # Page 1: Hierarchical clustering dendrogram
  par(mar = c(0, 0, 0, 0) + 0.1)  # Minimal margins for full-page plot
  plot(clustering_results$hierarchical$hclust_result,
       cex = 0.7,      # Text size for participant labels
       main = NULL,    # No main title (maximizes plot space)
       xlab = '',      # No x-axis label
       ylab = NULL,    # No y-axis label
       sub = '',       # No subtitle
       axes = FALSE)   # Hide axes for cleaner appearance
  
  # Page 2: Distance heatmap with hierarchical clustering
  pheatmap(distance_data$matrix,
           display_numbers = TRUE,        # Show distance values in cells
           clustering_method = "ward.D2", # Use Ward's method for clustering
           fontsize = 5,                  # Small font for participant names
           fontsize_number = 3,           # Smaller font for distance numbers
           number_format = "%.1f",        # One decimal place for distances
           legend = FALSE)                # Hide color legend to save space
  
  # Page 3: Force-directed network graph
  # Convert distances to similarities (inverse relationship)
  inverse_distances <- 1 / distance_data$matrix
  qgraph(inverse_distances,
         layout = 'spring',           # Spring-force layout algorithm
         vsize = 3,                   # Node size
         mar = c(1, 1, 1, 1))        # Plot margins
  
  # Page 4: t-SNE visualization with optimized text positioning
  par(mar = c(4, 4, 2, 1))  # Standard margins for labeled plot
  
  # Generate distinct colors for each cluster
  cluster_colors <- rainbow(max(clustering_results$cluster_assignments))
  
  # Create main t-SNE scatter plot
  plot(tsne_results$coordinates$tsne_x,
       tsne_results$coordinates$tsne_y,
       main = "",                         # No title
       xlab = "",                         # No x-axis label
       ylab = "",                         # No y-axis label
       col = cluster_colors[tsne_results$coordinates$cluster_id],  # Color by cluster
       pch = 19,                          # Solid circle markers
       cex = 1.2)                         # Point size
  
  # Calculate optimal text sizes to minimize label overlap
  optimized_text_sizes <- optimize_text_positioning(
    tsne_results$coordinates$tsne_x,      # X coordinates for text
    tsne_results$coordinates$tsne_y,      # Y coordinates for text
    tsne_results$coordinates$participant_name,  # Text labels
    base_cex = 0.6                        # Base text size
  )
  
  # Add participant labels with density-adjusted sizes
  text(tsne_results$coordinates$tsne_x,
       tsne_results$coordinates$tsne_y,
       labels = abbreviate(tsne_results$coordinates$participant_name, 3),  # 3-char abbreviations
       pos = 3,                           # Position above points
       cex = optimized_text_sizes,        # Use calculated sizes
       col = "black")                     # Black text color
  
  # Add cluster legend for color interpretation
  legend("topright",
         legend = paste("Cluster", 1:max(clustering_results$cluster_assignments)),  # Legend labels
         col = cluster_colors,              # Use same colors as plot
         pch = 19,                         # Same point style as plot
         cex = 0.8)                        # Legend text size
  
  # Page 5: Generate and add abbreviation legend page
  legend_data <- generate_abbreviation_legend(participant_names)
  create_abbreviation_page(legend_data)
  
  # Close main PDF document
  dev.off()
  
  # Generate separate high-resolution heatmap PDF for detailed analysis
  pdf('./results/detailed_heatmap.pdf', width = 60, height = 60)  # Large format for high detail
  pheatmap(distance_data$matrix,
           display_numbers = TRUE,          # Show all distance values
           clustering_method = "ward.D2")   # Use same clustering method
  dev.off()
  
  # Confirm successful PDF generation
  cat("PDF reports generated successfully in ./results/ directory.\n")
}


#' Execute the complete biosimilarity analysis pipeline
#'
#' This is the main function that orchestrates the entire analysis workflow.
#' It performs data loading, distance calculation, clustering analysis,
#' t-SNE dimensionality reduction, and generates comprehensive PDF reports.
#' It also exports abbreviation legend as CSV. All output files are saved
#' to the ./results directory.
#'
#' @param data_file_path Character string specifying the path to the input CSV file.
#'                       Can be relative or absolute path. If NULL, defaults to "./2025.csv".
#'
#' @return A list containing all analysis results:
#'         - data: Data loading and preprocessing results
#'         - distances: Distance matrix and calculations
#'         - clustering: All clustering analysis results
#'         - tsne: t-SNE dimensionality reduction results
#'
#' @examples
#' # Using default file
#' results <- run_biosimilarity_analysis()
#' 
#' # Using specific file
#' results <- run_biosimilarity_analysis("./data/my_data.csv")
#' results <- run_biosimilarity_analysis("C:/path/to/data.csv")
#' names(results)
run_biosimilarity_analysis <- function(data_file_path = "./2025.csv") {

  cat("=== BIOSIMILARITY ANALYSIS PIPELINE ===\n\n")  # Pipeline header
  
  # Display the file being analyzed
  cat("Input file:", data_file_path, "\n\n")
  
  # Create results directory if it doesn't exist
  if (!dir.exists("./results")) {
    dir.create("./results", recursive = TRUE)
    cat("Created results directory: ./results\n")
  }
  
  # Step 1: Load and preprocess participant data
  data_results <- load_and_prepare_data(data_file_path)
  
  # Step 2: Calculate pairwise distance matrix
  distance_results <- calculate_distance_matrix(data_results$numeric_data)
  
  # Step 3: Perform comprehensive clustering analysis
  clustering_results <- perform_clustering_analysis(data_results$numeric_data, distance_results$distances)
  
  # Step 4: Reduce dimensionality with t-SNE for visualization
  tsne_results <- perform_tsne_analysis(data_results$numeric_data, clustering_results$cluster_assignments)
  
  # Step 5: Generate comprehensive PDF reports and visualizations
  generate_comprehensive_pdf(clustering_results, distance_results, tsne_results, data_results$participant_names)
  
  # Step 6: Export abbreviation legend as standalone CSV file for reference
  legend_data <- generate_abbreviation_legend(data_results$participant_names)
  write.csv(legend_data, "./results/participant_abbreviations.csv", row.names = FALSE)  # No row numbers
  cat("Abbreviation legend exported to: ./results/participant_abbreviations.csv\n")
  
  # Pipeline completion message
  cat("\n=== ANALYSIS COMPLETED SUCCESSFULLY ===\n")
  
  # Return comprehensive results object for further analysis or inspection
  return(list(
    data = data_results,           # Original and processed data
    distances = distance_results,  # Distance matrices and calculations
    clustering = clustering_results,  # All clustering results and assignments
    tsne = tsne_results              # t-SNE coordinates and parameters
  ))
}


# Execute the complete analysis pipeline -----------------------------------------
# This line runs the entire biosimilarity analysis when the script is sourced
# You can change the file path here or call the function with different arguments
analysis_results <- run_biosimilarity_analysis("./data/2025.csv")
