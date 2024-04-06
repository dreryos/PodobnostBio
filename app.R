library(shiny)
library(bslib)
library(markdown)

# Define UI ----
ui <- fluidPage(
  titlePanel("Zářicí BioPodobnost"),
  tabsetPanel(
    tabPanel(
      "Nahrání souboru",
      sidebarLayout(
        sidebarPanel(
          includeMarkdown("howto.md"),
          fileInput("upload", NULL, buttonLabel = "Nahraj CSV nebo XLS/XLSX", multiple = FALSE, accept = c(".csv", ".xlsx", ".xls")),
          a(href="https://github.com/dreryos/PodobnostBio/blob/main/sample.csv", "Demonstrační CSV", target = "_blank"),
          includeMarkdown("credits.md")
        ),
        mainPanel(
          tableOutput("head"),

        )
      )
    ),
    tabPanel("Phylostrom",
      plotOutput("phylostrom", height = "1000px"),
    ),
    tabPanel("Dendogram",
    fixedPanel(
        sliderInput("k_centerD", "Počet K klustrů", min = 1, max = 10, value = 5, step = 1),
        right = 10,
        bottom = 10
      ),
      plotOutput("dendogram", height = "800px")
    ),
    tabPanel("K Klustry",
      fixedPanel(
        sliderInput("k_center", "Počet K klustrů", min = 1, max = 10, value = 5, step = 1),
        right = 10,
        bottom = 10
      ),
      plotOutput("kklustry", height = "800px")
    ),
    tabPanel("force-directed graph",
      page_fillable(
        plotOutput("forcegraph", height = "800px")
      )
    ),
    tabPanel("voronoi mosaic",
      page_fillable(
        plotOutput("voronoi.mosaic", height = "800px")
      )
    ),
  )
)

# Define server logic ----
server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
      csv = read.table(file = input$upload$datapath, sep = ",", header = TRUE, stringsAsFactors = FALSE),
      xlsx = openxlsx::read.xlsx(xlsxFile = input$upload$datapath, sheet = 1, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, skipEmptyCols = FALSE),
      validate("Invalid file; Please upload a .csv file")
    )
  })

  # Uprav data
  tidy_data <- reactive({
    data <- data()
    drops <- c("Časová.značka")
    data <- data[, !(names(data) %in% drops)]
    old_names <- c("Jméno.a.příjmení.", "Jméno.a.příjmení:", "Jméno.a.přijmení:")
    names(data)[match(old_names, names(data))] <- "Jméno"
    return(data)
  })

  # odstraň duplicity a NA
  rem_dup <- reactive({
    data <- tidy_data()
    data <- dplyr::distinct(data,Jméno, .keep_all= TRUE)
    data[is.na(data)] <- "NIC!"
    return(data)
  })

  # Tabulka (for debug)
  output$head <- renderTable({
    tidy_data()
  })

  # Faktory na čísla
  fac_data <- reactive({
    data <- rem_dup()
    data2 <- data[, -1]
    rownames(data2) <- data[, 1]

    for (i in seq_len(ncol(data2))){
      data2[, i] <- as.factor(data2[, i])
    }

    for (i in seq_len(ncol(data2))){
      data2[, i] <- as.numeric(data2[, i])
    }
    return(data2)
  })

  # Distances
  distances <- reactive({
    data <- fac_data()

    # Calculate pairwise distances using Euclidean distance
    distances <- dist(data, method = "euclidean")
    return(distances)
  })

  # Phylostrom s dist
  output$phylostrom <- renderPlot({
    req(input$upload)

    data <- fac_data()
    data2 <- rem_dup()
    distances <- distances()
    pheatmap::pheatmap(distances, display_numbers = TRUE, clustering_method = "ward.D2", labels_row = data2$Jméno, labels_col = data2$Jméno, fontsize = 18)
  })

  # Cluster dendogram
  output$dendogram <- renderPlot({
    req(input$upload)

    data <- fac_data()
    d <- distances()
    
    hc5 <- hclust(d, method = "ward.D2",)
    plot(hc5, hang = -1, cex = 1.2)
    rect.hclust(hc5, k = input$k_centerD, border = 2:5)

    # hc5 <- shipunov::Bclust(d, FUN=function(.x)
    #   hclust(dist(.x, method = "euclidean"), method = "ward.D"),iter=1000, mc.cores=1, monitor=TRUE, bootstrap=TRUE, relative=FALSE, hclist=NULL)

    # plot(hc5, hang = -1, cex = 1.2)
  })

  # K klustry
  output$kklustry <- renderPlot({
    req(input$upload)

    data <- fac_data()
    distances <- as.matrix(distances())

    set.seed(123)
    final <- kmeans(data, input$k_center, nstart = 25)
    factoextra::fviz_cluster(final, data = data, labelsize = 18, repel = TRUE)
    # FCPS::kmeansClustering(distances, ClusterNo = 5, RandomNo = 1, maxIt = 2000, PlotIt = TRUE,)
  })

  output$forcegraph <- renderPlot({
    req(input$upload)

    data <- fac_data()
    distances <- as.matrix(distances())

    set.seed(123)
    dist_mi <- 1/distances
    library(qgraph)
    qgraph(dist_mi, layout='spring', vsize=3)
  })

  output$voronoi.mosaic <- renderPlot({
    #TODO: Někdy jde někdy ne!!

    req(input$upload)

    data <- fac_data()
    distances <- as.matrix(distances())

    library(MASS)
    r <- isoMDS(distances)

    set.seed(123)
    library(tripack)
    x <- r$points
    plot(voronoi.mosaic(x[,1], x[,2], duplicate="remove"))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
