library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Zářicí BioPodobnost"),
  tabsetPanel(
    tabPanel(
      "Nahrání souboru",
      sidebarLayout(
        sidebarPanel(
          includeMarkdown("howto.md"),
          fileInput("upload", NULL, buttonLabel = "Nahraj CSV", multiple = FALSE, accept = ".csv"),
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
      plotOutput("dendogram", height = "1000px")
    ),
    tabPanel("K Klustry",
      fixedPanel(
        sliderInput("k_center", "Počet K klustrů", min = 1, max = 10, value = 5, step = 1),
        right = 10,
        bottom = 10
      ),
      plotOutput("kklustry", height = "1000px")
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
      validate("Invalid file; Please upload a .csv file")
    )
  })

  # Uprav data
  tidy_data <- reactive({
    data <- data()
    drops <- c("Časová.značka")
    data <- data[, !(names(data) %in% drops)]
    names(data)[names(data) == "Jméno.a.příjmení."] <- "Jméno"
    return(data)
  })

  # odstraň duplicity a NA
  rem_dup <- reactive({
    data <- tidy_data()
    data <- dplyr::distinct(data,Jméno, .keep_all= TRUE)
    data[is.na(data)] <- "NIC!"
    return(data)
  })

  # Tabulka
  output$head <- renderTable({
    rem_dup()
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
    plot(hc5, cex = 1.2)
  })

  # K klustry
  output$kklustry <- renderPlot({
    req(input$upload)

    data <- fac_data()

    set.seed(123)
    final <- kmeans(data, input$k_center, nstart = 25)
    factoextra::fviz_cluster(final, data = data, labelsize = 18)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
