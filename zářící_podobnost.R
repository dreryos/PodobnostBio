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
          fileInput("upload", NULL, buttonLabel = "Nahraj CSV", multiple = FALSE, accept = ".csv",),
          # tags$hr(),
          # h5(helpText("Select the read.table parameters below")),
          # checkboxInput(inputId = "header", label = "Header", value = FALSE),
          # checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
          # br(),
          # radioButtons(inputId = "sep", label = "Separator", choices = c(Comma = ",",Semicolon = ";",Tab = "\t", Space=""), selected = ",")
        ),
        mainPanel(
          tableOutput("head"),

        )
      )
    ),
    tabPanel("Phylostrom",
      plotOutput("phylostrom", width = "100%", height = "500px")
    ),
  )
)

# Define server logic ----
server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
      csv = read.table(file = input$upload$datapath, sep = ",", header = TRUE, stringsAsFactors = TRUE),
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
    data[is.na(data)] <- "NIC"
    return(data)
  })

  # Phylostrom s dist
  output$phylostrom <- renderPlot({
    req(input$upload)

    data <- rem_dup()
    data2 <- data[, -1]
    rownames(data2) <- data[, 1]

    ## Faktory na čísla
    for (i in seq_len(ncol(data2))){
      data2[, i] <- as.numeric(data2[, i])
    }

    # Calculate pairwise distances using Euclidean distance
    distances <- dist(data2)
    pheatmap::pheatmap(distances, display_numbers = TRUE, clustering_method = "ward.D2", labels_row = data$Jméno, labels_col = data$Jméno)
  })

  output$head <- renderTable({
    head(rem_dup(), rownames = TRUE)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
