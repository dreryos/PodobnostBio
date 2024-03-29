library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Zářicí BioPodobnost"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      includeMarkdown("include.md"),
       fileInput("upload", NULL, buttonLabel = "Nahraj CSV", multiple = FALSE, accept = ".csv"),
    ),
    mainPanel = mainPanel(
      tableOutput("head")
    ),
  ))

# Define server logic ----
server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      validate("Invalid file; Please upload a .csv file")
    )
  })

    output$head <- renderTable({
    head(data())
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
