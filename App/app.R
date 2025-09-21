# Required libraries
library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(caret)
library(writexl)
library(randomForest)
library(prospectr)
library(DT)
library(shinycssloaders)

# Load trained RF model
modelo_rf <- readRDS("model_rf_savitzkyGolay_1st_derivative.rds")

# Example data for download
datos_ejemplo <- read_excel("test.xlsx")

# Define UI
ui <- fluidPage(
  theme = bs_theme(
    version    = 4,
    bootswatch = "minty",
    primary    = "#006400",   # dark green
    secondary  = "#6c757d",   # gray
    base_font  = font_google("Roboto"),
    code_font  = font_google("Fira Code")
  ),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
    tags$style(HTML(
      "
      body { background-color: #f7f9fa; }

      /* Main header */
      .header {
        background: linear-gradient(135deg, #004d00 0%, #006400 100%);
        color: #ffffff;
        padding: 40px;
        text-align: center;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        margin-bottom: 30px;
        border-radius: 0 0 20px 20px;
      }
      .header h1 { margin: 0; font-size: 3.5em; font-weight: 700; letter-spacing: 1px; color: #ffffff !important; }
      .header p  { margin: 10px 0 0; font-size: 1.25em; opacity: 0.8; }

      /* Info panel */
      .info-panel {
        background: #ffffff;
        border-radius: 15px;
        padding: 25px 30px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .info-panel h4 { font-weight: 600; color: #006400; }
      .info-panel p, .info-panel ul { font-size: 1em; color: #333333; line-height: 1.5; }

      /* Buttons and tables */
      .btn-primary {
        background-color: #006400;
        border-color: #004d00;
      }
      .btn-primary:hover {
        background-color: #004d00;
      }
      .shiny-input-container .btn {
        background-color: #218838;
        border-color: #1e7e34;
      }
      .nav-tabs > li > a {
        color: #006400;
        font-weight: 600;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus {
        background-color: #006400;
        color: #ffffff;
      }
      table.dataTable thead {
        background-color: #f1f3f5;
      }
      "
    ))
  ),
  
  # Header
  div(class = 'header',
      h1("AIceite"),
      p("Real-time oil sample analysis with AI")
  ),
  
  # Info section
  div(class = 'info-section',
      div(class = 'info-panel',
          h4(icon('info-circle'), "What is OliDetect AI?"),
          p("A solution designed to identify the origin of oil samples in real time on production lines. 
             It integrates Vis-NIRS technologies with a Random Forest model to provide accurate diagnostics 
             and speed up decision-making in quality control.")
      ),
      div(class = 'info-panel',
          h4(icon('question-circle'), "How to use the application"),
          tags$ul(
            tags$li("Click 'Select...' to upload your .csv or .xlsx file with spectral data."),
            tags$li("Adjust the header, separator, and quote options if needed."),
            tags$li("Press 'Process' to generate the preview and predictions."),
            tags$li("In the 'Preview' tab, check the first uploaded records."),
            tags$li("In the 'Predictions' tab, get the estimated origin of each oil sample."),
            tags$li("Use 'Download example' to test the app with sample data.")
          )
      )
  ),
  
  # Main layout
  fluidRow(
    column(width = 4,
           div(class = 'card-panel',
               h4(icon('file-upload'), " Upload spectrum"),
               fileInput("file1", NULL, buttonLabel = "Select...", placeholder = ".csv, .xlsx", accept = c('.csv','.xlsx')),
               fluidRow(
                 column(6, checkboxInput("header", "Header", TRUE)),
                 column(6, selectInput("sep", "Separator",
                                       choices = c("," = ",", ";" = ";", "Tab" = "\t"),
                                       width = "100%"))
               ),
               fluidRow(
                 column(6, selectInput("quote", "Quotes",
                                       choices = c("None" = "", "Double" = '"', "Single" = "'"),
                                       width = "100%")),
                 column(6, actionButton("submitbutton", "Process", class = 'btn btn-primary w-100'))
               ),
               hr(),
               h5("Sample data"),
               downloadButton("downloadData", "Download example", class = 'btn btn-primary w-100')
           )
    ),
    column(width = 8,
           tabsetPanel(
             type = "tabs",
             tabPanel("Preview",
                      div(class = 'card-panel', withSpinner(DTOutput("dataTable"), type = 6))
             ),
             tabPanel("Predictions",
                      div(class = 'card-panel', withSpinner(DTOutput("predicciones"), type = 6))
             )
           )
    )
  )
)

# Define server
server <- function(input, output, session) {
  datos_cargados <- reactiveVal(FALSE)
  observeEvent(input$submitbutton, { datos_cargados(TRUE) })
  
  datos_subidos <- reactive({
    req(input$file1)
    path <- input$file1$datapath
    ext  <- tools::file_ext(path)
    if (ext %in% c('csv','txt')) {
      read.csv(path, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
    } else {
      read_excel(path, col_names = input$header)
    }
  })
  
  # Preview
  output$dataTable <- renderDT({
    req(datos_cargados())
    datatable(head(datos_subidos(), 6),
              options = list(dom = 't', ordering = FALSE),
              class = 'stripe hover')
  })
  
  # Predictions
  output$predicciones <- renderDT({
    req(datos_cargados())
    df       <- datos_subidos()
    ids      <- if ('ID' %in% colnames(df)) as.character(df$ID) else paste0('Sample_', seq_len(nrow(df)))
    espectro <- df %>% select(where(is.numeric)) %>% select(-any_of(c('GR','ID')))
    drv      <- savitzkyGolay(as.matrix(espectro), m = 1, p = 2, w = 11)
    drv_df   <- as.data.frame(drv)
    colnames(drv_df) <- make.names(colnames(drv_df))
    
    vars <- rownames(modelo_rf$importance)
    falt <- setdiff(vars, colnames(drv_df))
    drv_df[falt] <- 0
    drv_df <- drv_df[, vars, drop = FALSE]
    
    preds <- predict(modelo_rf, newdata = drv_df)
    res   <- data.frame(ID = ids, Group = preds, stringsAsFactors = FALSE)
    
    datatable(res,
              options = list(pageLength = 10, autoWidth = TRUE),
              class = 'compact')
  })
  
  # Download example
  output$downloadData <- downloadHandler(
    filename = function() 'example.xlsx',
    content  = function(file) write_xlsx(datos_ejemplo, file)
  )
}

# Run app
shinyApp(ui = ui, server = server)
