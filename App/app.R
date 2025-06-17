# Librerías necesarias
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

# Cargar modelo RF entrenado
modelo_rf <- readRDS("model_rf_savitzkyGolay_1st_derivative.rds")

# Datos de ejemplo para descarga
datos_ejemplo <- read_excel("test.xlsx")

# Definir UI
ui <- fluidPage(
  theme = bs_theme(
    version    = 4,
    bootswatch = "minty",
    primary    = "#006400",   # verde oscuro
    secondary  = "#6c757d",   # puede dejarse gris o cambiarse
    base_font  = font_google("Roboto"),
    code_font  = font_google("Fira Code")
  ),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = TRUE),
    tags$style(HTML(
      "
      body { background-color: #f7f9fa; }

      /* Encabezado principal */
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

      /* Panel informativo */
      .info-panel {
        background: #ffffff;
        border-radius: 15px;
        padding: 25px 30px;
        box-shadow: 0 6px 20px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .info-panel h4 { font-weight: 600; color: #006400; }
      .info-panel p, .info-panel ul { font-size: 1em; color: #333333; line-height: 1.5; }

      /* Botones y tablas */
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
  
  # Encabezado
  div(class = 'header',
      h1("AIceite"),
      p("Análisis de muestras de aceite con IA en tiempo real")
  ),
  
  # Sección informativa
  div(class = 'info-section',
      div(class = 'info-panel',
          h4(icon('info-circle'), "¿Qué es OliDetect IA?"),
          p("Solución diseñada para identificar el origen de muestras de aceite en tiempo real en líneas de producción.
             Integra tecnologías Vis-NIRS con un modelo Random Forest para ofrecer diagnósticos precisos
             y acelerar la toma de decisiones en control de calidad.")
      ),
      div(class = 'info-panel',
          h4(icon('question-circle'), "Cómo usar la aplicación"),
          tags$ul(
            tags$li("Haz click en 'Seleccionar...' para cargar tu archivo .csv o .xlsx con datos espectrales."),
            tags$li("Ajusta las opciones de encabezado, separador y comillas si es necesario."),
            tags$li("Presiona 'Procesar' para generar la vista previa y las predicciones."),
            tags$li("En la pestaña 'Vista previa' revisa los primeros registros cargados."),
            tags$li("En la pestaña 'Predicciones' obtén el origen estimado de cada muestra de aceite."),
            tags$li("Usa 'Descargar ejemplo' para probar la app con datos de muestra.")
          )
      )
  ),
  
  # Layout principal
  fluidRow(
    column(width = 4,
           div(class = 'card-panel',
               h4(icon('file-upload'), " Cargar espectro"),
               fileInput("file1", NULL, buttonLabel = "Seleccionar...", placeholder = ".csv, .xlsx", accept = c('.csv','.xlsx')),
               fluidRow(
                 column(6, checkboxInput("header", "Encabezado", TRUE)),
                 column(6, selectInput("sep", "Separador",
                                       choices = c("," = ",", ";" = ";", "Tab" = "\t"),
                                       width = "100%"))
               ),
               fluidRow(
                 column(6, selectInput("quote", "Comillas",
                                       choices = c("Ninguna" = "", "Doble" = '"', "Simple" = "'"),
                                       width = "100%")),
                 column(6, actionButton("submitbutton", "Procesar", class = 'btn btn-primary w-100'))
               ),
               hr(),
               h5("Datos de ejemplo"),
               downloadButton("downloadData", "Descargar ejemplo", class = 'btn btn-primary w-100')
           )
    ),
    column(width = 8,
           tabsetPanel(
             type = "tabs",
             tabPanel("Vista previa",
                      div(class = 'card-panel', withSpinner(DTOutput("dataTable"), type = 6))
             ),
             tabPanel("Predicciones",
                      div(class = 'card-panel', withSpinner(DTOutput("predicciones"), type = 6))
             )
           )
    )
  )
)

# Definir servidor
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
  
  # Vista previa
  output$dataTable <- renderDT({
    req(datos_cargados())
    datatable(head(datos_subidos(), 6),
              options = list(dom = 't', ordering = FALSE),
              class = 'stripe hover')
  })
  
  # Predicciones
  output$predicciones <- renderDT({
    req(datos_cargados())
    df       <- datos_subidos()
    ids      <- if ('ID' %in% colnames(df)) as.character(df$ID) else paste0('Muestra_', seq_len(nrow(df)))
    espectro <- df %>% select(where(is.numeric)) %>% select(-any_of(c('GR','ID')))
    drv      <- savitzkyGolay(as.matrix(espectro), m = 1, p = 2, w = 11)
    drv_df   <- as.data.frame(drv)
    colnames(drv_df) <- make.names(colnames(drv_df))
    
    vars <- rownames(modelo_rf$importance)
    falt <- setdiff(vars, colnames(drv_df))
    drv_df[falt] <- 0
    drv_df <- drv_df[, vars, drop = FALSE]
    
    preds <- predict(modelo_rf, newdata = drv_df)
    res   <- data.frame(ID = ids, Grupo = preds, stringsAsFactors = FALSE)
    
    datatable(res,
              options = list(pageLength = 10, autoWidth = TRUE),
              class = 'compact')
  })
  
  # Descarga de ejemplo
  output$downloadData <- downloadHandler(
    filename = function() 'ejemplo.xlsx',
    content  = function(file) write_xlsx(datos_ejemplo, file)
  )
}

# Ejecutar la app
shinyApp(ui = ui, server = server)

