library(shiny)
library(devtools)

devtools::load_all(".")

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fb;
        font-family: 'Segoe UI', sans-serif;
        color: #172b4d;
      }
      .app-title {
        font-size: 36px;
        font-weight: 800;
        color: #172b4d;
        margin-top: 20px;
        margin-bottom: 5px;
      }
      .app-subtitle {
        color: #6b778c;
        font-size: 16px;
        margin-bottom: 28px;
      }
      .panel-card {
        background: white;
        border-radius: 16px;
        padding: 26px;
        box-shadow: 0 6px 18px rgba(9,30,66,0.08);
        margin-bottom: 22px;
      }
      .section-title {
        font-size: 22px;
        font-weight: 750;
        color: #172b4d;
        margin-bottom: 18px;
      }
      .btn-primary {
        background-color: #4c78dd;
        border-color: #4c78dd;
        border-radius: 8px;
        font-weight: 700;
        width: 100%;
        margin-top: 15px;
        padding: 10px;
      }
      .btn-primary:hover {
        background-color: #375fc0;
        border-color: #375fc0;
      }
      .note {
        color: #6b778c;
        font-size: 13px;
        margin-top: 15px;
        line-height: 1.4;
      }
      table {
        background: white;
        width: 100%;
      }
      th {
        background-color: #edf2f7;
        color: #172b4d;
      }
      td, th {
        padding: 9px !important;
      }
      .footer {
        text-align: center;
        color: #6b778c;
        font-size: 13px;
        padding: 25px;
      }
    "))
  ),

  fluidRow(
    column(
      12,
      div(class = "app-title", "metricas2x2"),
      div(
        class = "app-subtitle",
        "Herramienta interactiva para calcular métricas de clasificación binaria a partir de una tabla 2x2."
      )
    )
  ),

  fluidRow(

    column(
      4,

      div(
        class = "panel-card",
        div(class = "section-title", "Tabla 2x2"),

        fluidRow(
          column(6, numericInput("tp", "TP", 50, min = 0)),
          column(6, numericInput("fn", "FN", 10, min = 0))
        ),

        fluidRow(
          column(6, numericInput("fp", "FP", 5, min = 0)),
          column(6, numericInput("tn", "TN", 35, min = 0))
        ),

        numericInput(
          "B",
          "Remuestreos bootstrap (B)",
          300,
          min = 50
        ),

        actionButton(
          "calc",
          "Calcular métricas",
          class = "btn-primary"
        )
      ),

      div(
        class = "panel-card",
        div(class = "section-title", "Convención usada"),
        HTML("
          <table class='table table-bordered text-center'>
            <tr>
              <th></th><th>Método +</th><th>Método -</th>
            </tr>
            <tr>
              <th>Estándar +</th><td>TP</td><td>FN</td>
            </tr>
            <tr>
              <th>Estándar -</th><td>FP</td><td>TN</td>
            </tr>
          </table>
        "),
        div(
          class = "note",
          "Las filas representan el estándar de referencia y las columnas el método evaluado."
        )
      )
    ),

    column(
      8,

      div(
        class = "panel-card",
        textOutput("titulo_resultados"),

        div(
          style = "max-height: 720px; overflow-y: auto;",
          h4("Medidas básicas de diagnóstico"),
tableOutput("tabla_basicas"),

h4("Medidas de concordancia sin corregir por azar"),
tableOutput("tabla_sin_azar"),

h4("Medidas de concordancia corregidas por azar"),
tableOutput("tabla_con_azar")
        ),

        div(
          class = "note",
          HTML(
  "Las métricas se agrupan según su interpretación. El error estándar puede obtenerse de forma analítica o mediante bootstrap según la métrica."
)
        )
      )
    )
  ),

  hr(),

  fluidRow(
    column(
      12,
      div(
        class = "footer",
        HTML(
          "Trabajo Fin de Máster · Universidad de Granada<br>
          Evaluación de modelos de clasificación binaria en diagnóstico médico"
        )
      )
    )
  )
)


server <- function(input, output) {

  resultados <- eventReactive(input$calc, {

    tab <- matrix(
      c(
        input$tp,
        input$fn,
        input$fp,
        input$tn
      ),
      nrow = 2,
      byrow = TRUE
    )

    res <- calcular_metricas_tabla(
      tab,
      B = input$B
    )

   clasificacion <- data.frame(
  metrica_original = c(
    "sensibilidad", "especificidad", "fpr", "fnr",
    "ppv", "npv", "accuracy", "error_rate",
    "balanced_accuracy", "youden", "markedness", "f1",
    "lr_pos", "lr_neg", "dor", "mcc", "yule_q", "yule_y",
    "kappa", "scott_pi", "bennett_s", "ac1", "delta"
  ),
  nombre_bonito = c(
    "Sensibilidad",
    "Especificidad",
    "Tasa de falsos positivos",
    "Tasa de falsos negativos",
    "Valor predictivo positivo",
    "Valor predictivo negativo",
    "Accuracy",
    "Error rate",
    "Balanced Accuracy",
    "Youden Index",
    "Markedness",
    "F1-score",
    "Likelihood Ratio +",
    "Likelihood Ratio -",
    "Diagnostic Odds Ratio",
    "Matthews Correlation Coefficient",
    "Yule Q",
    "Yule Y",
    "Cohen's Kappa",
    "Scott's Pi",
    "Bennett's S",
    "AC1 de Gwet",
    "Delta"
  ),
  grupo = c(
    rep("Medidas básicas de diagnóstico", 8),
    rep("Medidas de concordancia sin corregir por azar", 10),
    rep("Medidas de concordancia corregidas por azar", 5)
  ),
  metodo_se = c(
    rep("Analítico", 11),
    rep("Bootstrap", 12)
  ),
  stringsAsFactors = FALSE
)

idx <- match(res$metrica, clasificacion$metrica_original)

res$Grupo <- clasificacion$grupo[idx]
res$Métrica <- clasificacion$nombre_bonito[idx]
res$`Método SE` <- clasificacion$metodo_se[idx]

res$statistic <- sprintf("%.3f", res$statistic)
res$se <- sprintf("%.3f", res$se)

res <- res[, c("Grupo", "Métrica", "statistic", "se", "Método SE")]

colnames(res) <- c(
  "Grupo",
  "Métrica",
  "Estimación",
  "Error estándar",
  "Método SE"
)

    res
  })

  output$titulo_resultados <- renderText({

    req(resultados())

    paste0("Resultados agrupados por tipo de métrica")
  })

output$tabla_basicas <- renderTable({
  subset(resultados(), Grupo == "Medidas básicas de diagnóstico")[, -1]
}, striped = TRUE, bordered = FALSE, spacing = "m")

output$tabla_sin_azar <- renderTable({
  subset(resultados(), Grupo == "Medidas de concordancia sin corregir por azar")[, -1]
}, striped = TRUE, bordered = FALSE, spacing = "m")

output$tabla_con_azar <- renderTable({
  subset(resultados(), Grupo == "Medidas de concordancia corregidas por azar")[, -1]
}, striped = TRUE, bordered = FALSE, spacing = "m")
}


