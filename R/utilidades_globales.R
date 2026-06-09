# =========================================================
# 06_utilidades_globales.R
# Funciones agregadoras para calcular métricas
# =========================================================

#' Calcular métricas a partir de una tabla 2x2
#'
#' Calcula un conjunto de métricas de evaluación para clasificación binaria
#' a partir de una tabla de contingencia 2x2.
#'
#' La tabla debe seguir la convención:
#'
#' \preformatted{
#'            Pred+   Pred-
#' Real+        TP      FN
#' Real-        FP      TN
#' }
#'
#' @param tab Matriz o tabla 2x2 con frecuencias observadas.
#' @param B Número de remuestreos bootstrap para métricas sin error estándar analítico.
#' @param seed Semilla para reproducibilidad del bootstrap.
#' @param incluir_delta Lógico. Indica si se incluye la métrica Delta, actualmente pendiente de formulación definitiva.
#'
#' @return Un data.frame con tres columnas: nombre de la métrica, estimación puntual y error estándar.
#'
#' @examples
#' tab <- matrix(c(50, 10,
#'                 5, 35),
#'               nrow = 2,
#'               byrow = TRUE)
#'
#' calcular_metricas_tabla(tab, B = 100)
#'
#' @export
calcular_metricas_tabla <- function(tab,
                                    B = 1000,
                                    seed = 123,
                                    incluir_delta = FALSE) {

  check_2x2(tab)

  resultados <- list(

    sensibilidad = indice_sensibilidad(
      tab,
      se_method = "analytic"
    ),

    especificidad = indice_especificidad(
      tab,
      se_method = "analytic"
    ),

    fpr = indice_fpr(
      tab,
      se_method = "analytic"
    ),

    fnr = indice_fnr(
      tab,
      se_method = "analytic"
    ),

    ppv = indice_ppv(
      tab,
      se_method = "analytic"
    ),

    npv = indice_npv(
      tab,
      se_method = "analytic"
    ),

    accuracy = indice_accuracy(
      tab,
      se_method = "analytic"
    ),

    error_rate = indice_error_rate(
      tab,
      se_method = "analytic"
    ),

    balanced_accuracy = indice_balanced_accuracy(
      tab,
      se_method = "analytic"
    ),

    youden = indice_youden(
      tab,
      se_method = "analytic"
    ),

    markedness = indice_markedness(
      tab,
      se_method = "analytic"
    ),

    f1 = indice_f1(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    lr_pos = indice_lr_pos(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    lr_neg = indice_lr_neg(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    dor = indice_dor(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    mcc = indice_mcc(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    yule_q = indice_yule_q(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    yule_y = indice_yule_y(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    kappa = indice_kappa(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    scott_pi = indice_scott_pi(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    bennett_s = indice_bennett_s(
      tab,
      se_method = "bootstrap",
      B = B,
      seed = seed
    ),

    ac1 = indice_ac1(
	  tab,
	  se_method = "bootstrap",
	  B = B,
	  seed = seed
	),

	delta = indice_delta(
	  tab,
	  se_method = "bootstrap",
	  B = B,
	  seed = seed
	)
  )


  data.frame(
    metrica = names(resultados),
    statistic = sapply(
      resultados,
      function(x) x$statistic
    ),
    se = sapply(
      resultados,
      function(x) x$se
    ),
    row.names = NULL
  )

}


#' Calcular métricas a partir de scores
#'
#' Calcula métricas basadas en scores, como Brier score, AUC-ROC y AUC-PR.
#' Opcionalmente, también calcula métricas derivadas de una tabla 2x2 aplicando
#' un umbral de clasificación.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#' @param threshold Umbral usado para convertir scores en predicciones binarias.
#' @param B Número de remuestreos bootstrap.
#' @param seed Semilla para reproducibilidad.
#' @param incluir_metricas_tabla Lógico. Si TRUE, calcula también métricas de tabla 2x2.
#'
#' @return Data frame con tipo de métrica, nombre, estimación puntual y error estándar.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' calcular_metricas_scores(y_true, y_score, threshold = 0.5, B = 100)
#'
#' @export
calcular_metricas_scores <- function(y_true,
                                     y_score,
                                     threshold = 0.5,
                                     B = 1000,
                                     seed = 123,
                                     incluir_metricas_tabla = TRUE) {

  check_binary_vectors(
    y_true,
    y_score
  )

  resultados_scores <- list(

    brier_score = brier_score(
      y_true,
      y_score
    ),

    auc_roc = auc_roc(
      y_true,
      y_score
    ),

    auc_pr = auc_pr(
      y_true,
      y_score
    )

  )

  df_scores <- data.frame(
    tipo = "score_based",
    metrica = names(resultados_scores),
    statistic = sapply(
      resultados_scores,
      function(x) x$statistic
    ),
    se = sapply(
      resultados_scores,
      function(x) x$se
    ),
    row.names = NULL
  )

  if (!isTRUE(incluir_metricas_tabla)) {

    return(df_scores)

  }

  tab <- confusion_from_scores(
    y_true,
    y_score,
    threshold = threshold
  )

  df_tab <- calcular_metricas_tabla(
    tab,
    B = B,
    seed = seed,
    incluir_delta = FALSE
  )

  df_tab$tipo <- "threshold_based"

  df_tab <- df_tab[
    ,
    c("tipo", "metrica", "statistic", "se")
  ]

  rbind(
    df_scores,
    df_tab
  )

}
