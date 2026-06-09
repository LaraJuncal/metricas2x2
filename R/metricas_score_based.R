# =========================================================
# 05_metricas_score_based.R
# Métricas basadas en scores/probabilidades
# =========================================================

#' Comprobar vectores binarios y scores
#'
#' Comprueba que los vectores de valores reales y scores tienen la misma longitud,
#' que no contienen valores perdidos y que la variable real es binaria.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#'
#' @return TRUE de forma invisible si las comprobaciones son correctas.
#'
#' @export
check_binary_vectors <- function(y_true, y_score){
  
  if(length(y_true)!=length(y_score)){
    stop("y_true e y_score deben tener la misma longitud.")
  }
  
  if(any(is.na(y_true)) || any(is.na(y_score))){
    stop("No se permiten valores NA.")
  }
  
  if(!all(y_true %in% c(0,1))){
    stop("y_true debe contener solo 0 y 1.")
  }
  
  invisible(TRUE)
  
}


#' Calcular el Brier score
#'
#' Calcula el Brier score, definido como el error cuadrático medio entre
#' las probabilidades predichas y los valores reales.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' brier_score(y_true, y_score)
#'
#' @export
brier_score <- function(y_true,
                        y_score){
  
  check_binary_vectors(
    y_true,
    y_score
  )
  
  statistic <-
    mean(
      (y_score-y_true)^2
    )
  
  list(
    statistic=statistic,
    se=NA_real_
  )
  
}


#' Construir tabla 2x2 a partir de scores
#'
#' Convierte scores o probabilidades predichas en una tabla 2x2 usando un umbral
#' de clasificación.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#' @param threshold Umbral usado para convertir scores en predicciones binarias.
#'
#' @return Matriz 2x2 con la convención TP, FN, FP, TN.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' confusion_from_scores(y_true, y_score, threshold = 0.5)
#'
#' @export
confusion_from_scores <- function(y_true,
                                  y_score,
                                  threshold=0.5){
  
  check_binary_vectors(
    y_true,
    y_score
  )
  
  y_pred <-
    ifelse(
      y_score>=threshold,
      1,
      0
    )
  
  df_to_2x2(
    real=y_true,
    pred=y_pred,
    positive=1
  )
  
}


#' Obtener puntos de la curva ROC
#'
#' Calcula los puntos de la curva ROC al variar el umbral de clasificación.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#'
#' @return Data frame con umbral, FPR y TPR.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' roc_points(y_true, y_score)
#'
#' @export
roc_points <- function(y_true,
                       y_score){
  
  thresholds <-
    sort(
      unique(y_score),
      decreasing=TRUE
    )
  
  thresholds <-
    c(
      Inf,
      thresholds,
      -Inf
    )
  
  res <-
    data.frame(
      threshold=thresholds,
      fpr=NA_real_,
      tpr=NA_real_
    )
  
  for(i in seq_along(thresholds)){
    
    tab <-
      confusion_from_scores(
        y_true,
        y_score,
        thresholds[i]
      )
    
    res$fpr[i] <-
      indice_fpr(
        tab,
        se_method="none"
      )$statistic
    
    res$tpr[i] <-
      indice_sensibilidad(
        tab,
        se_method="none"
      )$statistic
    
  }
  
  res
  
}


#' Calcular AUC-ROC
#'
#' Calcula el área bajo la curva ROC mediante integración trapezoidal.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' auc_roc(y_true, y_score)
#'
#' @export
auc_roc <- function(y_true,
                    y_score){
  
  pts <-
    roc_points(
      y_true,
      y_score
    )
  
  ord <-
    order(
      pts$fpr
    )
  
  x <-
    pts$fpr[ord]
  
  y <-
    pts$tpr[ord]
  
  statistic <-
    sum(
      diff(x)*
        (
          head(y,-1)+
            tail(y,-1)
        )/2,
      na.rm=TRUE
    )
  
  list(
    statistic=statistic,
    se=NA_real_
  )
  
}


#' Obtener puntos de la curva Precision-Recall
#'
#' Calcula los puntos de la curva precision-recall al variar el umbral
#' de clasificación.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#'
#' @return Data frame con umbral, recall y precision.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' pr_points(y_true, y_score)
#'
#' @export
pr_points <- function(y_true,
                      y_score){
  
  thresholds <-
    sort(
      unique(y_score),
      decreasing=TRUE
    )
  
  thresholds <-
    c(
      Inf,
      thresholds,
      -Inf
    )
  
  res <-
    data.frame(
      threshold=thresholds,
      recall=NA_real_,
      precision=NA_real_
    )
  
  for(i in seq_along(thresholds)){
    
    tab <-
      confusion_from_scores(
        y_true,
        y_score,
        thresholds[i]
      )
    
    res$recall[i] <-
      indice_sensibilidad(
        tab,
        se_method="none"
      )$statistic
    
    res$precision[i] <-
      indice_ppv(
        tab,
        se_method="none"
      )$statistic
    
  }
  
  res
  
}


#' Calcular AUC-PR
#'
#' Calcula el área bajo la curva precision-recall mediante integración trapezoidal.
#'
#' @param y_true Vector con valores reales codificados como 0 y 1.
#' @param y_score Vector con scores o probabilidades predichas.
#'
#' @return Lista con statistic y se.
#'
#' @examples
#' y_true <- c(1, 1, 0, 0)
#' y_score <- c(0.9, 0.7, 0.4, 0.2)
#' auc_pr(y_true, y_score)
#'
#' @export
auc_pr <- function(y_true,
                   y_score){
  
  pts <-
    pr_points(
      y_true,
      y_score
    )
  
  pts <-
    pts[
      complete.cases(
        pts
      ),
    ]
  
  ord <-
    order(
      pts$recall
    )
  
  x <-
    pts$recall[ord]
  
  y <-
    pts$precision[ord]
  
  statistic <-
    sum(
      diff(x)*
        (
          head(y,-1)+
            tail(y,-1)
        )/2,
      na.rm=TRUE
    )
  
  list(
    statistic=statistic,
    se=NA_real_
  )
  
}