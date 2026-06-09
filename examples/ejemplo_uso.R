# =========================================================
# ejemplo_uso.R
# Ejemplo de uso del paquete / scripts de métricas 2x2
# TFM - Evaluación de modelos de clasificación binaria
# =========================================================

# ---------------------------------------------------------
# 1. Cargar scripts
# ---------------------------------------------------------

source("../R/00_auxiliares.R")
source("../R/01_metricas_basicas.R")
source("../R/02_metricas_derivadas.R")
source("../R/03_metricas_clinicas_y_asociacion.R")
source("../R/04_metricas_acuerdo_corregido_azar.R")
source("../R/05_metricas_score_based.R")
source("../R/06_utilidades_globales.R")


# ---------------------------------------------------------
# 2. Convención utilizada
# ---------------------------------------------------------
#            Pred+   Pred-
# Real+        TP      FN
# Real-        FP      TN


cat("\n=================================================\n")
cat("EJEMPLO 1: MÉTRICAS A PARTIR DE UNA TABLA 2x2\n")
cat("=================================================\n\n")


# ---------------------------------------------------------
# 3. Tabla 2x2 de ejemplo
# ---------------------------------------------------------

tab <- matrix(
  c(50, 10,
    5, 35),
  nrow = 2,
  byrow = TRUE
)

rownames(tab) <- c("Real+", "Real-")
colnames(tab) <- c("Pred+", "Pred-")

cat("Tabla 2x2 utilizada:\n\n")
print(tab)


# ---------------------------------------------------------
# 4. Métricas individuales
# ---------------------------------------------------------

cat("\n\nMétricas individuales:\n\n")

cat("Sensibilidad:\n")
print(indice_sensibilidad(tab))

cat("\nEspecificidad:\n")
print(indice_especificidad(tab))

cat("\nPPV / Precision:\n")
print(indice_ppv(tab))

cat("\nNPV:\n")
print(indice_npv(tab))

cat("\nAccuracy:\n")
print(indice_accuracy(tab))

cat("\nBalanced Accuracy:\n")
print(indice_balanced_accuracy(tab))

cat("\nYouden Index:\n")
print(indice_youden(tab))

cat("\nF1-score:\n")
print(indice_f1(tab, B = 300))

cat("\nMCC:\n")
print(indice_mcc(tab, B = 300))

cat("\nCohen's Kappa:\n")
print(indice_kappa(tab, B = 300))

cat("\nAC1 de Gwet:\n")
print(indice_ac1(tab, B = 300))


# ---------------------------------------------------------
# 5. Cálculo conjunto de métricas
# ---------------------------------------------------------

cat("\n\nResumen conjunto de métricas:\n\n")

res_tab <- calcular_metricas_tabla(
  tab,
  B = 300,
  seed = 123
)

print(res_tab)


# ---------------------------------------------------------
# 6. Ejemplo con scores/probabilidades
# ---------------------------------------------------------

cat("\n=================================================\n")
cat("EJEMPLO 2: MÉTRICAS A PARTIR DE SCORES\n")
cat("=================================================\n\n")

y_true <- c(
  1, 1, 1, 1,
  0, 0, 0, 0,
  1, 0
)

y_score <- c(
  0.90,
  0.80,
  0.70,
  0.40,
  0.60,
  0.30,
  0.20,
  0.10,
  0.95,
  0.55
)

cat("Valores reales:\n")
print(y_true)

cat("\nScores/probabilidades:\n")
print(y_score)


# ---------------------------------------------------------
# 7. Métricas score-based
# ---------------------------------------------------------

cat("\nBrier Score:\n")
print(brier_score(y_true, y_score))

cat("\nAUC-ROC:\n")
print(auc_roc(y_true, y_score))

cat("\nAUC-PR:\n")
print(auc_pr(y_true, y_score))


# ---------------------------------------------------------
# 8. Tabla 2x2 derivada de scores usando umbral 0.5
# ---------------------------------------------------------

tab_scores <- confusion_from_scores(
  y_true,
  y_score,
  threshold = 0.5
)

rownames(tab_scores) <- c("Real+", "Real-")
colnames(tab_scores) <- c("Pred+", "Pred-")

cat("\nTabla 2x2 derivada de scores con umbral 0.5:\n\n")
print(tab_scores)


# ---------------------------------------------------------
# 9. Cálculo conjunto desde scores
# ---------------------------------------------------------

cat("\nResumen conjunto desde scores:\n\n")

res_scores <- calcular_metricas_scores(
  y_true = y_true,
  y_score = y_score,
  threshold = 0.5,
  B = 300,
  seed = 123
)

print(res_scores)


# ---------------------------------------------------------
# 10. Estado actual
# ---------------------------------------------------------

cat("\n=================================================\n")
cat("OBSERVACIONES\n")
cat("=================================================\n\n")

cat("- Las métricas básicas utilizan error estándar analítico por defecto.\n")
cat("- Las métricas más complejas utilizan bootstrap para estimar el error estándar.\n")
cat("- Las métricas basadas en scores se calculan a partir de probabilidades o puntuaciones.\n")
cat("- La métrica Delta queda pendiente de cerrar según la formulación definitiva que se adopte.\n")
cat("- Esta estructura está pensada como base para un paquete de R y una aplicación Shiny.\n")
