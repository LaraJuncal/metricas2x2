# =========================================================
# validacion_metricas.R
# Validación básica de funciones del paquete metricas2x2
# =========================================================

devtools::load_all()

tab <- matrix(
  c(50, 10,
    5, 35),
  nrow = 2,
  byrow = TRUE
)

# Convención:
#            Método +   Método -
# Estándar +    TP        FN
# Estándar -    FP        TN

TP <- 50
FN <- 10
FP <- 5
TN <- 35
N <- TP + FN + FP + TN

# ---------------------------------------------------------
# Cálculos manuales
# ---------------------------------------------------------

manual_sensibilidad <- TP / (TP + FN)
manual_especificidad <- TN / (TN + FP)
manual_fpr <- FP / (FP + TN)
manual_fnr <- FN / (FN + TP)
manual_ppv <- TP / (TP + FP)
manual_npv <- TN / (TN + FN)
manual_accuracy <- (TP + TN) / N
manual_error_rate <- (FP + FN) / N
manual_balanced_accuracy <- (manual_sensibilidad + manual_especificidad) / 2
manual_youden <- manual_sensibilidad + manual_especificidad - 1
manual_markedness <- manual_ppv + manual_npv - 1
manual_f1 <- (2 * TP) / (2 * TP + FP + FN)
manual_dor <- (TP * TN) / (FP * FN)
manual_delta <- (TP + TN - 2 * sqrt(FN * FP)) / N

# ---------------------------------------------------------
# Resultados del paquete
# ---------------------------------------------------------

paquete_sensibilidad <- indice_sensibilidad(tab, se_method = "none")$statistic
paquete_especificidad <- indice_especificidad(tab, se_method = "none")$statistic
paquete_fpr <- indice_fpr(tab, se_method = "none")$statistic
paquete_fnr <- indice_fnr(tab, se_method = "none")$statistic
paquete_ppv <- indice_ppv(tab, se_method = "none")$statistic
paquete_npv <- indice_npv(tab, se_method = "none")$statistic
paquete_accuracy <- indice_accuracy(tab, se_method = "none")$statistic
paquete_error_rate <- indice_error_rate(tab, se_method = "none")$statistic
paquete_balanced_accuracy <- indice_balanced_accuracy(tab, se_method = "none")$statistic
paquete_youden <- indice_youden(tab, se_method = "none")$statistic
paquete_markedness <- indice_markedness(tab, se_method = "none")$statistic
paquete_f1 <- indice_f1(tab, se_method = "none")$statistic
paquete_dor <- indice_dor(tab, se_method = "none")$statistic
paquete_delta <- indice_delta(tab, se_method = "none")$statistic

# ---------------------------------------------------------
# Tabla comparativa
# ---------------------------------------------------------

validacion <- data.frame(
  metrica = c(
    "Sensibilidad",
    "Especificidad",
    "FPR",
    "FNR",
    "PPV",
    "NPV",
    "Accuracy",
    "Error rate",
    "Balanced accuracy",
    "Youden",
    "Markedness",
    "F1",
    "DOR",
    "Delta"
  ),
  calculo_manual = c(
    manual_sensibilidad,
    manual_especificidad,
    manual_fpr,
    manual_fnr,
    manual_ppv,
    manual_npv,
    manual_accuracy,
    manual_error_rate,
    manual_balanced_accuracy,
    manual_youden,
    manual_markedness,
    manual_f1,
    manual_dor,
    manual_delta
  ),
  paquete = c(
    paquete_sensibilidad,
    paquete_especificidad,
    paquete_fpr,
    paquete_fnr,
    paquete_ppv,
    paquete_npv,
    paquete_accuracy,
    paquete_error_rate,
    paquete_balanced_accuracy,
    paquete_youden,
    paquete_markedness,
    paquete_f1,
    paquete_dor,
    paquete_delta
  )
)

validacion$diferencia <- validacion$paquete - validacion$calculo_manual

print(validacion)

# ---------------------------------------------------------
# Comprobación automática simple
# ---------------------------------------------------------

tolerancia <- 1e-10

if (all(abs(validacion$diferencia) < tolerancia)) {
  message("Validación correcta: las métricas coinciden con el cálculo manual.")
} else {
  warning("Hay diferencias entre el cálculo manual y el paquete.")
}
