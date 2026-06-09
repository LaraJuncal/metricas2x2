# =========================================================
# validacion_paquetes.R
# Comparación frente a paquetes externos
# =========================================================

devtools::load_all()

library(caret)
library(irr)
library(psych)

tab <- matrix(
  c(
    50, 10,
    5, 35
  ),
  nrow = 2,
  byrow = TRUE
)

TP <- 50
FN <- 10
FP <- 5
TN <- 35

# ---------------------------------------------------------
# Datos individuales
# ---------------------------------------------------------

real <- c(
  rep(1, TP),
  rep(1, FN),
  rep(0, FP),
  rep(0, TN)
)

pred <- c(
  rep(1, TP),
  rep(0, FN),
  rep(1, FP),
  rep(0, TN)
)

real <- factor(real)
pred <- factor(pred)

# ---------------------------------------------------------
# CARET
# ---------------------------------------------------------

cm <- confusionMatrix(
  pred,
  real,
  positive = "1"
)

# ---------------------------------------------------------
# Tus funciones
# ---------------------------------------------------------

sens <- indice_sensibilidad(tab)$statistic
esp <- indice_especificidad(tab)$statistic
ppv <- indice_ppv(tab)$statistic
npv <- indice_npv(tab)$statistic
acc <- indice_accuracy(tab)$statistic
kappa <- indice_kappa(tab)$statistic

# ---------------------------------------------------------
# Tabla comparativa
# ---------------------------------------------------------

comparacion <- data.frame(

  metrica = c(
    "Sensibilidad",
    "Especificidad",
    "PPV",
    "NPV",
    "Accuracy",
    "Kappa"
  ),

  paquete_metricas2x2 = c(
    sens,
    esp,
    ppv,
    npv,
    acc,
    kappa
  ),

  referencia = c(
    cm$byClass["Sensitivity"],
    cm$byClass["Specificity"],
    cm$byClass["Pos Pred Value"],
    cm$byClass["Neg Pred Value"],
    cm$overall["Accuracy"],
    cm$overall["Kappa"]
  )

)

comparacion$diferencia <-
  comparacion$paquete_metricas2x2 -
  comparacion$referencia

print(comparacion)

# ---------------------------------------------------------
# Resultado
# ---------------------------------------------------------

tol <- 1e-8

if (
  all(
    abs(
      comparacion$diferencia
    ) < tol
  )
){

  message(
    "Validación con paquetes superada"
  )

}else{

  warning(
    "Hay diferencias"
  )

}
