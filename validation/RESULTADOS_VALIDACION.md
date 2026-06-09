# Validación del paquete metricas2x2

## Objetivo

Comprobar que las funciones implementadas generan resultados consistentes.

Se realizaron dos procedimientos independientes:

1. Comparación con cálculo manual.
2. Comparación con paquetes de referencia en R.

---

# Validación manual

Tabla utilizada:

|                | Método + | Método - |
|----------------|----------|----------|
| Estándar +     | 50       | 10       |
| Estándar -     | 5        | 35       |

Resultados:

- Sensibilidad ✔
- Especificidad ✔
- FPR ✔
- FNR ✔
- PPV ✔
- NPV ✔
- Accuracy ✔
- Error rate ✔
- Balanced accuracy ✔
- Youden ✔
- Markedness ✔
- F1 ✔
- DOR ✔
- Delta ✔

Todas las diferencias fueron iguales a 0.

---

# Validación con paquetes externos

Paquetes utilizados:

- caret
- psych
- irr

Métricas comparadas:

- Sensibilidad ✔
- Especificidad ✔
- PPV ✔
- NPV ✔
- Accuracy ✔
- Kappa ✔

Todas las diferencias fueron iguales a 0.

---

## Conclusión

Las funciones implementadas reproducen correctamente los valores esperados tanto respecto al cálculo manual como respecto a herramientas externas de referencia.
