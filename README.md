📈 Análisis de Series Temporales: Gastos en Alimentación en Australia
Este proyecto realiza un análisis detallado de una serie temporal correspondiente al gasto mensual total en cafés, restaurantes y servicios de comida para llevar en Australia desde abril de 1980 hasta abril de 2015.

🔍 Objetivos principales:
Realizar un análisis exploratorio de la serie.

Transformar y diferenciar la serie para lograr estacionariedad.

Identificar modelos SARIMA adecuados para modelar la serie.

Comparar modelos en función de métricas estadísticas (AIC, BIC, RMSE, MAE).

Diagnosticar los residuos para validar los supuestos del modelo.

Generar un pronóstico de 30 meses y evaluar el ajuste del modelo.

🛠 Herramientas y paquetes utilizados:
forecast: para modelado y pronóstico.

astsa, portes: para análisis estadístico avanzado.

Funciones personalizadas para la transformación Box-Cox y pruebas de diagnóstico.

🔧 Fases del análisis:
Exploración inicial: visualización, detección de estacionalidad y heterocedasticidad.

Transformación Box-Cox: se obtiene λ = 0.2.

Diferenciación: regular y estacional para lograr estacionariedad.

Modelado SARIMA: se prueban 7 modelos diferentes.

Comparación de modelos: selección del mejor modelo con base en AIC, BIC y precisión.

Diagnóstico de residuos: pruebas Ljung-Box, no paramétricas y normalidad.

Pronóstico: proyección de la serie a 30 meses con el modelo seleccionado.

📊 Resultado:
El modelo SARIMA seleccionado (Modelo 3) demuestra un ajuste sólido y proporciona una base confiable para pronósticos futuros del consumo en el sector alimenticio australiano.
