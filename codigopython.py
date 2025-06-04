import numpy as np
import pandas as pd
from statsmodels.tsa.statespace.sarimax import SARIMAX

# Leer datos con formato personalizado
with open('data9.txt', 'r') as f:
    lines = f.readlines()

# Procesar líneas manualmente
values = []
for line in lines[1:]:  # Saltar la primera línea ("x")
    parts = line.strip().split()
    if len(parts) >= 2:
        # Tomar el último elemento como valor numérico
        values.append(float(parts[-1]))

# Crear la serie temporal
z = pd.Series(values)
dates = pd.date_range(start='1980-04', periods=len(z), freq='ME')  # 'ME' para fin de mes
z.index = dates

# Ajustar modelo mod7
mod1 = SARIMAX(
    z,
    order=(1, 1, 1),
    seasonal_order=(3, 1, 1, 12),
    enforce_stationarity=False,
    enforce_invertibility=False,
    initialization='approximate_diffuse'
)

mod1_fit = mod1.fit(disp=True)

# Obtener coeficientes
print("\nCoeficientes del modelo mod7:")
print(mod1_fit.params.to_string(float_format="%.4f"))

# Resumen estadístico
print("\nResumen completo del modelo:")
print(mod1_fit.summary())