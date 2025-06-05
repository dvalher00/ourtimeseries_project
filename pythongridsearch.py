import pandas as pd
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.stats.diagnostic import acorr_ljungbox
from itertools import product
import warnings

# Cargar datos
data = pd.read_csv('data9.txt', delim_whitespace=True, header=0)
z = data.iloc[:, 0]

# Aplicar transformación Box-Cox con lambda = 0.2
y = (z**0.2 - 1) / 0.2

# Definir rangos de parámetros para la búsqueda
p_values = [2, 3]
d = 1  # Diferenciación regular fija
q_values = [1, 2, 3]
P_values = [0, 1, 2, 3]
D = 1  # Diferenciación estacional fija
Q_values = [0, 1, 2, 3]
s = 12  # Estacionalidad fija

# Generar todas las combinaciones de parámetros
param_grid = list(product(p_values, q_values, P_values, Q_values))

# Almacenar modelos válidos
valid_models = []
model_count = 0

# Suprimir advertencias de convergencia
warnings.filterwarnings('ignore')

# Realizar la búsqueda en la cuadrícula
for params in param_grid:
    p, q, P, Q = params
    order = (p, d, q)
    seasonal_order = (P, D, Q, s)
    
    try:
        # Ajustar modelo SARIMA
        model = SARIMAX(y, order=order, seasonal_order=seasonal_order, trend='n')
        model_fit = model.fit(disp=0, maxiter=50)
        
        # Obtener residuos y eliminar NaNs
        residuals = model_fit.resid
        residuals = residuals[~np.isnan(residuals)]
        
        # Realizar prueba de Ljung-Box (lags=4)
        lb_test = acorr_ljungbox(residuals, lags=[24], return_df=True)
        p_value = lb_test['lb_pvalue'].iloc[0]
        
        # Verificar si cumple el criterio
        if p_value >= 0.01:
            valid_models.append({
                'order': order,
                'seasonal_order': seasonal_order,
                'p_value': p_value,
                'aic': model_fit.aic
            })
            model_count += 1
            print(f"Modelo {model_count}: {order}{seasonal_order} - p-value = {p_value:.4f}")
            
            # Detenerse cuando se encuentren 4 modelos válidos
            if model_count >= 200:
                break
                
    except Exception as e:
        # Manejar errores de ajuste del modelo
        continue

# Mostrar resultados
print("\nModelos seleccionados:")
for i, model in enumerate(valid_models, 1):
    print(f"Modelo {i}:")
    print(f"  Parámetros: SARIMA{model['order']}{model['seasonal_order']}")
    print(f"  p-value Ljung-Box: {model['p_value']:.4f}")
    print(f"  AIC: {model['aic']:.2f}\n")