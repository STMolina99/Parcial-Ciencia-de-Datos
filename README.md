# Parcial-Ciencia-de-Datos
#Inversión Inteligente: Retorno, Riesgo y Flexibilidad (2017-2025)
# Integrante1:  Santiago Molina Torrillate
# Legajo: 909503
# Email: santiagomolina46@gmail.com

# Integrante2:  Matias Corbalan
# Legajo: 894445
# Email: matiascrb@gmail.com


#Introduccion
Este trabajo integra análisis cuantitativo en RStudio con teoría de mercados y estados financieros para construir la cartera óptima según tu perfil a partir de cinco activos seleccionadas. Además de precios históricos, se extraen datos de income statement, balance sheet y cash flow para calcular ratios clave y seguir la evolución financiera de cada activo.

#Objetivos
1. Automatizar la captura y limpieza de datos de precios junto con los tres estados financieros.
2. Calcular:
   -Retornos logarítmicos diarios y betas frente a SPY.
   -Medidas de riesgo (volatilidad, VaR).
   -Ratios financieros (liquidez, rentabilidad, endeudamiento y flujo de caja) a lo largo del tiempo.

3. Visualizar la evolución de los ratios, la correlación entre activo y armar portofolios  segun Markowitz

#Metodologia aplicada
Empleamos web scraping para automatizar la obtención de precios históricos y estados financieros, garantizando datos actualizados y coherentes sin intervención manual. A su vez, los gráficos sintetizan tendencias de riesgo, retornos y la frontera eficiente de Markowitz, facilitando la interpretación y comunicación de resultados incluso a usuarios con conocimientos básicos.


#¿Como sugerimos correr el codigo?
Usted va a encontrar en el repositorio 4 carpetas (raw, input, output y scripts) con los resultados que obtuvimos. Si usted desea obtener en su working directory el mismo resultado debe seguir los siguientes pasos.
Pasos a seguir:
Paso 1. Descargar los 4 scripts
Paso 2. LLevarlos a la carpeta donde desea trabajar
Paso 3. Abralos en Rstudio y defina el working directory en el mismo lugar donde los llevo en el paso anterior
Paso 4. Debe correr los scripts en el orden cronologico que estan numerados. El 01 creara la carpeta /raw, el 02 creara la carpeta /input , el 03 y 04 guardara los archivos en la carpeta /output.
Paso 5. Tenga cuidado cuando corre el 04 ya que al finalizar movemos los scripts sueltos a una carpeta /scripts


