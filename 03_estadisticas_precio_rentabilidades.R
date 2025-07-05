# Integrantes: Santiago Molina Torrillate
# Legajo: 909503
# Email: santiagomolina46@gmail.com

# 1. Instalo y cargo paquetes necesarios ---------------------------------------
# Definimos un vector con los nombres de los paquetes que usaremos
paquetes <- c("rvest","httr","jsonlite","quantmod","tidyquant","dplyr","lubridate","tidyr","tinytex","purrr","ggplot2","patchwork","forcats","scales","tidyverse","quadprog","ggthemes","viridis","plotly","timetk","PerformanceAnalytics","kableExtra","gridExtra","ggplotify","grid")
# Recorremos cada paquete y, si no está instalado, lo instalamos;
# luego lo cargamos con library().
for (pkg in paquetes) {if (!requireNamespace(pkg, quietly = TRUE)) {install.packages(pkg, dependencies = TRUE)}
  library(pkg, character.only = TRUE)}

# 2. Directorios y carga de CSVs ---------------------------------------------
# Definimos rutas para los datos crudos (raw) y los datos procesados (input)
raw_dir <- "raw"
out_dir <- "input"
# Listamos todos los archivos .csv en la carpeta de entrada
files <- list.files(path = out_dir, pattern = "\\.csv$", full.names = TRUE)
# Para cada archivo, extraemos el nombre sin extensión y leemos el CSV en una variable
for (f in files) {df_name <- tools::file_path_sans_ext(basename(f))
assign(df_name, read.csv(f, stringsAsFactors = FALSE), envir = .GlobalEnv)}


# 3. Calculo retornos log diarios ---------------------------------------------
rets <- precios_activos_wide %>%
  arrange(date) %>% # Ordena por fecha ascendente
  mutate(across(-date, ~ log(.x / lag(.x)))) %>% # Calcula log-return para cada columna (excepto date)
  drop_na() # Elimina filas con NA (primera fila de cada serie)
# Guardamos los nombres de los activos (todas las columnas menos 'date')
assets <- setdiff(colnames(rets), "date")


# 4. Media y covarianza diarias ----------------------------------------------
mu_daily  <- colMeans(rets[assets]) #colMeans promedia cada columna numérica.
cov_daily <- cov(rets[assets]) #cov calcula la covarianza empírica de cada par de columnas.Nos va a dar una matrix de 5x5

# 5. Simulación de portafolios ------------------------------------------------
set.seed(42) # Fijamos semilla para reproducibilidad
N   <- 5000   # Número de portafolios simulados
# replicate genera N vectores con pesos aleatorios y métricas
out <- replicate(N, {
  w   <- runif(length(assets)); w <- w / sum(w) # Pesos aleatorios que suman 1
  er  <- sum(w * mu_daily) * 252   # Retorno esperado anualizado
  vol <- sqrt(t(w) %*% cov_daily %*% w) * sqrt(252) # Volatilidad anualizada
  sr  <- er / vol #Sharpe Ratio simplificado. Existe otro sharpe donde se resta la tasa libre de riesgo. A los fines practicos asumimos 0. Teoricamente no es incorrecto pero es menos utilizado.
  c(w, er, vol, sr) # Vector de resultados
})

# El objeto 'out' es una matriz donde:
# Cada COLUMNA corresponde a un portafolio simulado (N columnas).
# Cada FILA corresponde a un valor: primero 'length(assets)' filas para pesos,
# Luego 3 filas con 'er', 'vol' y 'sr'.
# Queremos un data.frame donde cada FILA sea un portafolio, y las COLUMNAS sean:
# Una columna por peso de cada activo.
# Tres columnas adicionales para las métricas: expectedReturn, volatility, sharpeRatio.


# 6. Data.frame de resultados -------------------------------------------------
df <- as.data.frame(t(out))
colnames(df) <- c(assets, "expectedReturn", "volatility", "sharpeRatio")
# apply(df, 1, ...) aplica una función a cada FILA.
# 'r' es un vector con valores (pesos y métricas) para esa fila.
# as.numeric(r[assets]) extrae los pesos y los convierte a numérico.
# round(..., 3) redondea a 3 decimales.
# paste0(..., collapse = "<br>") genera un string con saltos de línea HTML.
# El resultado se almacena en 'df$text', una columna nueva.
df$text <- apply(df, 1, function(r) {
  pesos <- paste0(assets, ":", round(as.numeric(r[assets]), 3), collapse = "<br>")
  paste0(# Concatenamos las métricas al final del HTML
    pesos,
    "<br>Vol: ",   round(as.numeric(r["volatility"]), 3),
    "<br>Ret: ",   round(as.numeric(r["expectedReturn"]), 3),
    "<br>Sharpe: ",round(as.numeric(r["sharpeRatio"]), 3)
  )
})

# 7. Cartera Mín Vol y Máx Sharpe ---------------------------------------------
# Avanzamos a la identificación de portafolios extremos en el siguiente paso.
# Identificamos las filas con la mínima volatilidad y el mayor Sharpe.
idx_min_vol <- which.min(df$volatility)
idx_max_sr  <- which.max(df$sharpeRatio)

# Extraemos los vectores completos de pesos y métricas para cada caso.
port_min_vol <- df[idx_min_vol, ]
port_max_sr  <- df[idx_max_sr, ]

# 8. BARRAS + TABLA para MÍNIMA VOLATILIDAD -----------------------------------

# 8.1 Preparar datos en formato long
port_min_long <- tibble(
  Activo = assets,
  Peso   = as.numeric(port_min_vol[assets]) * 100
) %>%
  arrange(Peso) %>%
  mutate(Activo = factor(Activo, levels = Activo))

# 8.2 Gráfico de barras horizontales

# aes(x=Peso, y=Activo): Peso en eje x, activo en eje y.
# geom_col(): crea barras con ancho fijo.
# geom_text(): añade etiquetas con el porcentaje cerca del extremo.
# scale_x_continuous(expand=...): ajusta márgenes para que el texto no se corte.
# labs(): título y nombres de ejes; theme_minimal() para estilo limpio.
p_minbar <- ggplot(port_min_long, aes(x = Peso, y = Activo, fill = Activo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Peso,1), "%")),
            hjust = -0.05, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Portfolio Mínima Volatilidad", x = "Peso (%)", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(hjust = 0.5, face = "bold", size = 16))

# 8.3 Tabla de pesos
# tableGrob(): genera un objeto grob de tabla.
# ttheme_minimal(): define estilo minimalista.
# Aplicamos una paleta de colores igual a las barras.
tab_min <- port_min_long %>%
  mutate(`Peso (%)` = paste0(round(Peso,1), "%")) %>%
  select(Activo, `Peso (%)`)
tt <- ttheme_minimal(core = list(fg_params = list(hjust = 0, x = 0.1)))
table_min <- tableGrob(tab_min, rows = NULL, theme = tt)
pal_min <- scales::viridis_pal(option="D")(nrow(port_min_long))
for (i in seq_len(nrow(port_min_long))) {
  table_min$grobs[[i+1]]$gp$fill <- pal_min[i]
}

# 8.4 Pie de métricas
#Concatenamos Sharpe, Retorno y Volatilidad en un string.
caption_min <- paste0(
  "Sharpe: ", round(port_min_vol$sharpeRatio, 2),
  "   Retorno: ", scales::percent(round(port_min_vol$expectedReturn, 3)),
  "   Vol: ", scales::percent(round(port_min_vol$volatility, 3))
)

# 8.5 Combino con patchwork lado a lado

# wrap_elements(full = table_min): embed el grob de tabla.
# plot_layout(widths = c(3,1)): asigna espacio (3/4 para barras, 1/4 para tabla).
# plot_annotation(caption = caption_min): añade el caption.
# theme(plot.caption...): alinea el texto
min_side <- p_minbar + wrap_elements(full = table_min) +
  plot_layout(widths = c(3, 1)) +
  plot_annotation(caption = caption_min) &
  theme(plot.caption = element_text(hjust = 0))
print(min_side)


# 9. BARRAS + TABLA para MÁXIMO SHARPE ----------------------------------------
# El proceso es análogo al de mínima volatilidad, pero usando 'port_max_sr'.
# 9.1 Preparar datos en formato long
port_sr_long <- tibble(
  Activo = assets,
  Peso   = as.numeric(port_max_sr[assets]) * 100
) %>%
  arrange(Peso) %>%
  mutate(Activo = factor(Activo, levels = Activo))

# 9.2 Gráfico de barras horizontales
p_sr <- ggplot(port_sr_long, aes(x = Peso, y = Activo, fill = Activo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(round(Peso,1), "%")),
            hjust = -0.05, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Portfolio Máximo Sharpe", x = "Peso (%)", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title      = element_text(hjust = 0.5, face = "bold", size = 16))

# 9.3 Tabla de pesos
tab_sr <- port_sr_long %>%
  mutate(`Peso (%)` = paste0(round(Peso,1), "%")) %>%
  select(Activo, `Peso (%)`)
table_sr <- tableGrob(tab_sr, rows = NULL, theme = tt)
pal_sr <- scales::viridis_pal(option="D")(nrow(port_sr_long))
for (i in seq_len(nrow(port_sr_long))) {
  table_sr$grobs[[i+1]]$gp$fill <- pal_sr[i]
}

# 9.4 Pie de métricas
caption_sr <- paste0(
  "Sharpe: ", round(port_max_sr$sharpeRatio, 2),
  "   Retorno: ", scales::percent(round(port_max_sr$expectedReturn, 3)),
  "   Vol: ", scales::percent(round(port_max_sr$volatility, 3))
)

# 9.5 Combino con patchwork lado a lado
sr_side <- p_sr + wrap_elements(full = table_sr) +
  plot_layout(widths = c(3, 1)) +
  plot_annotation(caption = caption_sr) &
  theme(plot.caption = element_text(hjust = 0))
print(sr_side)


# 10.Gráfico interactivo con plotly --------------------------------
# Creamos un scatter de volatilidad vs retorno para todos los portafolios
# color según sharpeRatio, shape según tipo
# 'text' contiene los tooltips HTML definidos en df$text
p_int <- ggplot() +
  geom_point(data = df,
             aes(x = volatility, y = expectedReturn,
                 color = sharpeRatio, shape = "Portfolios Aleatorios", text = text),
             alpha = 0.8) +
  geom_point(data = port_min_vol,
             aes(x = volatility, y = expectedReturn,
                 shape = "Mín Volatilidad", text = text),
             color = "red", size = 4) +
  geom_point(data = port_max_sr,
             aes(x = volatility, y = expectedReturn,
                 shape = "Máx Sharpe", text = text),
             color = "blue", size = 4) +
  scale_color_viridis(option = "D", name = "Sharpe Ratio") +
  scale_shape_manual(name = "Tipo de punto",
                     values = c("Portfolios Aleatorios" = 16,
                                "Mín Volatilidad" = 17,
                                "Máx Sharpe"      = 15)) +
  labs(title = "Portfolios Markowitz con Heatmap de Sharpe Ratio",
       x = "Volatilidad", y = "Rentabilidad") +
  theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title          = element_text(hjust = 0.5, face = "bold", size = 16))
# ggplotly convierte el gráfico en interactivo, mostrando 'text' al pasar el cursor.
ggplotly(p_int, tooltip = "text")

# 11.Calculo de Var para 5 y 10 dias --------------------------------

# 11.1 Definir rango de fechas para descargar precios del benchmark
fecha_inicio <- as.Date("2017-01-01")
fecha_fin <- Sys.Date()

# 11.2 Especificar el benchmark y descargar sus precios
benchmark <- c("SPY")
precios_benchmark <- tq_get( #Esta funcion del paquete tidyquant nos permite traer los precios de los activos (precio de cierre) en el rango de tiempo que determinamos.
  benchmark,
  get  = "stock.prices", #Esta informacion se obtiene de yahoo finance
  from = fecha_inicio,
  to   = fecha_fin
)%>%select(symbol, date, adjusted)

precios_benchmark_wide <- precios_benchmark %>%
  pivot_wider(names_from = symbol, values_from = adjusted) #Convertir a formato ancho y chequear datos faltantes
colSums(is.na(precios_benchmark_wide))#Chequeamos que todas las columnas posean datos

# 11.3  El cuantíl z para un VaR al 95 %
# VaR paramétrico asume retornos ~ N(0, σ²). Un VaR95 mide la pérdida que no se excede con un 95% de confianza.
# En una Normal estándar, P(Z < −1.645) = 0.05  ⇒  z = 1.645
z <- abs(qnorm(0.05))

# 11.4 Volatilidades diarias de los portafolios
vol_min_daily <- port_min_vol$volatility / sqrt(252) # Mínima volatilidad anual → diaria. Salen del paso 7
vol_sr_daily  <- port_max_sr$volatility  / sqrt(252) # Máximo Sharpe anual → diario. Salen del paso 7

# 11.5 Retornos logarítmicos diarios de SPY
rets_spy <- precios_benchmark_wide %>%
  arrange(date) %>%
  mutate(log_return = log(SPY / lag(SPY))) %>%
  drop_na()

sd_spy_daily <- sd(rets_spy$log_return)

# 11.6 Fórmulas paramétricas de VaR (%)
# VaR 1 día  = σ_daily × z × 100
# VaR 10 días = σ_daily × √10 × z × 100
VaR1d_sr   <- vol_sr_daily  * z * 100
VaR10d_sr  <- vol_sr_daily  * sqrt(10) * z * 100

VaR1d_min  <- vol_min_daily * z * 100
VaR10d_min <- vol_min_daily * sqrt(10) * z * 100

VaR1d_spy  <- sd_spy_daily  * z * 100
VaR10d_spy <- sd_spy_daily  * sqrt(10) * z * 100

# 11.7 Armado de la tabla de resultados con tibble
library(tibble)
var_table <- tibble(
  Portfolio    = c("Sharpe Óptimo", "Mínima Volatilidad", "SPY"),
  `VaR 1 día (%)`   = round(c(VaR1d_sr,  VaR1d_min,  VaR1d_spy),   2),
  `VaR 10 días (%)` = round(c(VaR10d_sr, VaR10d_min, VaR10d_spy),  2)
)
print(var_table)

# 11.8 Crear un grob de tabla y convertirlo a ggplot para guardar o visualizar
tabla_grob <- tableGrob(
  var_table,
  rows = NULL,
  theme = ttheme_minimal(
    core = list(fg_params = list(hjust = 0.5)),  # Centrar el texto de celdas
    colhead = list(fg_params = list(fontface = "bold", hjust = 0.5)) # Encabezados en negrita
  )
)
# Convertimos a ggplot
tabla_gg <- as.ggplot(~ grid.draw(tabla_grob))
print(tabla_gg)  # Mostrar la tabla como gráfico

# 12 rentabilidad vs inflación --------------------------------

# Agrupamos por año y calculamos retorno efectivo anual para cada símbolo
rent_yearly <- rets %>%
  mutate(year = year(date)) %>%  # Extrae el año de la fecha
  pivot_longer(
    cols      = -c(date, year), # Toma todas las columnas excepto 'date' y 'year'
    names_to  = "symbol",  # Nueva columna con nombre del activo
    values_to = "logret"   # Nueva columna con el valor del log-retorno
  ) %>%
  group_by(symbol, year) %>%    # Agrupa por activo y año
  summarise(
    ret_year = exp(sum(logret, na.rm = TRUE)) - 1, # Convierte suma de log-retornos en retorno anual efectivo
    .groups   = "drop"
  )


rent_yearly <- rent_yearly %>%
  arrange(symbol, year) %>% # Asegura orden por activo y año
  group_by(symbol) %>% # Agrupa por activo
  mutate(
    index100 = 100 * cumprod(1 + ret_year)  # Índice acumulado: 100 * producto(1 + ret_year)
  ) %>%
  ungroup()

# 12.1. Definimos un tibble con inflación anual y su índice acumulado
inflation_df <- tibble::tibble(
  year      = 2017:2025,
  infl_rate = c(-0.021, -0.024, -0.018, -0.012, -0.047, -0.080, -0.041, -0.029, -0.026) # Tasas de inflación reales
) %>%
  arrange(year) %>%
  mutate(
    index100   = 100 * cumprod(1 + infl_rate), # Índice de inflación acumulada, base 100
    symbol      = "Inflación USD"
  ) %>%
  select(symbol, year, index100)

# 12.2. Unimos los datos de activos e inflación para la gráfica
plot_df <- rent_yearly %>%
  select(symbol, year, index100) %>%
  bind_rows(inflation_df)

# 12.3. Gráfico de líneas: evolución de $100 en activos vs inflación
ggplot(plot_df, aes(x = year, y = index100, color = symbol)) +
  geom_line(size = 1.2) + # Dibuja líneas con grosor 1.2
  scale_color_manual(
    values = c(
      "AAPL" = "#1b9e77", "KO" = "#d95f02", "PFE" = "#7570b3",
      "TM"   = "#e7298a", "MELI" = "#66a61e", "Inflación USD" = "black"
    ),
    name = ""
  ) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") + # Línea horizontal en 100 para referencia
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "USD"),
    limits = c(0, max(plot_df$index100) * 1.05), # Límite superior al 105% del máximo
    breaks = scales::pretty_breaks(n = 8) # Ocho marcas en el eje
  ) +
  scale_x_continuous(breaks = unique(plot_df$year)) + # Marcas en cada año
  labs(
    title    = "Inversion de 100 USD en Activos vs Inflación USD", # Título principal
    subtitle = "Comparativa desde 2017 hasta la actualidad", # Subtítulo explicativo
    x        = "Año",
    y        = "Valor Acumulado"
  ) +
  theme_minimal(base_size = 15) +  # Estilo limpio con base de tamaño 15
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5), # Centrado y negrita en título
    plot.subtitle    = element_text(hjust = 0.5), # Centrado en subtítulo
    axis.text.x      = element_text(angle = 45, hjust = 1), # Rota etiquetas de eje X
    panel.grid.minor = element_blank() # Oculta líneas de cuadrícula menores
  )


# 13 Matriz de correlaciones y betas --------------------------------
# 13.1. Preparamos los log-retornos de los activos (ya los calculaste en pasos anteriores)
log_retornos_activos <- rets %>%
  select(-date) 

# 13.2. Calculamos la matriz de correlaciones entre esos log-retornos
matriz_correlacion <- cor(log_retornos_activos, use = "pairwise.complete.obs") # Usa pares completos para cada correlación

# 13.3. Llevamos la matriz a formato “long” para ggplot (una fila por par de activos)
matriz_correlacion_larga <- matriz_correlacion %>%
  as.data.frame() %>% # Convierte la matriz a data.frame
  tibble::rownames_to_column("Var1") %>% # Pasa los nombres de filas a columna "Var1"
  pivot_longer(cols      = -Var1, # Todas menos "Var1"
               names_to  = "Var2", # Nombre de la segunda variable
               values_to = "Correlation")    # Valores de correlación

# 13.4. Aseguramos el orden correcto de factores para el eje X/Y
niveles <- colnames(log_retornos_activos)  # Capturamos el orden original de los activos
matriz_correlacion_larga <- matriz_correlacion_larga %>%
  mutate(
    Var1 = factor(Var1, levels = niveles),# Forzamos Factor con orden de 'niveles'
    Var2 = factor(Var2, levels = niveles)
  )

# 13.5. Dibujamos el mapa de calor de correlaciones
mapa_correlaciones <- ggplot(matriz_correlacion_larga,
                             aes(x = Var1, y = Var2, fill = Correlation)) +
  # Pintamos cada tile con borde negro
  geom_tile(color = "black", size = 0.4) + # Cada celda con borde negro
  scale_fill_gradientn(
    colours = c("#00008B", "#ffffff", "#ca0020"),  # azul intenso → blanco → rojog
    values  = c(0, 0.3, 1),
    limits  = c(0, 1),
    name    = "Correlación"
  ) +
  scale_x_discrete(position = "top") + # Eje X en la parte superior
  scale_y_discrete(limits = rev(levels(matriz_correlacion_larga$Var1))) + # Invertimos eje Y
  coord_fixed() +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid  = element_blank(),
    plot.title  = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Matriz de Correlaciones del Portfolio",
    x     = NULL,
    y     = NULL
  )

print(mapa_correlaciones)


# --- Calcular los Betas vs SPY ----------------------------------------------

# 13.6 Preparo los log-retornos diarios de SPY
spy_ret <- rets_spy %>%
  select(date, spy_logret = log_return) %>%         # renombro columna de retorno
  mutate(date = as.Date(date))                      # por si acaso, convierto a Date

# 13.7 Uno ambos data.frames por fecha: quedan solo fechas con datos en ambos
rets<-rets%>%mutate(date = as.Date(date))
merged_betas <- inner_join(rets, spy_ret, by = "date")

# 13.8 Calculo la varianza diaria de SPY (denominador en la fórmula de Beta)
var_spy_daily <- var(merged_betas$spy_logret, na.rm = TRUE)

# 13.9 Para cada activo: Beta = cov(ret_activo, ret_spy) / var_spy_daily
betas_spy <- sapply(
  setdiff(colnames(merged_betas), c("date", "spy_logret")),
  function(sym) {
    cov(merged_betas[[sym]], merged_betas$spy_logret,
        use = "complete.obs") / var_spy_daily
  }
)

# 13.10 Organizo los resultados en un tibble y lo ordeno de mayor a menor Beta
df_betas_spy <- tibble(
  Ticker = names(betas_spy),
  Beta   = unname(betas_spy)
) %>%
  arrange(desc(Beta)) %>%
  mutate(Ticker = fct_reorder(Ticker, Beta))

# 13.11 Gráfico de barras horizontales de Betas vs SPY
nudge_amt <- max(df_betas_spy$Beta) * 0.03

grafico_betas_spy <- ggplot(df_betas_spy, aes(x = Beta, y = Ticker, fill = Beta)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Beta)),
            position = position_nudge(x = nudge_amt),
            hjust = 0, size = 3) +
  scale_fill_gradient(name = expression(beta)) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(
    title = "Betas de cada activo vs SPY",
    x     = expression(beta),
    y     = NULL
  ) +
  theme(legend.position = "none",
        plot.title     = element_text(hjust = 0.5))

print(grafico_betas_spy)


# 13.12 Combinar Mapa de Correlaciones y Gráfico de Betas vs SPY --------
# Requiere 'mapa_correlaciones' definido previamente y 'patchwork' cargado
layout_spy <- "
AA
BB
BB
"
mapa_vs_betas_spy_side <- mapa_correlaciones + grafico_betas_spy +
  plot_layout(ncol = 2, widths = c(3, 2))  # ajusta proporción de anchos
print(mapa_vs_betas_spy_side)
