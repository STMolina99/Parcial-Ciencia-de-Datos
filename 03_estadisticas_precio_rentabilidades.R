#Integrantes: Santiago Molina Torrillate
#Legajo <- 909503
#Email: santiagomolina46@gmail.com

#Cargo los paquetes que voy a utilizar
paquetes <- c("rvest", "httr", "jsonlite", "quantmod", "tidyquant", "dplyr", "lubridate", "tidyr", "tinytex","purrr","ggplot2","patchwork","forcats","scales")

# 3. Instalar y cargar cada paquete
for (pkg in paquetes) {
  # Si no está instalado, lo instala (incluyendo dependencias)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  # Luego lo carga con library()
  library(pkg, character.only = TRUE)
}

raw_dir <- "raw"
out_dir <- "input"

files <- list.files(
  path       = "input",
  pattern    = "\\.csv$",
  full.names = TRUE
)

for (f in files) {
  df_name <- tools::file_path_sans_ext(basename(f))
  assign(df_name,
         read.csv(f, stringsAsFactors = FALSE),
         envir = .GlobalEnv)
}

rentabilidad_wide_logarit <- precios_activos_wide %>%
  arrange(date) %>%
  mutate(across(
    AAPL:TM,
    ~ log(.x / lag(.x)),
    .names = "{.col}_logret"
  )) %>%
  select(date, ends_with("_logret")) %>%
  filter(!is.na(AAPL_logret))

rent_yearly <- rentabilidad_wide_logarit %>%
  mutate(year = year(date)) %>%
  pivot_longer(
    cols      = ends_with("_logret"),
    names_to  = "symbol",
    values_to = "logret"
  ) %>%
  mutate(symbol = sub("_logret$", "", symbol)) %>%
  group_by(symbol, year) %>%
  summarise(
    ret_year = exp(sum(logret, na.rm = TRUE)) - 1,
    .groups   = "drop"
  )

# 3) Acumular 100 $
rent_yearly <- rent_yearly %>%
  arrange(symbol, year) %>%
  group_by(symbol) %>%
  mutate(
    index100 = 100 * cumprod(1 + ret_year)
  ) %>%
  ungroup()

# 4) Inflación real
inflation_df <- tibble::tibble(
  year      = 2017:2025,
  infl_rate = c(-0.021, -0.024, -0.018, -0.012, -0.047, -0.080, -0.041, -0.029, -0.026)
) %>%
  arrange(year) %>%
  mutate(
    index100   = 100 * cumprod(1 + infl_rate),
    symbol      = "Inflación USD"
  ) %>%
  select(symbol, year, index100)

# 5) Unir series
plot_df <- rent_yearly %>%
  select(symbol, year, index100) %>%
  bind_rows(inflation_df)

# 6) Plot final
ggplot(plot_df, aes(x = year, y = index100, color = symbol)) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c(
      "AAPL" = "#1b9e77", "KO" = "#d95f02", "PFE" = "#7570b3",
      "TM"   = "#e7298a", "YPF" = "#66a61e", "Inflación USD" = "black"
    ),
    name = ""
  ) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50") +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "$"),
    limits = c(0, max(plot_df$index100) * 1.05),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_x_continuous(breaks = unique(plot_df$year)) +
  labs(
    title    = "Inversion de 100 USD vs Inflación USD",
    subtitle = "Comparativa de 100 USD desde 2017 hasta la actualidad",
    x        = "Año",
    y        = "Valor Acumulado"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )





log_retornos_activos <- rentabilidad_wide_logarit %>%
                        select(ends_with("_logret"))

colnames(log_retornos_activos) <- gsub("_logret$", "", colnames(log_retornos_activos))

# Matriz de correlaciones
matriz_correlacion <- cor(log_retornos_activos, use = "pairwise.complete.obs")

# Pasar a formato largo
matriz_correlacion_larga <- matriz_correlacion %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  pivot_longer(cols      = -Var1,
               names_to  = "Var2",
               values_to = "Correlation")

# Definir orden deseado
niveles <- colnames(log_retornos_activos)  # c("AAPL","YPFD","KO","PFE","TM")

matriz_correlacion_larga <- matriz_correlacion_larga %>%
  mutate(
    Var1 = factor(Var1, levels = niveles),
    Var2 = factor(Var2, levels = niveles)
  )

# Graficar

mapa_correlaciones <- ggplot(matriz_correlacion_larga,
                             aes(x = Var1, y = Var2, fill = Correlation)) +
  # Pintamos cada tile con borde negro
  geom_tile(color = "black", size = 0.4) +
  scale_fill_gradientn(
    colours = c("#00008B", "#ffffff", "#ca0020"),  # azul intenso → blanco → rojo
    values  = c(0, 0.3, 1),
    limits  = c(0, 1),
    name    = "Correlación"
  ) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(matriz_correlacion_larga$Var1))) +
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

# --- Calcular los Betas ---
log_retornos_activos_no_na <- na.omit(log_retornos_activos)
# Construimos un índice “proxy” de mercado como el promedio de nuestros activos
# Aquí hacemos un promedio fila a fila de los log‐retornos diarios de todos los activos, obteniendo una serie de retornos diarios que es tu índice interno.
retorno_indice_cartera  <- rowMeans(log_retornos_activos_no_na)

#La varianza mide la dispersión de los retornos del índice; se usa en la fórmula del Beta.
varianza_indice_cartera <- var(retorno_indice_cartera)

betas <- sapply( #  Para cada activo: Beta = cov(activo, índice) / varianza(índice)
  colnames(log_retornos_activos_no_na),
  function(sym) {cov(log_retornos_activos_no_na[[sym]], retorno_indice_cartera) / varianza_indice_cartera}
)

df_betas <- tibble(
  Ticker = names(betas),
  Beta   = as.numeric(betas)
) %>%
  arrange(desc(Beta)) %>%
  mutate(Ticker = fct_reorder(Ticker, Beta))

# --- 5. Gráfico de Betas (barras rojas + texto fuera) ---
df_betas <- df_betas %>%
  arrange(desc(Beta)) %>%
  mutate(Ticker = fct_reorder(Ticker, Beta))

# ————— 3. Gráfico de Betas con texto fuera —————
nudge_amount <- max(df_betas$Beta) * 0.03

grafico_betas <- ggplot(df_betas, aes(x = Beta, y = Ticker, fill = Beta)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", Beta)),
            position = position_nudge(x = nudge_amount),
            hjust    = 0,
            color    = "black",
            size     = 3) +
  scale_fill_gradient(
    low  = "#6BAED6",
    high = "#00008B",
    name = expression(beta)
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Beta del Portfolio",
    x     = expression(beta),
    y     = NULL
  ) +
  theme(
    legend.position    = "none",
    axis.text.y        = element_text(face = "bold"),
    # Hacemos más visibles las líneas de la cuadrícula vertical
    panel.grid.major.x = element_line(color = "gray70", linewidth = 0.5),
    panel.grid.major.y = element_blank(),      # sigue sin líneas horizontales
    plot.title         = element_text(hjust = 0.5),
    plot.margin        = margin(5, 5, 5, 5),
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# --- 6. Combinar y mostrar los dos gráficos juntos ---
layout <- "
AAB
AAB
AAB
"

mapa_final_combinado <- mapa_correlaciones + grafico_betas +
  plot_layout(design = layout)

print(mapa_final_combinado)
