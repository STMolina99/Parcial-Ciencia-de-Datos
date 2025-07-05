#Integrantes: Santiago Molina Torrillate
#Legajo <- 909503
#Email: santiagomolina46@gmail.com

#En esta seccion vamos a limpiar los datos que obtuvimos en 01_extraccion_datos
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

# Lista todos los CSV de raw_dir
csv_files <- list.files(
  path       = raw_dir,
  pattern    = "\\.csv$",     # sólo archivos .csv
  full.names = TRUE           # rutas completas
)

# Para cada CSV: leerlo y asignarlo como data.frame en GlobalEnv
for (f in csv_files) {
  # extrae el nombre del archivo sin carpeta ni extensión
  var_name <- tools::file_path_sans_ext(basename(f))
  # lee el CSV
  df <- read.csv(f, row.names = NULL, stringsAsFactors = FALSE)
  # asigna df a una variable con el nombre var_name
  assign(var_name, df, envir = .GlobalEnv)
}

#Pivoteamos los datos que obtuvimos con el paquete tidyquant
precios_activos_wide <- precios_activos %>%
                        pivot_wider(names_from = symbol, values_from = adjusted)
#Chequeamos que todas las columnas posean datos
colSums(is.na(precios_activos_wide))


# ————————————————————————— #
# Limpieza de balancesheet  #
# ————————————————————————— #

# 1) Identificamos los objetos
all_objs <- ls()
bs_objs  <- all_objs[grepl("^(?i)balancesheet_", all_objs, perl=TRUE)]

# 2) Recorremos cada uno
for (obj in bs_objs) {
  df <- get(obj, envir = .GlobalEnv)
  
  # a) Renombrar primera columna a "Concepto"
  names(df)[1] <- "Concepto"
  
  # b) Renombrar el resto de columnas: X2024FY → 2024, etc.
  old <- names(df)[-1]
  new <- sub("^X(\\d{4})FY$", "\\1", old, perl=TRUE)
  names(df)[-1] <- new
  
  # c) Guardamos de vuelta en el GlobalEnv
  assign(obj, df, envir = .GlobalEnv)
}

# ———————————————————————————— #
# Limpieza de IncomeStatement  #
# ———————————————————————————— #
is_objs <- ls(pattern = "(?i)^incomestatement_")  

for (obj in is_objs) {
  df <- get(obj, envir = .GlobalEnv)
  
  # 1) Renombra la primera columna a "Concepto"
  names(df)[1] <- "Concepto"
  
  # 2) Para el resto de columnas: deja TTM y convierte X2024FY→2024
  cols      <- names(df)
  to_rename <- cols[-1]
  new_names <- ifelse(
    to_rename == "TTM",
    "TTM",
    sub("^X(\\d{4})FY$", "\\1", to_rename)
  )
  names(df)[-1] <- new_names
  
  # 3) Guarda el df limpio
  assign(obj, df, envir = .GlobalEnv)
}


# ————————————————————— #
# Limpieza de Cashflow  #
# ————————————————————— #
cf_objs <- ls(pattern = "(?i)^cashflow_")

for (obj in cf_objs) {
  df <- get(obj, envir = .GlobalEnv)
  
  # 1) Primera columna → "Concepto"
  names(df)[1] <- "Concepto"
  
  # 2) Renombra años, preserva TTM
  cols      <- names(df)
  to_rename <- cols[-1]
  new_names <- ifelse(
    to_rename == "TTM",
    "TTM",
    sub("^X(\\d{4})FY$", "\\1", to_rename)
  )
  names(df)[-1] <- new_names
  
  # 3) Sobrescribe el objeto limpio
  assign(obj, df, envir = .GlobalEnv)
}

# ———————————————————————————————— #
# 1) Portfolio de IncomeStatement  #
# ———————————————————————————————— #


# 1) Identificamos todos los objetos IncomeStatement_*
is_objs <- ls(pattern = "^IncomeStatement_")

# 2) Para cada uno, lo pivotamos y renombramos el valor al ticker
long_list <- map(is_objs, function(obj_name) {
  # extraemos el ticker del nombre del objeto
  ticker <- sub("^IncomeStatement_(.*)$", "\\1", obj_name)
  
  # tomamos el data.frame, pivot_longer y renombramos la columna de valor
  get(obj_name, envir = .GlobalEnv) %>%
    pivot_longer(
      cols      = -Concepto,
      names_to  = "Year",
      values_to = ticker
    )
})

# 3) Hacemos un join secuencial por Concepto + Year
IncomeStatementPortfolio <- reduce(
  long_list,
  full_join,
  by = c("Concepto", "Year")
)

# ——————————————————————————————— #
# 2) Portfolio de Balance Sheets  #
# ——————————————————————————————— #
bs_objs <- ls(pattern = "^Balancesheet_")

bs_long_list <- map(bs_objs, function(obj_name) {
  ticker <- sub("^Balancesheet_(.*)$", "\\1", obj_name)
  get(obj_name) %>%
    pivot_longer(
      cols      = -Concepto,
      names_to  = "Year",
      values_to = ticker
    )
})

BalanceSheetPortfolio <- reduce(
  bs_long_list,
  full_join,
  by = c("Concepto", "Year")
)

glimpse(BalanceSheetPortfolio)


# ———————————————————————————— #
# 3) Portfolio de Cash Flows   #
# ———————————————————————————— #
cf_objs <- ls(pattern = "^Cashflow_")

cf_long_list <- map(cf_objs, function(obj_name) {
  ticker <- sub("^Cashflow_(.*)$", "\\1", obj_name)
  get(obj_name) %>%
    pivot_longer(
      cols      = -Concepto,
      names_to  = "Year",
      values_to = ticker
    )
})

CashflowPortfolio <- reduce(
  cf_long_list,
  full_join,
  by = c("Concepto", "Year")
)


out_dir <- "input"
if (!dir.exists(out_dir)) {
  dir.create(out_dir)
}

# ————————————————————————————————————————————— #
#  Guardar todos los portfolios y precios wide  #
# ————————————————————————————————————————————— #

# 1) Income Statement Portfolio
write.csv(
  IncomeStatementPortfolio,
  file.path(out_dir, "IncomeStatementPortfolio.csv"),
  row.names = FALSE
)

# 2) Balance Sheet Portfolio
write.csv(
  BalanceSheetPortfolio,
  file.path(out_dir, "BalanceSheetPortfolio.csv"),
  row.names = FALSE
)

# 3) Cashflow Portfolio
write.csv(
  CashflowPortfolio,
  file.path(out_dir, "CashflowPortfolio.csv"),
  row.names = FALSE
)

# 4) Precios activos en formato wide
write.csv(
  precios_activos_wide,
  file.path(out_dir, "precios_activos_wide.csv"),
  row.names = FALSE
)
