#Integrantes: Santiago Molina Torrillate
#Legajo <- 909503
#Email: santiagomolina46@gmail.com

#Voy a crear una carpeta donde me guarde los datos que vamos a ir recolectando
if(!dir.exists("raw")) {
  dir.create("raw")
}

raw_dir <- "raw"

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

#Vamos a definir el periodo que vamos a analizar los datos (2017-2025). Tener bastantes años nos va a dar una mejor nocion de como se movieron los activos
fecha_inicio <- as.Date("2017-01-01")
fecha_fin <- Sys.Date()

#Vamos a definir los activos financieron con los que trabajaremos.
tickers_acciones <- c(
  "AAPL", #Apple (Tecnologica)  
  "YPF", #YPF  (Commodities)
  "KO",   #Coca Cola (Consumo masivo)
  "PFE",  #Pfizer (Salud)
  "TM"    #Toyota (Automotriz)
)
precios_activos <- tq_get( #Esta funcion del paquete tidyquant nos permite traer los precios de los activos (precio de cierre) en el rango de tiempo que determinamos.
  tickers_acciones,
  get  = "stock.prices", #Esta informacion se obtiene de yahoo finance
  from = fecha_inicio,
  to   = fecha_fin
) %>%
  select(symbol, date, adjusted) # Nos quedamos con las columnas importantes

#Guardamos la primer base de datos con la que vamos a trabajar
write.csv(precios_activos, file.path(raw_dir, "precios_activos.csv"), row.names = FALSE)

#El objetivo de esta funcion es obtener el balance sheet, income statement y cashflow de los activos que seleccionamos previamente
#Dentro de la funcion se encuentra el scraping que hicimos de la pagina finviz (una de las paginas mas importantes de datos financieros)
getFinVizFiz = function(symbol,AQ,FS){
 # Construye la URL para la API de Finviz con el símbolo, el periodo AQ y el tipo de estado FS
 URL = paste0("https://finviz.com/api/statement.ashx?t=",symbol,"&so=F&s=",FS,AQ)
 headers=c(  # Define los encabezados HTTP necesarios para la petición GET
   "Host"="finviz.com",
   "User-Agent"= "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:139.0) Gecko/20100101 Firefox/139.0",
   "Accept" = "*/*",
   "Accept-Language"= "es-AR,es;q=0.8,en-US;q=0.5,en;q=0.3",
   "Accept-Encoding"= "gzip, deflate, br, zstd",
   "Referer"= "https://finviz.com/quote.ashx?t=AAPL&p=d",
   "Connection"= "keep-alive",
   "Cookie"= "chartsTheme=dark; notice-newsletter=show; finviz_ab=elitePage%3A1%3B0%3B1749691796; icAbTest3=control; survey_dialog_cohort=0; pv_date=Wed Jun 11 2025 22:29:57 GMT-0300 (hora estÃ¡ndar de Argentina); pv_count=3; ic_tagmanager=AY; usprivacy=1---; IC_ViewCounter_finviz.com=2; usprivacy=1NNN; _gcl_au=1.1.1927987265.1749691799; _ga_ZT9VQEWD4N=GS2.1.s1749691798$o1$g1$t1749691896$j60$l0$h0; _ga=GA1.1.1362314487.1749691799; _lr_retry_request=true; _lr_env_src_ats=false; _unifiedId=%7B%22TDID%22%3A%2296453a68-a9b8-4579-8506-f8251aca6dcb%22%2C%22TDID_LOOKUP%22%3A%22FALSE%22%2C%22TDID_CREATED_AT%22%3A%222025-06-12T01%3A30%3A00%22%7D; _unifiedId_cst=VyxHLMwsHQ%3D%3D; __gads=ID=9648793eeb8049e7:T=1749691801:RT=1749691801:S=ALNI_MZVcTVz3hE_INm74-El50alNmt8pg; __gpi=UID=000010d3750f7dcf:T=1749691801:RT=1749691801:S=ALNI_MaHD5Pv_8BCCubW96-NmXfnfm-_3Q; __eoi=ID=1bc84c54e2085c57:T=1749691801:RT=1749691801:S=AA-AfjbzrX8aihH-dxbmwPaNi8AV; _cc_id=bb9ace51d117883aaaca5073a0c4b6c3; panoramaId_expiry=1750296601633; panoramaId=651970c64833dacfecee21cc82cf185ca02cffb16ee2b2fd5cde07b55eb968f8; panoramaIdType=panoDevice; __binsSID=19871a2e-d84a-4092-ab6b-22b67e4aa6db; __binsUID=aeff7a0d-367d-448e-b4aa-8f92d74ad90a; _awl=2.1749691823.5-6e8ebeebda221af99d33be7fba013517-6763652d75732d6561737431-0; _lr_geo_location_state=B; _lr_geo_location=AR; _lr_sampling_rate=100; cto_bundle=mIM5MF82MSUyQmNCODdxTkpNNVZCcXdXQWpKTFd4Z0FOR1hCQ0tLdXJaWUM5b3JGZGFnQ1FHTWtpcnFVVG1BanRVOXd1ViUyQmZXa05kTTZCMFU2djdTQTVEUmYlMkZJbnpET0NRQ29Nd2VVYnAzWkZZJTJCNk1DTklMYjV3dyUyRklSZ2Zpb09XODQzMkxvUVRVYTFhTmdkSUlJaHN4eWUlMkZoY2clM0QlM0Q; cto_bundle=B-w6BF82MSUyQmNCODdxTkpNNVZCcXdXQWpKTFQ5d2JKbGM3RXclMkJtWFpFdVlMZ2hiTCUyQmYwVHc1QktiU3ZORldpSiUyRldrYVhIWCUyRmc3TiUyRmcwZGZuWnAlMkJrR2t5OVM4OERueHdSZzE1cEJ5NHdUUGZnMklVOFdUMWxkVEoyeTA5Rm12VCUyRkdHVGJWdGNGJTJGQzhpSkV5RVp0SFBRYVltOEElM0QlM0Q; cnx_userId=2-25fa2bde95d94715a2e3e38a5a9871db; cto_bidid=MrEdRV81MlFIQiUyQjBxeVozNU5udDlXeldTQUsxQ09hOEthZzE4VGpFaGJwT1N5SFpRRlE3anMwQ1dUViUyRiUyQkdjR1pmaWNXY25mWlNxUnhNa1VLV2xvQmZVOHpuRUtHRHh4dHZCTllwOEJJTnpJODhNNCUzRA; cto_dna_bundle=NvnRWV82MSUyQmNCODdxTkpNNVZCcXdXQWpKTGN6QTBIZDhTdE5ZUzBSeTViSnRibjQ0eUJKa2xYMWZVQ2FadHF4cTNLQjZyRWszTDdYdGg5UVNHbDdQQjFYemdnJTNEJTNE",
   "Sec-Fetch-Dest"= "empty",
   "Sec-Fetch-Mode"= "cors",
   "Sec-Fetch-Site"= "same-origin",
   "Priority"= "u=4")
 # Realiza la petición GET con los encabezados definidos
 pg <- httr::GET(url = URL, add_headers(headers))
 # Parsea el contenido de la respuesta (JSON) a lista en R
 res <- httr::content(pg)
 # Extrae y aplana los datos del resultado
 tbl <- unlist(res$data)
 # Genera nombres de filas eliminando el sufijo "1" de las claves únicas
 rName <- gsub("1", "", unique(names(tbl)[seq(1, length(tbl), 9)]))
 # Convierte el vector en una matriz con 9 columnas (cada fila de datos)
 df <- matrix(tbl, ncol = 9, byrow = TRUE)
 # Transforma la matriz en data.frame y asigna nombres de fila
 df <- data.frame(df, row.names = rName)
 # Define las columnas usando la primera fila como encabezado
 colnames(df) <- df[1,1:ncol(df)]
 # Elimina la fila de encabezados duplicada
 df <- df[-1,]
 print(df)
 # Convierte todas las columnas a numérico, quitando comas de miles
 dfn = data.frame(apply(df, 2, function(x) as.numeric(gsub("\\,", "", x))), row.names = rownames(df))
 # Conserva los mismos nombres de columna
 colnames(dfn) = colnames(df)
 # Filtra filas que no sean completamente NA
 dfn = dfn[rowSums(is.na(dfn)) != ncol(dfn),]
 dfn
}

#El objetivo de este loop es iterar los activos que elegimos para obtener los datos que scrapeamos en FinViz
for (ticker in tickers_acciones) {
  # 1) Limpiar el sufijo ".BA". En el caso que sea necesario. Nosotros utilizamos activos que cotizan en EEUU y en dolares. En el caso de querer activos que coticen en Argentina (cedears, panel general, panel lider, etc) se pone .BA despues del nombre del activo
  tmp <- gsub("\\.BA$", "", ticker)
  
  # 2) Caso especial: "YPFD" → "YPF" para FinViz
  if (tmp == "YPFD") {
    clean_ticker <- "YPF"
  } else {
    clean_ticker <- tmp
  }
  
  # 3) Llamadas a getFinVizFiz guardando directamente en variables con nombre dinámico
  assign(
    paste0("Balancesheet_", clean_ticker),
    getFinVizFiz(clean_ticker, "A", "B"),
    envir = .GlobalEnv
  )
  assign(
    paste0("IncomeStatement_", clean_ticker),
    getFinVizFiz(clean_ticker, "A", "I"),
    envir = .GlobalEnv
  )
  assign(
    paste0("Cashflow_", clean_ticker),
    getFinVizFiz(clean_ticker, "A", "C"),
    envir = .GlobalEnv
  )
  
  # 4) Guardar cada uno en CSV en la carpeta raw
  write.csv(
    get(paste0("Balancesheet_", clean_ticker)),
    file.path(raw_dir, paste0("Balancesheet_", clean_ticker, ".csv")),
    row.names = TRUE
  )
  write.csv(
    get(paste0("IncomeStatement_", clean_ticker)),
    file.path(raw_dir, paste0("IncomeStatement_", clean_ticker, ".csv")),
    row.names = TRUE
  )
  write.csv(
    get(paste0("Cashflow_", clean_ticker)),
    file.path(raw_dir, paste0("Cashflow_", clean_ticker, ".csv")),
    row.names = TRUE
  )
}