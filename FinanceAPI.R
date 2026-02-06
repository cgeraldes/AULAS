library(RMySQL)
library(httr)
library(jsonlite)
library(dplyr)
library(DBI)

# Função para obter configuração do banco de dados
config <- function() {
  list(
    user = "cgeraldes",
    password = "geraldinhos123",
    host = "cgwebsrv1",
    port = 3306,
    database = "gestativos"
  )
}

# Função para conectar ao MySQL
connect_to_db <- function() {
  db_config <- config()
  conn <- dbConnect(MySQL(),
                    user = db_config$user,
                    password = db_config$password,
                    host = db_config$host,
                    port = db_config$port,
                    dbname = db_config$database)
  return(conn)
}

backup_table_to_csv <- function() {
  conn <- connect_to_db()
  df <- dbReadTable(conn, "TimeSeriesHist")
  dbDisconnect(conn)
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("TimeSeriesHist_backup_", timestamp, ".csv")
  write.csv(df, filename, row.names = FALSE)
  
  message(paste("✅ Backup em CSV criado:", filename))
}

getDatafromRapidAPI <- function(isin) {
        url <- paste0("https://tepilora-etfs-and-funds.p.rapidapi.com/H/0/", isin, "/Json")
        
        headers <- add_headers(
          "X-RapidAPI-Key" = "36e83e7943msh7f9f8ef52101e48p1ed54ajsnd325c5c84239",
          "X-RapidAPI-Host" = "tepilora-etfs-and-funds.p.rapidapi.com"
        )
        
        response <- GET(url, headers)
        content <- content(response, "text", encoding = "UTF-8")
        json_data <- fromJSON(content, flatten = TRUE)
        
        if ("error.code" %in% names(json_data)) {
          df <- data.frame()
        } else {
          responsejson <- json_data$H[[1]]
          df <- as.data.frame(responsejson)
          df <- df[-1, ]  # Remover a primeira linha
          colnames(df) <- c("Date", "Close", "Dividend")
          
          df <- df %>% select(-Dividend)
          
          # Garantir que 'Date' seja do tipo character antes de usar strsplit
          df$Date <- as.character(df$Date)
          
          # Agora, aplicar strsplit para separar a data
          df$Date <- as.Date(sapply(strsplit(df$Date, " "), `[[`, 1), format = "%Y-%m-%d")
          
          df$Currency <- json_data$ListingCurrency
          df$Source <- "RAPIDAPI"
          df$ProdCode <- isin
        }
        return(df)
}

insertTimeSeriesHist <- function(df) {
  conn <- connect_to_db()

  # Apagar todos os registros da tabela antes de inserir novos dados
  dbExecute(conn, "DELETE FROM TimeSeriesHist")

  # Adicionar colunas de log
  df$LOG <- "Inserted"
  df$LOGDate <- Sys.time()

  # Selecionar as colunas corretas
  df <- df %>% select(Date, Close, Currency, Source, ProdCode, LOG, LOGDate)

  # Inserir os novos dados na tabela
  dbWriteTable(conn, "TimeSeriesHist", df, append = TRUE, row.names = FALSE)

  # Fechar a conexão com o banco de dados
  dbDisconnect(conn)
}

# Função para ler dados da tabela TimeSeriesHist
readTimeSeriesHist <- function() {
  conn <- connect_to_db()
  query <- "SELECT Date, Close, Currency, Source, ProdCode, LOG, LOGDate FROM TimeSeriesHist"
  df <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  return(df)
}

check_and_prepare_table <- function(conn) {
  # Verifica se a tabela existe
  tables <- dbListTables(conn)
  
  if (!"TimeSeriesHist" %in% tables) {
    message("Tabela não existe. Vai ser criada.")
    create_time_series_table(conn)
    return()
  }
  
  # Verifica se está vazia
  result <- dbGetQuery(conn, "SELECT COUNT(*) as n FROM TimeSeriesHist")
  if (result$n == 0) {
    message("Tabela está vazia. Vai ser recriada.")
    dbExecute(conn, "DROP TABLE IF EXISTS TimeSeriesHist")
    create_time_series_table(conn)
  }
}

create_time_series_table <- function(conn) {
  query <- "
    CREATE TABLE TimeSeriesHist (
      IDTimeSeriesReg INT AUTO_INCREMENT PRIMARY KEY,
      Date DATE,
      Close DOUBLE,
      Currency VARCHAR(10),
      Source VARCHAR(50),
      ProdCode VARCHAR(50),
      LOG VARCHAR(50),
      LOGDate DATETIME
    );
  "
  dbExecute(conn, query)
  message("Tabela TimeSeriesHist criada.")
}

remove_duplicates_time_series <- function() {
  conn <- connect_to_db()
  
  query <- "
    DELETE FROM TimeSeriesHist
    WHERE IDTimeSeriesReg NOT IN (
      SELECT * FROM (
        SELECT MAX(IDTimeSeriesReg)
        FROM TimeSeriesHist
        GROUP BY ProdCode, Date
      ) AS keep_ids
    );
  "
  
  dbExecute(conn, query)
  dbDisconnect(conn)
  
  message("✅ Registros duplicados removidos com subquery.")
}

df.fundos <- read.table("fundos.csv", sep=";", header = TRUE)

isin_list <- df.fundos$ProdFin

backup_table_to_csv()

for (isin in isin_list) {
  print(isin)
  df_data <- getDatafromRapidAPI(isin)
  
  if (nrow(df_data) > 0) {
    
    conn <- connect_to_db()
    check_and_prepare_table(conn)
    
    # 🔃 Preparar dados
    df_data$LOG <- "Inserted"
    df_data$LOGDate <- Sys.time()
    df_data <- df_data %>% select(Date, Close, Currency, Source, ProdCode, LOG, LOGDate)
    
    # 💾 Inserir
    dbWriteTable(conn, "TimeSeriesHist", df_data, append = TRUE, row.names = FALSE)
    dbDisconnect(conn)
  } else {
    message(paste("Sem dados válidos para:", isin))
  }
}

remove_duplicates_time_series()
