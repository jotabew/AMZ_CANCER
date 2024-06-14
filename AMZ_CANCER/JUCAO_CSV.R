library(dplyr)
library(purrr)
library(readr)

# Função para ler e juntar múltiplos arquivos CSV
JUNCAO_CSV <- function(diretorio) {
  
  # Ler o arquivo dbc (requer o pacote read.dbc)
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("O pacote 'dplyr' precisa ser instalado para juntar os arquivos .dbc")
  }
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("O pacote 'purrr' precisa ser instalado para juntar os arquivos .dbc")
  }
  
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("O pacote 'readr' precisa ser instalado para juntar os arquivos .dbc")
  }
  
  
  
  
  # Lista todos os arquivos CSV no diretório
  arquivos_csv <- list.files(diretorio, pattern = "\\.csv$", full.names = TRUE)
  
  # Lê cada arquivo e junta em um único data frame
  data_combined <- arquivos_csv %>%
    map_dfr(read_csv2)
  
  return(data_combined)
}

# Exemplo de uso
diretorio <- "C:\\Users\\wilso\\OneDrive\\Documentos\\PROJETOS_R\\AMZ_CANCER"
dados <- JUNCAO_CSV(diretorio)

