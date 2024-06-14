DOWNLOAD_SIM <- function(estados, anos) {
  # Lista de siglas dos estados brasileiros
  siglas <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", 
              "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", 
              "RO", "RR", "RS", "SC", "SE", "SP", "TO")
  
  # Verifica se todas as siglas fornecidas são válidas
  if (!all(estados %in% siglas)) {
    stop("Erro: A sigla do estado deve ser uma das seguintes: ", paste(siglas, collapse = ", "))
  }
  
  # Verifica se os anos fornecidos são válidos
  if (!all(anos %in% 2000:2024)) {
    stop("Erro: Os anos devem estar entre 2000 e 2024")
  }
  
  # Contar quantos estados e anos foram fornecidos
  num_estados <- length(estados)
  num_anos <- length(anos)
  
  # Criar variáveis de destino e URLs para download
  url_base <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/DO"
  idx <- 1
  
  for (estado in estados) {
    for (ano in anos) {
      assign(paste0("dest_file", idx), paste0("SIM_", estado, "_", ano, ".dbc"))
      assign(paste0("str_download", idx), paste0(url_base, estado, ano, ".dbc"))
      idx <- idx + 1
    }
  }
  
  # Realizar o download, ler os dados e salvar em formato CSV
  for (i in 1:(num_estados * num_anos)) {
    # Baixar o arquivo
    download.file(get(paste0("str_download", i)), destfile = get(paste0("dest_file", i)), mode = 'wb')
    
    # Ler o arquivo dbc (requer o pacote read.dbc)
    if (!requireNamespace("read.dbc", quietly = TRUE)) {
      stop("O pacote 'read.dbc' precisa ser instalado para ler os arquivos .dbc")
    }
    dados <- read.dbc::read.dbc(file = get(paste0("dest_file", i)))
    
    # Salvar como CSV
    write.csv2(dados, file = paste0("sim_", estados[ceiling(i / num_anos)], "_", anos[((i - 1) %% num_anos) + 1], ".csv"))
  }
}

estados = c('AP', 'AC', "AM", "PA", 'RO', 'RR', 'TO', 'MA', 'MT')

DOWNLOAD_SIM(estados, 2019)




















