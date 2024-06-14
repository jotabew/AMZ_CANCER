dados <- read.csv2('dados2019.csv')


taxas_mortalidade <- dados_filtrados %>%
  group_by(CODMUNRES, FXET) %>%
  summarise(mortes = n(), .groups = 'drop') %>%
  left_join(populacao, by = c("CODMUNRES" = "mun", "FXET" = "FXET")) %>%
  mutate(taxa_mortalidade = (mortes / pop) * 100000)

library(tidyverse)

pop_filt$mun = as.integer(pop_filt$mun)
pop_filt$FXET = as.integer(pop_filt$FXET)

############
PADRONIZA <- function(dados, pop, CID) {
  
  # POP PADRAO - BRASIL 2010
  POP_PAD <- data.frame(
    FXET = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
    populacao = c(14858280, 15865195, 17397304, 17219086, 17476779, 17334110, 15955949, 14075108,
                  13184170, 11992439, 10276773, 8387564, 6596741, 4905988, 3792042, 2597990, 2975164)
  )
  
  # AGRUPAMENTO DA POPULACAO
  POPULACAO = pop %>% 
    group_by(mun, FXET) %>% 
    summarise(pop = sum(pop), .groups = 'drop')
  
  # FILTRAGEM DOS DADOS
  dados_filt = dados %>% filter(str_detect(CAUSABAS, CID))
  
  # CALCULO DO COEFICIENTE BRUTO
  TX_OBT = dados_filt %>% 
    group_by(CODMUNRES, FXET) %>%
    summarise(mortes = n(), .groups = 'drop') %>% 
    left_join(POPULACAO, by = c('CODMUNRES' = 'mun', 'FXET' = 'FXET')) %>% 
    mutate(taxa_mortalidade = (mortes / pop))
  
  # CALCULO DOS OBITOS ESPERADOS
  OBT_ESP = TX_OBT %>% 
    left_join(POP_PAD, by = 'FXET') %>% 
    mutate(obitos_esperados = taxa_mortalidade * populacao)
  
  # CALCULO DO COEFICIENTE PADRONIZADO
  TX_PAD = OBT_ESP %>% 
    group_by(CODMUNRES) %>% 
    summarise(TX_PAD = sum(obitos_esperados) / sum(populacao), .groups = 'drop') %>% 
    mutate(TX_PAD = TX_PAD * 100000)
  
  return(TX_PAD)
}




banco = PADRONIZA(dados,pop_filt, "C34")
view(banco)
