library(tidyverse)
PA = read.csv2("sim_PA_2019.csv")
AM = read.csv2("sim_AM_2019.csv")
AC = read.csv2('sim_AC_2019.csv')
AP = read.csv2('sim_AP_2019.csv')
RO = read.csv2('sim_RO_2019.csv')
RR = read.csv2('sim_RR_2019.csv')
TO = read.csv2('sim_TO_2019.csv')
MA = read.csv2('sim_MA_2019.csv')
MT = read.csv2('sim_MT_2019.csv')



dados = rbind(dados,MT)

COD_MUN = as.character(COD_MUN)
dados$CODMUNRES= as.character(dados$CODMUNRES)

dadosfilt <- dados %>%
  filter(CODMUNRES %in% str_sub(COD_MUN, 1, 6))
write.csv2(dadosfilt, 'dados2019.csv')

