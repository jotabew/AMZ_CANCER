dados = read.csv2('dados2019.csv')
colunas <- c('DTOBITO','NATURAL','DTNASC','IDADE',
             'SEXO','CODMUNRES',
             'CAUSABAS')


dados = dados %>% select(all_of(colunas))

dados$DTOBITO = str_pad(dados$DTOBITO, width = 8, pad = "0")
dados$DTNASC = str_pad(dados$DTNASC, width = 8, pad = '0')



dados$DTOBITO = dmy(dados$DTOBITO)
dados$DTNASC  = dmy(dados$DTNASC )

dados$OBT_IDADE = time_length(dados$DTOBITO - dados$DTNASC, unit = "years")
write.csv2(dados, 'dados2019.csv')

#################

# 01  ## "From 0 to 4 years"
# 02  ## "From 5 to 9 years"
# 03  ## "From 10 to 14 years"
# 04  ## "From 15 to 19 years"
# 05  ## "From 20 to 24 years"
# 06  ## "From 25 to 29 years"
# 07  ## "From 30 to 34 years"
# 08  ## "From 35 to 39 years"
# 09  ## "From 40 to 44 years"
# 10  ## "From 45 to 49 years"
# 11  ## "From 50 to 54 years"
# 12  ## "From 55 to 59 years"
# 13  ## "From 60 to 64 years"
# 14  ## "From 65 to 69 years"
# 15  ## "From 70 to 74 years"
# 16  ## "From 75 to 79 years"
# 17  ## "From 80 years or more"
# 18  ## "Total"

dados = dados %>% mutate(FXET = 
                           case_when(
                             OBT_IDADE <= 4.99999999 ~ '01',
                             between(dados$OBT_IDADE, 5, 9.99999999) ~ '02',
                             between(dados$OBT_IDADE, 10, 14.99999999) ~ '03',
                             between(dados$OBT_IDADE, 15, 19.99999999) ~ '04',
                             between(dados$OBT_IDADE, 20, 24.99999999) ~ '05',
                             between(dados$OBT_IDADE, 25, 29.99999999) ~ '06',
                             between(dados$OBT_IDADE, 30, 34.99999999) ~ '07',
                             between(dados$OBT_IDADE, 35, 39.99999999) ~ '08',
                             between(dados$OBT_IDADE, 40, 44.99999999) ~ '09',
                             between(dados$OBT_IDADE, 45, 49.99999999) ~ '10',
                             between(dados$OBT_IDADE, 50, 54.99999999) ~ '11',
                             between(dados$OBT_IDADE, 55, 59.99999999) ~ '12',
                             between(dados$OBT_IDADE, 60, 64.99999999) ~ '13',
                             between(dados$OBT_IDADE, 65, 69.99999999) ~ '14',
                             between(dados$OBT_IDADE, 70, 74.99999999) ~ '15',
                             between(dados$OBT_IDADE, 75, 79.99999999) ~ '16',
                             OBT_IDADE >= 80 ~ "17"
                           ))
write.csv2(dados, 'dados2019.csv')
######################
######################
## POPULATION
######################
######################

library(brpop)

pop = mun_pop()
pop$mun = as.character(pop$mun)
COD_MUN = as.character(COD_MUN)
pop_filt = pop %>% filter(mun %in% str_sub(COD_MUN, 1,6))
write.csv2(pop_filt, 'popamz.csv')

pop_filt = pop_filt %>% filter(year == 2019)
pop_filt = pop_filt %>% filter(age_group != "Total")

pop_filt$FXET = case_when(
  pop_filt$age_group == "From 0 to 4 years"    ~ '01',
  pop_filt$age_group == "From 5 to 9 years"    ~ '02',
  pop_filt$age_group == "From 10 to 14 years"  ~ '03',
  pop_filt$age_group == "From 15 to 19 years"  ~ '04',
  pop_filt$age_group == "From 20 to 24 years"  ~ '05',
  pop_filt$age_group == "From 25 to 29 years"  ~ '06',
  pop_filt$age_group == "From 30 to 34 years"  ~ '07',
  pop_filt$age_group == "From 35 to 39 years"  ~ '08',
  pop_filt$age_group == "From 40 to 44 years"  ~ '09',
  pop_filt$age_group == "From 45 to 49 years"  ~ '10',
  pop_filt$age_group == "From 50 to 54 years"  ~ '11',
  pop_filt$age_group == "From 55 to 59 years"  ~ '12',
  pop_filt$age_group == "From 60 to 64 years"  ~ '13',
  pop_filt$age_group == "From 65 to 69 years"  ~ '14',
  pop_filt$age_group == "From 70 to 74 years"  ~ '15',
  pop_filt$age_group == "From 75 to 79 years"  ~ '16',
  pop_filt$age_group == "From 80 years or more"~ '17'
  

)

view(pop_filt)
write.csv2(pop_filt, 'popfilt19.csv')
#####################
# FAIXAS ETARIAS POP
# "From 0 to 4 years"     "From 10 to 14 years"   "From 15 to 19 years"   "From 20 to 24 years"   "From 25 to 29 years"   "From 30 to 34 years"  
# "From 35 to 39 years"   "From 40 to 44 years"   "From 45 to 49 years"   "From 5 to 9 years"     "From 50 to 54 years"   "From 55 to 59 years"  
# "From 60 to 64 years"   "From 65 to 69 years"   "From 70 to 74 years"   "From 75 to 79 years"   "From 80 years or more" "Total

