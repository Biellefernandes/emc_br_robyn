#----------------------------------------------------------------------------------------------#
#                       Robyn MMM    (cliente: Stellantis) - 2024                              #
#----------------------------------------------------------------------------------------------#

pacotes <- c("tidyr", "dplyr", "lubridate", "readr")

# Instalar os pacotes
install.packages(pacotes)

library(tidyr)
library(dplyr)
library(lubridate) # biblioteca para trabalhar com datas
library(readr)

#------------------------------------------------------------------------------#
#  PARTE 1: AJUSTANDO AS BASES DE DADOS DO CLIENTE (MARCA: JEEP)               #
#------------------------------------------------------------------------------#

base_inicial <- JEEP
base_inicial

# Converter a variável 'data' para o formato Date
base_inicial$DATA_DAS_INFORMACOES <- as.Date(base_inicial$DATA_DAS_INFORMACOES)

# Consolidar os dados usando aggregate
base_1 <- aggregate(cbind(FLUXO_DE_LOJA,TEST_DRIVE,PEDIDOS_DE_VENDAS_TOTAIS,PEDIDOS_DE_VENDAS_DIRETA,PEDIDOS_DE_VENDAS_VAREJO) ~ DATA_DAS_INFORMACOES, data = base_inicial, FUN = sum)

# Visualizar os dados consolidados
print(base_1)


# A base de dados (dados_resumidos) está diarizada e com os valores consolidados!

base_2 <- emplacamentos_Jeep
base_2 

# Convertendo a variável 'data' para o formato Date
base_2$DATA_DAS_INFORMACOES <- as.Date(base_2$DATA_DAS_INFORMACOES)


# Juntando as bases de dados 1 e 2:

base_final <- merge(base_1, base_2, by = "DATA_DAS_INFORMACOES", all = TRUE)
base_final

# Arredondando a coluna de emplacamentos:

base_final$EMPLACAMENTO <- round(base_final$EMPLACAMENTO, digits = 0)

# Visualizando os dados finais

print(base_final)
View(base_final)


# Consolidando os dados diarizados em semanais:

data_inicio <- as.Date("2021-01-03") 
base_final <- base_final[base_final$DATA_DAS_INFORMACOES >= data_inicio, ]

base_final$DATA_DAS_INFORMACOES <- as.Date(cut(base_final$DATA_DAS_INFORMACOES, "7 days"))

# Agrupar os dados por grupo e somar as variáveis
base_final <- base_final %>%
  group_by(DATA_DAS_INFORMACOES) %>%
  summarize(
    FLUXO_DE_LOJA = sum(FLUXO_DE_LOJA, na.rm = TRUE),
    TEST_DRIVE = sum(TEST_DRIVE, na.rm = TRUE),
    PEDIDOS_DE_VENDAS_TOTAIS = sum(PEDIDOS_DE_VENDAS_TOTAIS, na.rm = TRUE),
    PEDIDOS_DE_VENDAS_DIRETA = sum(PEDIDOS_DE_VENDAS_DIRETA, na.rm = TRUE),
    PEDIDOS_DE_VENDAS_VAREJO = sum(PEDIDOS_DE_VENDAS_VAREJO, na.rm = TRUE),
    EMPLACAMENTO = sum(EMPLACAMENTO, na.rm = TRUE),
    .groups = "drop"  # Esta linha é necessária para evitar mensagens de aviso
  )

# Visualizar os dados consolidados com os intervalos de 5 dias e a soma das variáveis
print(base_final)
View(base_final)

# Salvando a base_final depois de ter trabalhado nela:

saveRDS(base_final,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_final.rds")
write_csv2(base_final,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_final.csv")


#------------------------------------------------------------------------------#
# AJUSTANDO AS BASES DE DADOS (DIGITAL E OFF - MARCA: JEEP)                    #
#------------------------------------------------------------------------------#

#------------ #
# (DIGITAL)   #
#------------ #

# Alterando o nome da variável semana para data e transformando a data:

base_digital <- V2_Jeep_Digital_2021_2023
View(base_digital)

base_digital <- base_digital %>% 
  rename(data = semana)

# Convertendo a variável 'data' para o formato Date
base_digital$data <- as.Date(base_digital$data)

# Reorganizando os dados
base_digital <- base_digital %>%
  pivot_wider(names_from = player, values_from = c(investimento,impressoes))

# Removendo as colunas que nao irao manter na base
base_digital <- base_digital %>%
  select(-marca,-modelo,-fase_funil,-configs,-leads)

# Exibindo os dados reorganizados
view(base_digital)


# Agrupar os dados por grupo e somar as variáveis
base_digital <- base_digital %>%
  group_by(data) %>%
  summarize(
    investimento_SEARCH = sum(investimento_SEARCH, na.rm = TRUE),
    investimento_DV360 = sum(investimento_DV360, na.rm = TRUE),
    `investimento_GOOGLE-ADS` = sum(`investimento_GOOGLE-ADS`, na.rm = TRUE),
    investimento_META = sum(investimento_META, na.rm = TRUE),   
    investimento_AMAZON = sum(investimento_AMAZON, na.rm = TRUE),
    investimento_TEADS = sum(investimento_TEADS, na.rm = TRUE),
    investimento_TWITTER = sum(investimento_TWITTER, na.rm = TRUE),
    investimento_YAHOO = sum(investimento_YAHOO, na.rm = TRUE),
    investimento_TIKTOK = sum(investimento_TIKTOK, na.rm = TRUE),
    investimento_PINTEREST = sum(investimento_PINTEREST, na.rm = TRUE),
    `investimento_MICROSOFT-ADS` = sum(`investimento_MICROSOFT-ADS`, na.rm = TRUE),
    investimento_VERIZON = sum(investimento_VERIZON, na.rm = TRUE),
    investimento_ORGÂNICO = sum(investimento_ORGÂNICO, na.rm = TRUE),
    investimento_LINKEDIN = sum(investimento_LINKEDIN, na.rm = TRUE),
    investimento_OUTROS = sum(investimento_OUTROS, na.rm = TRUE),
    `investimento_CONECTED-INTERATIVE` = sum(`investimento_CONECTED-INTERATIVE`, na.rm = TRUE),
    `investimento_CONNECTED INTERATIVE` = sum(`investimento_CONNECTED INTERATIVE`, na.rm = TRUE),
    investimento_HYPR = sum(investimento_HYPR, na.rm = TRUE),
    investimento_GDB = sum(investimento_GDB, na.rm = TRUE),
    `investimento_MERCADO LIVRE` = sum(`investimento_MERCADO LIVRE`, na.rm = TRUE),
    investimento_UOL = sum(investimento_UOL, na.rm = TRUE),
    impressoes_SEARCH = sum(impressoes_SEARCH, na.rm = TRUE),
    impressoes_DV360 = sum(impressoes_DV360, na.rm = TRUE),
    `impressoes_GOOGLE-ADS` = sum(`impressoes_GOOGLE-ADS`, na.rm = TRUE),
    impressoes_META = sum(impressoes_META, na.rm = TRUE),
    impressoes_AMAZON = sum(impressoes_AMAZON, na.rm = TRUE),
    impressoes_TEADS = sum(impressoes_TEADS, na.rm = TRUE),
    impressoes_TWITTER = sum(impressoes_TWITTER, na.rm = TRUE),
    impressoes_YAHOO = sum(impressoes_YAHOO, na.rm = TRUE),
    impressoes_TIKTOK = sum(impressoes_TIKTOK, na.rm = TRUE),
    impressoes_PINTEREST = sum(impressoes_PINTEREST, na.rm = TRUE),
    `impressoes_MICROSOFT-ADS` = sum(`impressoes_MICROSOFT-ADS`, na.rm = TRUE),
    impressoes_VERIZON = sum(impressoes_VERIZON, na.rm = TRUE),
    impressoes_ORGÂNICO = sum(impressoes_ORGÂNICO, na.rm = TRUE),
    impressoes_LINKEDIN = sum(impressoes_LINKEDIN, na.rm = TRUE),
    impressoes_OUTROS = sum(impressoes_OUTROS, na.rm = TRUE),
    `impressoes_CONECTED-INTERATIVE` = sum(`impressoes_CONECTED-INTERATIVE`, na.rm = TRUE),
    `impressoes_CONNECTED INTERATIVE` = sum(`impressoes_CONNECTED INTERATIVE`, na.rm = TRUE),
    impressoes_HYPR = sum(impressoes_HYPR, na.rm = TRUE),
    impressoes_GDB = sum(impressoes_GDB, na.rm = TRUE),
    `impressoes_MERCADO LIVRE` = sum(`impressoes_MERCADO LIVRE`, na.rm = TRUE),
    impressoes_UOL = sum(impressoes_UOL, na.rm = TRUE),
    .groups = "drop"  # Esta linha é necessária para evitar mensagens de aviso
  )

# Visualizar os dados consolidados e a soma das variáveis
View(base_digital)


# Salvando a base_final depois de ter trabalhado nela:

saveRDS(base_digital,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_digital.rds")
write_csv2(base_digital,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_digital.csv")


#------------ #
# (OFFLINE)   #
#------------ #

# Alterando o nome da variável semana para data e transformando a data:

base_off <- V3_Jeep_Offline_2021_2023
View(base_off)

base_off <- base_off %>% 
  rename(data = SEMANA)
print(base_off)

# Convertendo a variável 'data' para o formato Date
base_off$data <- as.Date(base_off$data)

# Agrupar os dados por grupo e somar as variáveis
base_off <- base_off %>%
  group_by(data, MEIO) %>%
  summarize(
    `BUDGET REALIZADO` = sum(`BUDGET REALIZADO`, na.rm = TRUE),
    IMPRESSÕES = sum(IMPRESSÕES, na.rm = TRUE),
    .groups = "drop"  # Esta linha é necessária para evitar mensagens de aviso
  )

# Reorganizando os dados
base_off <- base_off %>%
  pivot_wider(names_from = MEIO, values_from = `BUDGET REALIZADO`)

# Removendo as colunas que nao irao manter na base
base_off <- base_off %>%
  select(-IMPRESSÕES)

# Exibindo os dados reorganizados
View(base_off)

# Salvando a base_final depois de ter trabalhado nela:

saveRDS(base_off,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_off.rds")
write_csv2(base_off,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_off.csv")


#------------------------------------------------------------------------------#
# JUNTANDO AS BASES DE DADOS (DIGITAL, OFF E CLIENTE - MARCA: JEEP)            #
#------------------------------------------------------------------------------#

base_digital = readRDS(file="base_digital.rds")
View(base_digital)

base_robyn <- merge(base_digital, base_off, by = "data", all = TRUE)
base_robyn

View(base_robyn)


base_final = readRDS(file="base_final.rds")
View(base_final)

base_final <- base_final %>% 
  rename(data = DATA_DAS_INFORMACOES)
print(base_final)


base_robyn <- merge(base_robyn, base_final, by = "data", all = TRUE)
base_robyn

View(base_robyn)


# Salvando a base_robyn (base final ajustada e mergeada com as outras bases:

saveRDS(base_robyn,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_robyn.rds")
write_csv2(base_robyn,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_robyn.csv")



#------------------------------------------------------------------------------#
# AJUSTES NA BASE FINAL (base_robyn) DO MODELO  (NA/ARREDONDAMENTO, ETC)       #
#------------------------------------------------------------------------------#

base_robyn = readRDS(file="base_robyn.rds")
View(base_robyn)


# Arredondando todos os valores
  
#base_robyn[, -1] <- round(base_robyn[, -1], 0)    # Arredonda para 2 casas decimais
#View(base_robyn)


# Renomeando as variáveis:

base_robyn <- base_robyn %>% 
  rename(Data = data,
         Spend_SEARCH = investimento_SEARCH,
         Spend_DV360 = investimento_DV360,
         Spend_GOOGLE_ADS = `investimento_GOOGLE-ADS`,
         Spend_META = investimento_META,
         Spend_AMAZON = investimento_AMAZON,
         Spend_TEADS = investimento_TEADS,
         Spend_TWITTER = investimento_TWITTER,
         Spend_YAHOO = investimento_YAHOO,
         Spend_TIKTOK = investimento_TIKTOK,
         Spend_PINTEREST = investimento_PINTEREST,
         Spend_MICROSOFT_ADS = `investimento_MICROSOFT-ADS`,
         Spend_VERIZON = investimento_VERIZON,
         Spend_ORGANICO = investimento_ORGÂNICO,
         Spend_LINKEDIN = investimento_LINKEDIN,
         Spend_OUTROS = investimento_OUTROS,
         Spend_CONNECTED_INTERATIVE = `investimento_CONNECTED INTERATIVE`,
         Spend_HYPR = investimento_HYPR,
         Spend_GDB = investimento_GDB,
         Spend_MERCADO_LIVRE = `investimento_MERCADO LIVRE`,
         Spend_UOL = investimento_UOL,
         Imp_SEARCH = impressoes_SEARCH,
         Imp_DV360 = impressoes_DV360,
         Imp_GOOGLE_ADS = `impressoes_GOOGLE-ADS`,
         Imp_META = impressoes_META,
         Imp_AMAZON = impressoes_AMAZON,
         Imp_TEADS = impressoes_TEADS,
         Imp_TWITTER = impressoes_TWITTER,
         Imp_YAHOO = impressoes_YAHOO,
         Imp_TIKTOK = impressoes_TIKTOK,
         Imp_PINTEREST = impressoes_PINTEREST,
         Imp_MICROSOFT_ADS = `impressoes_MICROSOFT-ADS`,
         Imp_VERIZON = impressoes_VERIZON,
         Imp_ORGANICO = impressoes_ORGÂNICO,
         Imp_LINKEDIN = impressoes_LINKEDIN,
         Imp_OUTROS = impressoes_OUTROS,
         Imp_CONNECTED_INTERATIVE = `impressoes_CONNECTED INTERATIVE`,
         Imp_HYPR = impressoes_HYPR,
         Imp_GDB = impressoes_GDB,
         Imp_MERCADO_LIVRE = `impressoes_MERCADO LIVRE`,
         Imp_UOL = impressoes_UOL,
         Spend_PaidTV = PaidTV,
         Spend_Jornal = Jornal,
         Spend_Revista = Revista,
         Spend_OpenTV = OpenTV,
         Spend_OOH = OOH,
         Spend_Cinema = Cinema)

# Removendo as colunas que nao irao manter na base
base_robyn <- base_robyn %>%
  select(-`investimento_CONECTED-INTERATIVE`,-`impressoes_CONECTED-INTERATIVE`,
         -TEST_DRIVE,-PEDIDOS_DE_VENDAS_DIRETA,-PEDIDOS_DE_VENDAS_VAREJO)


# Consolidando os dados semanais:

data_inicio <- as.Date("2020-12-28") 
base_robyn <- base_robyn[base_robyn$Data >= data_inicio, ]

base_robyn$Data <- as.Date(cut(base_robyn$Data, "7 days"))

# Agrupar os dados por grupo e somar as variáveis
base_robyn <- base_robyn %>%
  group_by(Data) %>%
  summarize(
    Spend_SEARCH = sum(Spend_SEARCH, na.rm = TRUE),
    Spend_DV360 = sum(Spend_DV360, na.rm = TRUE),
    Spend_GOOGLE_ADS = sum(Spend_GOOGLE_ADS, na.rm = TRUE),
    Spend_META = sum(Spend_META, na.rm = TRUE),
    Spend_AMAZON = sum(Spend_AMAZON, na.rm = TRUE),
    Spend_TEADS = sum(Spend_TEADS, na.rm = TRUE),
    Spend_TWITTER = sum(Spend_TWITTER, na.rm = TRUE),
    Spend_YAHOO = sum(Spend_YAHOO, na.rm = TRUE),
    Spend_TIKTOK = sum(Spend_TIKTOK, na.rm = TRUE),
    Spend_PINTEREST = sum(Spend_PINTEREST, na.rm = TRUE),
    Spend_MICROSOFT_ADS = sum(Spend_MICROSOFT_ADS, na.rm = TRUE),
    Spend_VERIZON = sum(Spend_VERIZON, na.rm = TRUE),
    Spend_ORGANICO = sum(Spend_ORGANICO, na.rm = TRUE),
    Spend_LINKEDIN = sum(Spend_LINKEDIN, na.rm = TRUE),
    Spend_OUTROS = sum(Spend_OUTROS, na.rm = TRUE),
    Spend_CONNECTED_INTERATIVE = sum(Spend_CONNECTED_INTERATIVE, na.rm = TRUE),
    Spend_HYPR = sum(Spend_HYPR, na.rm = TRUE),
    Spend_GDB = sum(Spend_GDB, na.rm = TRUE),
    Spend_MERCADO_LIVRE = sum(Spend_MERCADO_LIVRE, na.rm = TRUE),
    Spend_UOL = sum(Spend_UOL, na.rm = TRUE),
    Imp_SEARCH = sum(Imp_SEARCH, na.rm = TRUE),
    Imp_DV360 = sum(Imp_DV360, na.rm = TRUE),
    Imp_GOOGLE_ADS = sum(Imp_GOOGLE_ADS, na.rm = TRUE),
    Imp_META = sum(Imp_META, na.rm = TRUE),
    Imp_AMAZON = sum(Imp_AMAZON, na.rm = TRUE),
    Imp_TEADS = sum(Imp_TEADS, na.rm = TRUE),
    Imp_TWITTER = sum(Imp_TWITTER, na.rm = TRUE),
    Imp_YAHOO = sum(Imp_YAHOO, na.rm = TRUE),
    Imp_TIKTOK = sum(Imp_TIKTOK, na.rm = TRUE),
    Imp_PINTEREST = sum(Imp_PINTEREST, na.rm = TRUE),
    Imp_MICROSOFT_ADS = sum(Imp_MICROSOFT_ADS, na.rm = TRUE),
    Imp_VERIZON = sum(Imp_VERIZON, na.rm = TRUE),
    Imp_ORGANICO = sum(Imp_ORGANICO, na.rm = TRUE),
    Imp_LINKEDIN = sum(Imp_LINKEDIN, na.rm = TRUE),
    Imp_OUTROS = sum(Imp_OUTROS, na.rm = TRUE),
    Imp_CONNECTED_INTERATIVE = sum(Imp_CONNECTED_INTERATIVE, na.rm = TRUE),
    Imp_HYPR = sum(Imp_HYPR, na.rm = TRUE),
    Imp_GDB = sum(Imp_GDB, na.rm = TRUE),
    Imp_MERCADO_LIVRE = sum(Imp_MERCADO_LIVRE, na.rm = TRUE),
    Imp_UOL = sum(Imp_UOL, na.rm = TRUE),
    Spend_PaidTV = sum(Spend_PaidTV, na.rm = TRUE),
    Spend_Jornal = sum(Spend_Jornal, na.rm = TRUE),
    Spend_Revista = sum(Spend_Revista, na.rm = TRUE),
    Spend_OpenTV = sum(Spend_OpenTV, na.rm = TRUE),
    Spend_OOH = sum(Spend_OOH, na.rm = TRUE),
    Spend_Cinema = sum(Spend_Cinema, na.rm = TRUE),
    FLUXO_DE_LOJA = sum(FLUXO_DE_LOJA, na.rm = TRUE),
    PEDIDOS_DE_VENDAS_TOTAIS = sum(PEDIDOS_DE_VENDAS_TOTAIS, na.rm = TRUE),
    EMPLACAMENTO = sum(EMPLACAMENTO, na.rm = TRUE),
    .groups = "drop"  # Esta linha é necessária para evitar mensagens de aviso
  )


# Visualizar os dados consolidados com os intervalos de 5 dias e a soma das variáveis
print(base_robyn)
View(base_robyn)


# Salvando a base_robyn com os ajustes finais:

saveRDS(base_robyn,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_robyn.rds")
write_csv2(base_robyn,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_robyn.csv")

