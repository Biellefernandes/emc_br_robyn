#----------------------------------------------------------------------------------------------#
#                       Robyn MMM    (cliente: Stellantis) - 2024                              #
#----------------------------------------------------------------------------------------------#

pacotes <- c("dplyr", "lubridate", "readr", "tidyverse", "naniar", "expss",
             "GGally", "rpart.plot", "rpart", "ggplot2", "plotly", "hrbrthemes")

# Instalar os pacotes
install.packages(pacotes)

#-----------------------------------------------------#
# 1) Instalando e carregando os pacotes basicos       #
#-----------------------------------------------------#

library(dplyr)
library(lubridate) # biblioteca para trabalhar com datas
library(readr)
library(tidyverse)   # pacote para trabalhar com os dados
library(naniar)      # biblioteca para mostrar os dados faltantes
library(expss)
library(GGally)
library(rpart.plot)  # gera gráficos mais bonitos
library(rpart)
library(ggplot2)
library(plotly)
library(hrbrthemes)

#----------------------------------------------------------------------------------------------#

#-----------------------------------------------------#
# PARTE 2: LIMPEZA, ANÁLISES DESCRITIVAS E AJUSTES    #
#-----------------------------------------------------#

# Leitura da base de dados #

base_robyn = readRDS(file="base_robyn.rds")
View(base_robyn)


#-----------------------------------------------------#
# 1) Leitura/estrutura da base de dados               #
#-----------------------------------------------------#

glimpse(base_robyn)


#-----------------------------------------------------#
# 2.1) Limpeza da base de dados                       #
#-----------------------------------------------------#

# Antes de ajustar/treinar qualquer modelo precisamos analisar com cuidado 
# a base de dados para não termos problemas no futuro.

# Os principais passos dessa análise de dados são:

# 1) breve análise a partir da função summary;
# 2) procurar dados faltantes;
# 3) procurar covariáveis com variância quase zero (ou zero);
# 4) procurar covariáveis com alta correlação.

#----------------------#
# 2.1.1) Breve análise #
#----------------------#

summary(base_robyn)

# Tirando da base de dados as variáveis que não tem nenhuma informação (coluna toda zerada):

base_robyn = base_robyn |> select(-c(Spend_MICROSOFT_ADS, Spend_VERIZON, Spend_ORGANICO,
                                     Spend_OUTROS, Spend_UOL, Imp_MICROSOFT_ADS, Imp_VERIZON,  
                                     Imp_ORGANICO, Imp_OUTROS, Imp_UOL))


#------------------------#
# 2.1.2) Dados faltantes # 
#------------------------#

# Após realizar o summary, foi possível observa que há muitos zeros e sendo assim irei substituir
# por NA para realizar as imputações necessárias:


# Definindo as colunas que serão modificadas:

colunas_para_modificar <- c("Spend_SEARCH", "Spend_DV360", "Spend_GOOGLE_ADS", "Spend_META", "Spend_AMAZON",   
                            "Spend_TEADS", "Spend_TWITTER", "Spend_YAHOO", "Spend_TIKTOK", "Spend_PINTEREST", "Spend_LINKEDIN",            
                            "Spend_CONNECTED_INTERATIVE", "Spend_HYPR", "Spend_GDB", "Spend_MERCADO_LIVRE", "Imp_SEARCH", "Imp_DV360",                 
                            "Imp_GOOGLE_ADS", "Imp_META", "Imp_AMAZON", "Imp_TEADS", "Imp_TWITTER", "Imp_YAHOO",                 
                            "Imp_TIKTOK", "Imp_PINTEREST", "Imp_LINKEDIN", "Imp_CONNECTED_INTERATIVE", "Imp_HYPR", "Imp_GDB",                   
                            "Imp_MERCADO_LIVRE", "Spend_PaidTV", "Spend_Jornal", "Spend_Revista", "Spend_OpenTV", "Spend_OOH",                 
                            "Spend_Cinema", "FLUXO_DE_LOJA", "PEDIDOS_DE_VENDAS_TOTAIS", "EMPLACAMENTO" )

# Substituindo os valores zerados por NA em cada coluna

for (coluna in colunas_para_modificar) {
  base_robyn[, coluna][base_robyn[, coluna] == 0] <- NA
}


# Verificando se há dados faltantes na base de dados: 

gg_miss_var(x = base_robyn) # gráfico
vis_miss(x = base_robyn)    # gráfico


# Mostra a quantidade de dados faltantes em cada variável:

base_robyn |> miss_var_summary()


# Imputação dos valores faltantes das variáveis quantitativas:

base_robyn = base_robyn |>
  mutate(Spend_SEARCH = replace_na(Spend_SEARCH, mean(Spend_SEARCH, na.rm = TRUE)),
         Spend_DV360 = replace_na(Spend_DV360, mean(Spend_DV360, na.rm = TRUE)),
         Spend_GOOGLE_ADS = replace_na(Spend_GOOGLE_ADS, mean(Spend_GOOGLE_ADS, na.rm = TRUE)),
         Spend_META = replace_na(Spend_META, mean(Spend_META, na.rm = TRUE)),
         Spend_AMAZON = replace_na(Spend_AMAZON, mean(Spend_AMAZON, na.rm = TRUE)),
         Spend_TEADS = replace_na(Spend_TEADS, mean(Spend_TEADS, na.rm = TRUE)),                
         Spend_TWITTER = replace_na(Spend_TWITTER, mean(Spend_TWITTER, na.rm = TRUE)),              
         Spend_YAHOO = replace_na(Spend_YAHOO, mean(Spend_YAHOO, na.rm = TRUE)),                
         Spend_TIKTOK = replace_na(Spend_TIKTOK, mean(Spend_TIKTOK, na.rm = TRUE)),                
         Spend_PINTEREST = replace_na(Spend_PINTEREST, mean(Spend_PINTEREST, na.rm = TRUE)),             
         Spend_LINKEDIN = replace_na(Spend_LINKEDIN, mean(Spend_LINKEDIN, na.rm = TRUE)),          
         Spend_CONNECTED_INTERATIVE = replace_na(Spend_CONNECTED_INTERATIVE, mean(Spend_CONNECTED_INTERATIVE, na.rm = TRUE)),
         Spend_HYPR = replace_na(Spend_HYPR, mean(Spend_HYPR, na.rm = TRUE)),    
         Spend_GDB = replace_na(Spend_GDB, mean(Spend_GDB, na.rm = TRUE)),                   
         Spend_MERCADO_LIVRE = replace_na(Spend_MERCADO_LIVRE, mean(Spend_MERCADO_LIVRE, na.rm = TRUE)),        
         Imp_SEARCH = replace_na(Imp_SEARCH, mean(Imp_SEARCH, na.rm = TRUE)),
         Imp_DV360 = replace_na(Imp_DV360, mean(Imp_DV360, na.rm = TRUE)),                
         Imp_GOOGLE_ADS = replace_na(Imp_GOOGLE_ADS, mean(Imp_GOOGLE_ADS, na.rm = TRUE)),
         Imp_META = replace_na(Imp_META, mean(Imp_META, na.rm = TRUE)),
         Imp_AMAZON = replace_na(Imp_AMAZON, mean(Imp_AMAZON, na.rm = TRUE)),               
         Imp_TEADS = replace_na(Imp_TEADS, mean(Imp_TEADS, na.rm = TRUE)),
         Imp_TWITTER = replace_na(Imp_TWITTER, mean(Imp_TWITTER, na.rm = TRUE)),
         Imp_YAHOO = replace_na(Imp_YAHOO, mean(Imp_YAHOO, na.rm = TRUE)),                 
         Imp_TIKTOK = replace_na(Imp_TIKTOK, mean(Imp_TIKTOK, na.rm = TRUE)),                 
         Imp_PINTEREST = replace_na(Imp_PINTEREST, mean(Imp_PINTEREST, na.rm = TRUE)), 
         Imp_LINKEDIN = replace_na(Imp_LINKEDIN, mean(Imp_LINKEDIN, na.rm = TRUE)),             
         Imp_CONNECTED_INTERATIVE = replace_na(Imp_CONNECTED_INTERATIVE, mean(Imp_CONNECTED_INTERATIVE, na.rm = TRUE)), 
         Imp_HYPR = replace_na(Imp_HYPR, mean(Imp_HYPR, na.rm = TRUE)), 
         Imp_GDB = replace_na(Imp_GDB, mean(Imp_GDB, na.rm = TRUE)),                   
         Imp_MERCADO_LIVRE = replace_na(Imp_MERCADO_LIVRE, mean(Imp_MERCADO_LIVRE, na.rm = TRUE)), 
         Spend_PaidTV = replace_na(Spend_PaidTV, mean(Spend_PaidTV, na.rm = TRUE)),
         Spend_Jornal = replace_na(Spend_Jornal, mean(Spend_Jornal, na.rm = TRUE)),             
         Spend_Revista = replace_na(Spend_Revista, mean(Spend_Revista, na.rm = TRUE)), 
         Spend_OpenTV = replace_na(Spend_OpenTV, mean(Spend_OpenTV, na.rm = TRUE)), 
         Spend_OOH = replace_na(Spend_OOH, mean(Spend_OOH, na.rm = TRUE)),                  
         Spend_Cinema = replace_na(Spend_Cinema, mean(Spend_Cinema, na.rm = TRUE)), 
         FLUXO_DE_LOJA = replace_na(FLUXO_DE_LOJA, mean(FLUXO_DE_LOJA, na.rm = TRUE)),
         PEDIDOS_DE_VENDAS_TOTAIS = replace_na(PEDIDOS_DE_VENDAS_TOTAIS, mean(PEDIDOS_DE_VENDAS_TOTAIS, na.rm = TRUE)),
         EMPLACAMENTO = replace_na(EMPLACAMENTO, mean(EMPLACAMENTO, na.rm = TRUE)) 
 )


base_robyn <- data.frame(lapply(base_robyn, function(x) round(x, 2)))
base_robyn <- base_robyn %>%
  mutate(FLUXO_DE_LOJA = round(base_robyn$FLUXO_DE_LOJA,0),
         PEDIDOS_DE_VENDAS_TOTAIS = round(PEDIDOS_DE_VENDAS_TOTAIS,0),
         EMPLACAMENTO = round(EMPLACAMENTO,0))


#-----------------------------------------------#
# 2.1.3) Covariáveis com variância (quase) zero #
#-----------------------------------------------#

# Para procurar as variáveis com variância (quase) zero vamos analisar a 
# variabilidade de cada variável. Nesse momento é importante tratar de forma 
# diferente as variáveis quantitativas das qualitativas, por isso foram criados
# os objetos qualitativas e quantitativas, que guardam os nomes das covariáveis 
# quantitativas e qualitativas. 

# A variabilidade das variáveis quantitativas será dada pela variância amostral,
# que pode ser encontrada a partir do comando var.

diag(var(base_robyn |>  select(where(is.numeric))))

# Spend_Jornal e Spend_Cinema apresentam variancia zerada porém não vou tirar da base.


#--------------------------------------#
# 2.1.4) Análise de Multicolinearidade #
#--------------------------------------#

# A Análise de Multicolinearidade é o processo de seleção de variáveis para garantir
# que as covariáveis da base não apresentam alta correlação entre si. Esse processo
# consiste em procurar variáveis altamente correlacionadas e, no caso destas 
# existirem, escolher algumas para ficarem na base e outras para saírem, de forma
# que a base final não contenha covariáveis com correlação maior que 80%.

view(base_robyn)

#--------------------------------------------------#
# 2.1.4.2) Entre pares de covariáveis qualitativas #
#--------------------------------------------------#

# Para mensurar a associação entre duas variáveis qualitativas será usado o 
# Coeficiente de Contingência Modificado. Primeiro serão feitas as contas para
# um par específico de covariáveis e depois a conta será generalizada para todos
# os pares.

mat_cor = base_robyn |>
  select(where(is.numeric)) |>
  cor()
mat_cor


#######################################################
# 2.3) Breve Análise Descritiva                       #
#######################################################

summary(base_robyn)


# Salvando a base_robyn com os ajustes finais:

saveRDS(base_robyn,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_robyn.rds")
write_csv2(base_robyn,file="C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024/base_robyn.csv")


#----------------------------------------------------------------------------------------------#

#-----------------------------------------------------#
# PARTE 3: APLICAÇÃO  DO ROBYN                        #
#-----------------------------------------------------#


#------------------------------------#
# Etapa 1: Instalaçao dos pacotes    #
#------------------------------------#

# Instalando as bibliotecas que iremos usar:
install.packages("Robyn")
install.packages("reticulate") # O pacote fornece um conjunto abrangente de ferramentas entre o Python e o R.
install.packages("dplyr")      # Pacote para realizar transformação de dados.
install.packages("tidyr")
install.packages("lubridate")  # Pacote para simplificar ao máximo a leitura de datas e extração de informações dessas datas.
install.packages("remotes") 
remotes::install_github("facebookexperimental/Robyn/R")

# Importando as bibliotecas que iremos usar:
library(Robyn)
library(reticulate)
library(dplyr)
library(tidyr)
library(lubridate)

# Configurando o ambiente virtual e instalando a biblioteca nevergrad
virtualenv_create("r-reticulate")
py_install("nevergrad", pip = TRUE)
use_virtualenv("r-reticulate", required = TRUE)

# Force multi-core use when running RStudio

Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)


#------------------------------------#
# Etapa 2: Carregando os dados       #
#------------------------------------#

# Leitura da base de dados #

base_robyn = readRDS(file="base_robyn.rds")
View(base_robyn)


# Diretório para onde você deseja exportar os resultados
robyn_directory <- "~/C:/Users/gabrielle.fernandes/OneDrive - insidemedia.net/Desktop/Projetos Essence/Projeto Robyn/Stellantis_2024"


#------------------------------------#
# Etapa 3: Especificação do modelo   #
#------------------------------------#

# Antes de rodar o modelo, vamos analisar o comportamento da série:

# Usual area chart
p <- base_robyn %>%
  ggplot( aes(x=Data, y=EMPLACAMENTO)) +
  geom_area(fill="#dece1d", alpha=0.5) +
  geom_line(color="#dece1d") +
  xlab("Data") +
  ylab("Emplacamento") 
#theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p


# Etapa 3.1: Especificando as variáveis de entrada (todos os parâmetros de entrada para o modelo)

InputCollect  <- robyn_inputs(
dt_input = base_robyn,                               #conjunto de dados
dt_holidays = dt_prophet_holidays,                  #feriados 
dep_var = "EMPLACAMENTO",                            #variável dependente (deve haver apenas uma)
dep_var_type = "conversion",                        # "receita" (ROI) ou "conversão" (CPA)
date_var = "Data",                                  #formato de data deve ser "2020-01-01 
prophet_vars = c("trend","season","holiday"),       #"tendência","estação", "dia da semana" & "feriado"
prophet_country = "BR", 
#context_vars = c("concorrente_vendas_B","eventos"), #ex: concorrentes, desconto, desemprego etc
paid_media_spends = c("Spend_SEARCH","Spend_DV360","Spend_GOOGLE_ADS","Spend_META","Spend_AMAZON","Spend_TEADS",
                      "Spend_TWITTER","Spend_YAHOO","Spend_TIKTOK","Spend_PINTEREST","Spend_LINKEDIN","Spend_CONNECTED_INTERATIVE",
                      "Spend_HYPR","Spend_GDB","Spend_MERCADO_LIVRE"),
paid_media_vars = c("Imp_SEARCH","Imp_DV360","Imp_GOOGLE_ADS","Imp_META","Imp_AMAZON","Imp_TEADS",
                    "Imp_TWITTER","Imp_YAHOO","Imp_TIKTOK","Imp_PINTEREST","Imp_LINKEDIN","Imp_CONNECTED_INTERATIVE",
                    "Imp_HYPR","Imp_GDB","Imp_MERCADO_LIVRE"),
organic_vars  = c("Spend_PaidTV","Spend_Revista","Spend_OpenTV","Spend_OOH"),    # "Spend_Jornal" e "Spend_Cinema" têm variação nula, o que significa que todos os valores nessas colunas são iguais. Logo, essas colunas não estão fornecendo informações úteis para a análise e por isso foram removidas.                #atividade de marketing sem gastos com mídia
#factor_vars = c("events"),      #força as variáveis em context_vars ou organic_vars a serem categóricas
window_start = "2021-12-20",
window_end = "2024-01-15",
adstock = "geometric")           #geométrico, weibull_cdf ou weibull_pdf.

print(InputCollect)


# Etapa 3a-2: Definindo e adicionando os hiperparâmetros

hyper_names(adstock = InputCollect$adstock,all_media = InputCollect$all_media)


# IMPORTANTE: defina plot = TRUE para ver gráficos auxiliares do efeito do hiperparâmetro na transformação
plot_adstock(plot=TRUE)
plot_saturation(plot=TRUE)


# Definindo limites superiores e inferiores para cada hiperparâmetro:

hyper_limits()

hyperparameters  <-  list (
  Spend_AMAZON_alphas = c(0.5,3),
  Spend_AMAZON_gammas = c(0.3,1),
  Spend_AMAZON_thetas = c(0,0.3),
  Spend_CONNECTED_INTERATIVE_alphas = c(0.5,3),
  Spend_CONNECTED_INTERATIVE_gammas = c(0.3,1),
  Spend_CONNECTED_INTERATIVE_thetas = c(0,0.3),
  Spend_DV360_alphas = c(0.5,3),
  Spend_DV360_gammas = c(0.3,1),
  Spend_DV360_thetas = c(0.1,0.4),
  Spend_GDB_alphas = c(0.5,3),
  Spend_GDB_gammas = c(0.3,1),
  Spend_GDB_thetas = c(0,0.3),
  Spend_GOOGLE_ADS_alphas = c(0.5,3),
  Spend_GOOGLE_ADS_gammas = c(0.3,1),
  Spend_GOOGLE_ADS_thetas = c(0,0.3),
  Spend_HYPR_alphas = c(0.5,3),
  Spend_HYPR_gammas = c(0.3,1),
  Spend_HYPR_thetas = c(0,0.4),
  Spend_LINKEDIN_alphas = c(0.5,3),
  Spend_LINKEDIN_gammas = c(0.3,1),
  Spend_LINKEDIN_thetas = c(0,0.4),
  Spend_MERCADO_LIVRE_alphas = c(0.5,3),
  Spend_MERCADO_LIVRE_gammas = c(0.3,1),
  Spend_MERCADO_LIVRE_thetas = c(0.3,0.8),
  Spend_META_alphas = c(0.5,3),
  Spend_META_gammas = c(0.3,1),
  Spend_META_thetas = c(0.3,0.8),
  Spend_OOH_alphas = c(0.5,3),
  Spend_OOH_gammas = c(0.3,1),
  Spend_OOH_thetas = c(0.1,0.4),
  Spend_OpenTV_alphas = c(0.5,3),
  Spend_OpenTV_gammas = c(0.3,1),
  Spend_OpenTV_thetas = c(0.3,0.8),
  Spend_PaidTV_alphas = c(0.5,3),
  Spend_PaidTV_gammas = c(0.3,1),
  Spend_PaidTV_thetas = c(0.1,0.4),
  Spend_PINTEREST_alphas = c(0.5,3),
  Spend_PINTEREST_gammas = c(0.3,1),
  Spend_PINTEREST_thetas = c(0.1,0.4),
  Spend_Revista_alphas = c(0.5,3),
  Spend_Revista_gammas = c(0.3,1),
  Spend_Revista_thetas = c(0.1,0.4),            
  Spend_SEARCH_alphas = c(0.5,3), 
  Spend_SEARCH_gammas = c(0.3,1),             
  Spend_SEARCH_thetas = c(0.3,0.8),
  Spend_TEADS_alphas = c(0.5,3),  
  Spend_TEADS_gammas = c(0.3,1),
  Spend_TEADS_thetas = c(0.1,0.4),               
  Spend_TIKTOK_alphas = c(0.5,3),
  Spend_TIKTOK_gammas = c(0.3,1),             
  Spend_TIKTOK_thetas = c(0.3,0.8),
  Spend_TWITTER_alphas = c(0.5,3),             
  Spend_TWITTER_gammas = c(0.3,1),
  Spend_TWITTER_thetas = c(0.1,0.4),            
  Spend_YAHOO_alphas = c(0.5,3),
  Spend_YAHOO_gammas = c(0.3,1),               
  Spend_YAHOO_thetas = c(0.1,0.4),              
  train_size = c(0.5, 0.8)           #Parâmetro de validação da série temporal
)


# Etapa 3a-3: Adicionando hiperparâmetros em robyn_inputs()

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)


# Verificando o ajuste de exposição de gastos:

if (length(InputCollect$exposure_vars) > 0) {
  lapply(InputCollect$modNLS$plots, plot)
}


#------------------------------------------------#
# Etapa 4:  Construindo o modelo inicial         #
#------------------------------------------------#

# Executando todos os testes e iterações:

OutputModels <- robyn_run(
  InputCollect = InputCollect, 
  cores = NULL,                
  iterations = 2000,           
  trials = 5,                  
  ts_validation = TRUE,        
  add_penalty_factor = FALSE   
)
print(OutputModels)


# Verificando os gráficos de convergência MOO (otimização multiobjetivo)

# Robyn aproveita o MOO de Nevergrad para sua etapa de seleção de modelo,
# retornando automaticamente um conjunto de resultados ideais.

OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot


# Verificando o gráfico de validação da série temporal (quando ts_validation == TRUE)

if (OutputModels$ts_validation) OutputModels$ts_validation_plot


# Calculando frentes de Pareto, agrupando e exportando resultados e gráficos

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto",         # escolhe automaticamente quantos pareto-fronts preencher
  # min_candidates = 100,         # principais modelos de pareto para agrupamento. Padrão para 100
  # calibration_constraint = 0.1, # intervalo c(0,01, 0,1) e padrão em 0,1
  csv_out = "pareto",             
  clusters = TRUE,                # Defina como TRUE para agrupar modelos semelhantes por ROAS
  export = create_files,          # criará arquivos localmente
  plot_folder = robyn_directory,  # caminho para exportação de plotagens e criação de arquivos
  plot_pareto = create_files      # Defina como FALSE para desativar a plotagem e salvar modelos de uma página
)
print(OutputCollect)


