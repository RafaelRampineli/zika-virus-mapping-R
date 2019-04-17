# Mini Projeto 3 - Mapeando ocorrência do Zica Virus no Brasil. 

# Dados de Abril/2016 - Junho/2016
# Dados extraídos do link: http://combateaedes.saude.gov.br/situacao-epidemiologica

options(warn = 0)

##############################################################################################################################
                        ################## ETAPA 1: CARREGANDO OS DADOS ##################
##############################################################################################################################

# Carregando pacotes dplyr e ggplot2 que serão utilizados para análise e plotagem de gráficos.
# dplyr -> pacote para manipulação de dados utilizando conceitos SQL.
# ggplot2 -> pacote para criação de plots e gráficos.
library(dplyr)
library(ggplot2)

# Criando uma lista com todos os arquivos com extensão .CSV que se encontram no diretório atual.
lists_temp <- list.files(pattern = ".csv")
lists_temp

# Carregando todos os arquivos da lista dentro de um único objeto
zica_virus_list <- lapply(lists_temp, read.csv, stringsAsFactors = FALSE) # Não permite que campos Strings sejam transformados em factor.

# Verificando o tipo do arquivo
typeof(zica_virus_list)

# Obtendo um resumo dos dados contidos no arquivo
str(zica_virus_list)

##############################################################################################################################
                      ################## ETAPA 2: MANIPULANDO E PREPARANDO OS DADOS ################## 
##############################################################################################################################

# Utilizando a função do.call para combinar todas as listas do dataset orientado por linha (rbind), de modo que a operação
# será realizada para todas as listas do objeto.
# Sem a utilização da função do.call teriamos que executar o rbind x Nº de registros dentro da lista.
zicavirus <- do.call(rbind, zica_virus_list)

# Verificando se existem dados missing no dataset e removendo-os.
if (any(is.na(zicavirus))){
  
  for (n in ncol(zicavirus):1){
    
    if (any(is.na(zicavirus[n]))){
      
      zicavirus <- zicavirus %>% select(-n)
    }
  }
  rm(n)
}

# Resumo dos dados. Igual a função str
glimpse(zicavirus)

# Transformando a coluna report_date que está como character para date.
zicavirus <- zicavirus %>% 
  mutate(report_date = as.Date(report_date))

glimpse(zicavirus)

# Visulizando as primeiras linhas utilizando o mecanismo de slice.
# mesmo resultado que: head(zicavirus,20)
zicavirus %>% slice(1:20)

##############################################################################################################################
                  ################## ETAPA 3: APRESENTANDO OS DADOS EM FORMA DE GRÁFICOS ################## 
##############################################################################################################################

View(zicavirus)

# Selecionando somente os registros que possuem a sumarização por Região para então gerar um gráfico de linha.
# Para cada dia da análise, temos 5 regiões {Norte-Nordeste-Sudeste-Sul-Centro-Oeste}
zicavirus %>% filter(location_type == "region")

# Gerando um gráfico de linha com os incidentes de casos do zika virus pelas regiões do Brasil
zicavirus %>% filter(location_type == "region") %>%
  ggplot(aes(x = report_date, y = value, group = location, color = location)) +
  geom_line() +
  geom_point() +
  ggtitle("Zika Virus por Região do Brasil")

# Gerando um gráfico de barra por região do Brasil
zicavirus %>% filter(location_type == "region") %>%
  group_by(location) %>%
  summarise(Total = sum(value)) %>%
  arrange(desc(Total)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x =location, y = Total)) + geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  ylab("Número de Casos Reportados") + xlab("Região Brasil") + 
  ggtitle("Casos de Zika Virus Reportados no Brasil")

##############################################################################################################################
                      ################## ETAPA 4: REALIZANDO PADRONIZAÇÃO DOS DADOS ##################
##############################################################################################################################

# Atribuindo um vector do tamanho do numero de linhas do dataset
posicao_dataset <- vector()
length(posicao_dataset) <- nrow(zicavirus)

# Com base na análise realizada, foi criado uma lógica de acordo como os dados estão armazenados.
# Obs.: A ordem em que os dados estão armazenados influênciam totalmente no loop for abaixo.
for (n in 1:nrow(zicavirus)) {
  if (zicavirus[n,]$location_type == "region")  {
    newlab <- zicavirus[n,]$location
    posicao_dataset[n] <- newlab
  } else {
    posicao_dataset[n] <- newlab
  }
}

posicao_dataset

# Combinando os 2 datasets
zicavirus_combined <- cbind(zicavirus,posicao_dataset)


# Selecionando somente os dados agrupados por estado e removendo os dados sumarizados por Região e por País.
zicavirus_combined <- zicavirus_combined %>% filter(location_type == "state") 
View(zicavirus_combined)

##############################################################################################################################
      ################## ETAPA 5: APRESENTANDO O RESULTADO GRÁFICO NO GOOGLE MAPS DE FORMA ITERATIVA ################## 
##############################################################################################################################

# Carregando package do google maps para ser possível realizar o geo-reverse com base no endereço e recuperar uma latitude/longitude.
# install.packages("ggmap")
library(ggmap)

#?geocode
#?register_google

# API Key Google. É necessário criar a conta grátis por 12 meses com 300 reais para uso.
register_google(key = "cOLOQUE A API KEY AQUI")

# Na coluna Location do dataset está o nome dos estados. Existem n registros para cada estado, pois temos a ocorrência em diferentes datas.
# Para poupar processamento, estou pegando somente o valor único de cada estado. É como se fosse um DISTINCT de SQL.
# A operação mutate foi implementada para rertornar junto com a lat/lon o nome do estado e salvar na coluna loc que é criado em tempo
# de execução.
longlat <- geocode(unique(zicavirus_combined$location)) %>% 
  mutate(loc = unique(zicavirus_combined$location)) 

# Salvando os geocodes do dataframe zicavirus_combined.
# Estou aplicando um filter de data para poupar processamento e tempo
# O resultado da operação será atribuido a um novo dataframe chamado formapping
zicavirus_combined %>% filter(as.character(report_date) == "2016-06-11") %>% 
  group_by(location) %>% summarize(cases = sum(value)) %>% 
  inner_join(longlat, by = c("location" = "loc")) %>% 
  mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping

# Visualizando os dados
head(formapping) 

# Formatando a saída
# Tornando cada valor do cases um registro no novo dataset. Igual um ungroup
num_of_times_to_repeat <- formapping$cases
long_formapping <- formapping[rep(seq_len(nrow(formapping)),
                                  num_of_times_to_repeat),]

# Visualizando os dados
head(long_formapping)

# Instalando e carregando o pacote leaflet que permite plotar gráficos de mapa iterativo.
# install.packages("leaflet")
library(leaflet)

# Gerando o mapa iterativo com os dados do dataset
# Aplique o zoom
leaflet(long_formapping) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

# Observação: Todas as latitudes e longitudes de cada estado plotado são iguais. O model apenas foi criado para representação
# porém, com latitudes diferentes e reais, funcionará perfeitamente.

# Com essa análise foi possível concluir visivelmente que a região Sudeste e Nordeste do Brasil possuem maior casos reportados
# do Zika Virus no período de Abril/2016 - Junho/2016