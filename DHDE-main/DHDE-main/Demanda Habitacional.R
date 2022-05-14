#informando o diretório
setwd("C:/Users/Fabricio/Downloads")
# Carregar os pacotes necessários
library(survey)
library(srvyr)
library(readr)
library(magrittr)
library(srvyr)
library(dplyr)
library(survey)
library(tidyr)
library(lubridate)
library(sidrar)
library(stringr)
library(rgdal)
library(ggplot2)
library(ggrepel)
library(sf)
library(RColorBrewer)
library(ggspatial)
library(tidyverse)



#caregando a pdad moradores
pdad_2018_moradores <- read_delim("mor2018_33ras.csv",
                                  ";", escape_double = FALSE, trim_ws = TRUE)
#caregando a pdad domicilios
pdad_2018_domicilios <- read_delim("dom2018_33ras.csv",
                                   ";", escape_double = FALSE, trim_ws = TRUE)


renda_individual <- pdad_2018_moradores %>%
  # Vamos mudar para ausente os valores das variáveis G16,G19,G201 até G204
  # com códigos 77777 ou 88888.
  # Vamos também mudar para 0 quando os valores que não se aplicarem
  # ou não forem observados rendimentos
  dplyr::mutate_at(vars(G16,G19,G201:G204), # Variáveis a serem alteradas
                   # Função a ser aplicada
                   list(M=~case_when(. %in% c(77777,88888)~NA_real_,
                                     . %in% c(66666,99999)~0,
                                     TRUE~as.numeric(.)))) %>%
  # Selecionar apenas as variáveis de interesse
  dplyr::select(A01nficha,E02,idade_calculada,G16,G19,G201:G204,G16_M:G204_M) %>%
  # Somar as variáveis modificadas para construir a renda individual
  dplyr::mutate(renda_individual=rowSums(.[,c("G16_M","G19_M",
                                              "G201_M","G202_M",
                                              "G203_M","G204_M")],na.rm = F))



# Calcular inflator geral, para referência de jul/2018
inflator_geral <-
  dplyr::bind_rows(
    sidrar::get_sidra(api='/t/1419/n6/5300108/v/63/p/201803,201804,201805,201806,201807/c315/7169/d/v63%202') %>%
      dplyr::select("Mês (Código)","Mês","Valor") %>%
      dplyr::rename_all(list(~c("ref","ref1","valor"))) %>%
      dplyr::mutate(MES_N=as.numeric(substr(ref,6,6)),
                    MES=str_trim(gsub("[0-9]","",ref1))) %>%
      dplyr::arrange(-MES_N) %>%
      dplyr::mutate(inflator_geral=cumprod((valor/100)+1)),
    sidrar::get_sidra(api='/t/1419/n6/5300108/v/63/p/201807,201808,201809,201810/c315/7169/d/v63%202') %>%
      dplyr::select("Mês (Código)","Mês","Valor") %>%
      dplyr::rename_all(list(~c("ref","ref1","valor"))) %>%
      dplyr::mutate(MES_N=as.numeric(substr(ref,6,6)),
                    MES=str_trim(gsub("[0-9]","",ref1))) %>%
      dplyr::mutate(inflator_geral=1/cumprod((valor/100)+1))) %>%
  dplyr::select(MES,inflator_geral) %>%
  dplyr::mutate(inflator_geral=ifelse(MES=="julho",1,inflator_geral)) %>%
  dplyr::distinct()

# Fazer o join das bases
pdad <- pdad_2018_moradores %>%
  # Entrar com a função para left join
  dplyr::left_join(
    # Informar a base que iremos unir, filtrando para colunas repetidas
    pdad_2018_domicilios %>%
      # Filtrar as colunas repetidas
      dplyr::select(-c(A01ra,FATOR_PROJ)),
    by=c("A01nficha"="A01nFicha")) %>%
  # Ajustar o nome das RAs
  dplyr::mutate(RA_nome=factor(case_when(A01ra==1~"Plano Piloto",
                                         A01ra==2~"Gama",
                                         A01ra==3~"Taguatinga",
                                         A01ra==4~"Brazlândia",
                                         A01ra==5~"Sobradinho",
                                         A01ra==6~"Planaltina",
                                         A01ra==7~"Paranoá",
                                         A01ra==8~"Núcleo Bandeirante",
                                         A01ra==9~"Ceilândia",
                                         A01ra==10~"Guará",
                                         A01ra==11~"Cruzeiro",
                                         A01ra==12~"Samambaia",
                                         A01ra==13~"Santa Maria",
                                         A01ra==14~"São Sebastião",
                                         A01ra==15~"Recanto das Emas",
                                         A01ra==16~"Lago Sul",
                                         A01ra==17~"Riacho Fundo",
                                         A01ra==18~"Lago Norte",
                                         A01ra==19~"Candangolândia",
                                         A01ra==20~"Águas Claras",
                                         A01ra==21~"Riacho Fundo II",
                                         A01ra==22~"Sudoeste/Octogonal",
                                         A01ra==23~"Varjão",
                                         A01ra==24~"Park Way",
                                         A01ra==25~"SCIA/Estrutural",
                                         A01ra==26~"Sobradinho II",
                                         A01ra==27~"Jardim Botânico",
                                         A01ra==28~"Itapoã",
                                         A01ra==29~"SIA",
                                         A01ra==30~"Vicente Pires",
                                         A01ra==31~"Fercal",
                                         A01ra==32~"Sol Nascente/Pôr do Sol",
                                         A01ra==33~"Arniqueira")),
                # Criar uma variável com o mês da pesquisa para o join com os inflatores
                MES=months(lubridate::mdy(datavisita)),
                count=1) %>%
  # Pegar informações do inflator do aluguel
  # Pegar informações do inflator geral
  dplyr::left_join(inflator_geral) %>%
  # Pegar informação da renda domiciliar
  dplyr::left_join(renda_individual) %>%
  dplyr::mutate(renda_individual_real=renda_individual*inflator_geral)

# Declarar o desenho incial
sample.pdad <-
  survey::svydesign(id = ~A01nficha, # Identificador único da unidade amostrada
                    strata = ~A01setor, # Identificação do estrato
                    weights = ~PESO_PRE, # Probabilidade da unidade ser sorteada
                    nest=TRUE, # Parâmetro de tratamento para dos IDs dos estratos
                    data=pdad # Declarar a base a ser utilizada
  )

# Criar um objeto para pós estrato
post.pop <- pdad %>%
  dplyr::group_by(POS_ESTRATO) %>% # Agrupar por pos-estrato
  dplyr::summarise(Freq=max(POP_AJUSTADA_PROJ)) # Capturar o total da população

# Declarar o objeto de pós-estrato
# Estamos dizendo nesse passo qual é a população alvo para cada
# pós-estrato considerado
sample.pdad <- survey::postStratify(sample.pdad,~POS_ESTRATO,post.pop)

# Criar objeto para calcular os erros por bootstrap (Rao and Wu's(n ??? 1) bootstrap)
# J. N. K. Rao and C. F. J. Wu - Journal of the American Statistical Association
# Vol. 83, No. 401 (Mar., 1988), pp. 231-241
amostra <- survey::as.svrepdesign(sample.pdad, type = "subbootstrap")

# Ajustar estratos com apenas uma UPA (adjust=centered)
options( survey.lonely.psu = "adjust")

# Ajustar objeto de amostra, para uso com o pacote srvyr
amostra <- srvyr::as_survey(amostra)




populacao <-  amostra %>%
  # Selecionando as variáveis desejadas e filtrando somente a população entre 24 e 64 anos de idade
  srvyr::select(A01nficha,RA_nome,idade_calculada,E02,renda_individual_real) %>%
  srvyr::filter(!is.na(renda_individual_real)) %>%
  srvyr::filter(idade_calculada>=24 & idade_calculada<=64) %>%
  #criando as faixa etárias desejadas
  mutate(idade_calculada=case_when(idade_calculada>=24 & idade_calculada<=29 ~ "24 a 29",
                                   idade_calculada>=30 & idade_calculada<=39~"30 a 39",
                                   idade_calculada>=40 & idade_calculada<=64~"40 a 64")) %>%
  #avaliando chefes e não chefes
  mutate(situacao=case_when(E02==1~"Chefe",
                            E02!=1~"não chefe")) %>%
  srvyr::group_by(idade_calculada,situacao) %>%
  # Criar uma variável auxiliar para contagem
  srvyr::mutate(count=1) %>%
  # Calcular o total da população, com seu intervalo de confiança
  srvyr::summarise(n=survey_total(count, vartype = "ci"))

#criando o objeto para taxa de chefia
taxachefia<-populacao[,c(-4,-5)]%>%
  spread(situacao,n)
taxachefia$taxadechefia = taxachefia$Chefe/(taxachefia$Chefe+taxachefia$`não chefe`)


populacaodhde <-  amostra %>%
  # Filtrar somente a população dentre dos critérios do DHDE
  srvyr::select(A01nficha,RA_nome,idade_calculada,E02,renda_individual_real,E03,E04,G05,G13,G08,G09,G041) %>%
  #exclui  sem renda declarada
  srvyr::filter(!is.na(renda_individual_real)) %>%
  #idade
  srvyr::filter(idade_calculada>=24 & idade_calculada<65) %>%
  #situação
  srvyr::filter(E02!=1 & E02!=2 & E02!=3) %>%
  #faixas etárias
  mutate(idade_calculada=case_when(idade_calculada>=24 & idade_calculada<=29 ~ "24 a 29",
                                   idade_calculada>=30 & idade_calculada<=39~"30 a 39",
                                   idade_calculada>=40 & idade_calculada<=64~"40 a 64")) %>%
  mutate(faixa_renda_ind=case_when(renda_individual_real>=0    & renda_individual_real<=1800 ~ "Até R$1800",
                                   renda_individual_real>1800 &  renda_individual_real<=2600~"Entre R$1801 e R$2600",
                                   renda_individual_real>2600 &  renda_individual_real<=4000~"Entre R$2601 e R$4000",
                                   renda_individual_real>4000 &  renda_individual_real<=9000~"Entre R$4001 e R$9000",
                                   renda_individual_real>9000~"Maior que R$9000")) %>%
  mutate(genero=case_when(E03==1~"Masculino",
                          E03==2~"Feminino"))%>%
  mutate(raca=case_when(E04==1~"Branca",
                          E04==2~"Negra", #preto
                          E04==3~"Amarela",
                          E04==4~"Negra", #Parda
                          E04==5~"Indígena"))%>%
  mutate(job=case_when(
    G13==3 & G08 %in% c(1,2,4,14,15,16)~"informal",
    G08 %in% c(5,6,7,8) & G09==2~"informal",
    G13==88~"Não respondeu",
    G05%in% c(4,99) & G041!=1~"Desempregado",
    G041==1~"Aposentado",
    TRUE~"formal")) %>%
srvyr::group_by(A01nficha,RA_nome,idade_calculada,faixa_renda_ind,raca,genero,job) %>%
  # Criar uma variável auxiliar para contagem
  srvyr::mutate(count=1) %>%
  # Calcular o total da população, com seu intervalo de confiança
  srvyr::summarise(n=survey_total(count, vartype = "ci"))



#juntar as informações pessoais com as informaoes de renda
pop<-left_join(populacaodhde,taxachefia,by=c("idade_calculada"))


#seleciona as variaveis de interesse
Dhde<-pop[,-1]%>%
  srvyr::select(RA_nome,faixa_renda_ind,idade_calculada,n,taxadechefia,raca,genero,job) %>%
  srvyr::group_by(RA_nome,faixa_renda_ind,idade_calculada,taxadechefia,raca,genero,job) %>%
  srvyr::summarise(pop=sum(n))

Dhdegeral<-Dhde%>%
  srvyr::select(RA_nome,taxadechefia,pop) %>%
  srvyr::group_by(RA_nome) %>%
  srvyr::summarise(DHDE=sum(pop*taxadechefia))

####O caculo do DHDE####

Dhde$DHDE = Dhde$pop * Dhde$taxadechefia

#Dhde por faixa de Renda
Dhdeinf<-Dhde %>%
  srvyr::select(RA_nome,DHDE) %>%
  srvyr::group_by(RA_nome,faixa_renda_ind) %>%
  srvyr::summarise(DHDE=sum(DHDE))

Dhdeinf<-spread(Dhdeinf,faixa_renda_ind,DHDE)
Dhdeinf[is.na(Dhdeinf)]<-0
Dhdeinf<-gather(Dhdeinf,faixa_renda_ind,DHDE,2:6)

#criando o grafico Por situacao de emprego
ggplot(Dhde, aes(x=idade_calculada, y = DHDE, fill =job), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))+
  facet_wrap(.~RA_nome, ncol=3)

#criando o grafico Por Etnia
ggplot(Dhde, aes(x=RA_nome, y = DHDE, fill =raca), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Etnia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))+
  facet_wrap(.~job, ncol=3)


#criando o grafico Por situacao de emprego
ggplot(Dhde, aes(x=idade_calculada, y = DHDE, fill =genero), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))+
  facet_wrap(.~RA_nome, ncol=3)


#criando o grafico Por renda, faixa etária e RA do DHDE
ggplot(Dhde, aes(x=idade_calculada, y = DHDE, fill =faixa_renda_ind), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Faixas de Renda')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text((size=14)))+
  facet_wrap(.~RA_nome, ncol=3)



DF<-rgdal::readOGR("C:/Users/Fabricio/Downloads/Regiões Administrativas","Regiões Administrativas",use_iconv = TRUE, encoding ="UTF-8")
DF <- sf::st_as_sf(DF)
DF<- cbind(DF, st_coordinates(st_centroid(DF)))
DF$ra[DF$ra == "Pôr do Sol"] <- "Sol Nascente/Pôr do Sol"
DF$ra[DF$ra == "SCIA"] <- "SCIA/Estrutural"


Dhde1<-left_join(Dhdeinf,DF, by = c("RA_nome"="ra"))
Dhde1<-Dhde1 %>%
  mutate(cat=cut(DHDE,breaks=c(-Inf,0,2000,4000,6000,8000,10000,+Inf),
                 labels=c('sem dados','0-2000','2000-4000','4000-6000','6000-8000','8000-10000','+10000')))

#mapas dhde por faixas de renda
ggplot(Dhde1) +
  geom_sf(aes(geometry=geometry))+
  geom_sf(aes(geometry=geometry,fill=cat))+
  scale_fill_brewer(palette = "RdYlGn")+
  facet_wrap(.~faixa_renda_ind, ncol=3)+
  geom_sf_label(data = st_centroid(DF), aes(label=ra_num),size=3, fontface="bold", family="Arial",
                label.padding=unit(0.05,"line"))+
  labs(x="",y="",fill="DHDE")+
  # tira sistema cartesiano
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+
  ggspatial::annotation_scale(location ='br')+
  ggspatial::annotation_north_arrow(location='tl',
                                    style = ggspatial::north_arrow_nautical(),
                                    width = unit(1.8,'cm'),
                                    height = unit(1.8,'cm'))


#salvar csv com dados
write.table(Dhde,"Dhde.csv",
            sep = ";",
            fileEncoding = "latin1",
            row.names = F,
            dec=",",
            na = "")


#salvar csv com dados
write.table(populacaodhde,"populacaodhde.csv",
            sep = ";",
            fileEncoding = "latin1",
            row.names = F,
            dec=",",
            na = "")
