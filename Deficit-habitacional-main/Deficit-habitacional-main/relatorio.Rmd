---
title: "Deficit Habitacional"
author: "Diretoria do Monitoramento"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
    html_document:
       base_format: rticles::jss_article
       highlight: textmate
       theme: flatly
       number_sections: yes
       toc: yes
       toc_float:
         collapsed: yes
         smooth_scroll: no
---
# Déficit  Habitacional

O indicador busca calcular resultados para famílias em situação de déficit habitacional e/ou inadequação domiciliar, definidas mais especificamente como: a reposição de estoque, que contempla a necessidade de repor as habitações em função da sua precariedade e desgaste de uso. e o incremento de estoque, que refere-se a demanda por novas moradias decorente de: Coabitação forçada,ônus excessivo com aluguel e adensamento excessivo.

# Metodologia

O indicador Déficit Habitacional foi desenvolvido com base na metodologia definida na [Nota Técnica nº. 1 do IPEA Estimativas do déficit habitacional brasileiro (2007-2011) por municípios (2010)](https://www.ipea.gov.br/portal/images/stories/PDFs/nota_tecnica/130517_notatecnicadirur01.pdf), para o cálculo do déficit habitacional com dados do Censo 2010, que por sua vez, é baseada nos conceitos estabelecidos pela Fundação João Pinheiro (FJP) e o Ministério das Cidades.  
A base de dados utilizada foi a [PDAD 2018](http://www.codeplan.df.gov.br/microdados-pdad-2018/)(33 Ras), disponibilizada pela CODEPLAN.
entretanto, a metodologia proposta pela FJP se baseia no questionário da PNAD, mas como ambas pesquisas possuem as questões necessárias para o estudo, fez-se a equivalência entre suas questões.  
Note, que o indicador é composto por qualquer um de seus componentes ou subcomponentes, de modo que há domicílios nas quais mais de uma situação é observada, porém tal domicílio só é contado no total do déficit uma única vez. Em termos práticos, isso significa que a soma dos componentes é maior do que o valor do indicador do déficit habitacional.

## Precariedade

São consideradas habitações precárias todos aqueles domicílios que estejam classicados como **domícilios improvisados**. Outra categoria que também compõe as habitações precárias é a dos **domicílios Rústicos**, que são aqueles que embora sejam permanenetes não tenham alvenaria ou madeira aparelhada como material predominante nas paredes do domicílio.

## Coabitação

Contempla todas aquelas famílias que declaram viver em domicílios do tipo cômodo,independente da condição de sua ocupação (alugados,próprios ou cedidos), com isso tem-se o subcomponente **Famílias residentes em cômodos**. Há ainda o subcomponente **Famílias conviventes**, que ocorre quando há a presença de mais de um família residindo no mesmo domicílio e com intenção de mudar.

## Ônus excessivo com aluguel  

Caracteriza-se quando o peso pago com a prestação do aluguel no orçamento domiciliar for superior ou igual a 30% da renda domiciliar, sendo excluído do cálculo aquelas famílias cuja renda declarada é 0. Este indicador é calculado exclusivamente para os domicílios cujas rendas domiciliares são de até três salários mínimos. Segundo a metodologia desenvolvida pelo IPEA (2013), não é possível estabelecer que todos os domicílios que declaram a renda domiciliar igual a 0, apresentem-se estruturalmente sem renda (o que os impossibilitaria de fazer o pagamento do aluguel), ou se o fato é apenas conjuntural. Neste caso, optou-se por não incluir tais domicílios no cálculo do déficit.  

## Adensamento excessivo

Ocorre em domicílios alugados, que possuem mais de três habitantes por cômodo utilizado, permanentemente, como dormitório. 

# Resultados






```{r Pacotes, message=FALSE, warning=FALSE, echo=FALSE}
library(plotly)
library(tidyverse)
library(survey)
library(srvyr)
library(sidrar)
library(readr)
library(dplyr)                                
library(ggplot2)
library(knitr)
library(kableExtra)
library(htmltools)
library(plotly)

```


```{r bases, echo=FALSE,message=FALSE, warning=FALSE}
pdad_dom_2018 <- read_delim("~/Downloads/dom2018_33ras.csv", 
                            ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)%>% 
  
  # Renomear colunas
  dplyr::rename(A01nficha= A01nFicha,
                A01npessoas=A01nPessoas,
                A01nfamilias=A01nFamilias)

# Baixar microdados de pessoas do site da Codeplan
pdad_pes_2018 <- read_delim("~/Downloads/mor2018_33ras.csv", 
                            ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                            trim_ws = TRUE)

# Calcular inflator do aluguel, para referência de jul/2018
inflator_aluguel <-  
  dplyr::bind_rows(
    # Baixar informações do Sidra IBGE
    sidrar::get_sidra(api='/t/1419/n6/5300108/v/63/p/201803,201804,201805,201806,201807/c315/7448/d/v63%202') %>% 
      # Selecionar colunas de interesse
      dplyr::select("Mês (Código)","Mês","Valor") %>% 
      # Renomear colunas
      dplyr::rename_all(list(~c("ref","ref1","valor"))) %>% 
      # Criar variável para o mês 
      dplyr::mutate(MES_N=as.numeric(substr(ref,6,6)),
                    MES=str_trim(gsub("[0-9]","",ref1))) %>% 
      # Organizar variáveis
      dplyr::arrange(-MES_N) %>% 
      # Criar o inflator de março a julho
      dplyr::mutate(inflator_aluguel=cumprod((valor/100)+1)),
    # Fazer o mesmo processo para julho a outubro
    sidrar::get_sidra(api='/t/1419/n6/5300108/v/63/p/201807,201808,201809,201810/c315/7448/d/v63%202') %>% 
      dplyr::select("Mês (Código)","Mês","Valor") %>% 
      dplyr::rename_all(list(~c("ref","ref1","valor"))) %>% 
      dplyr::mutate(MES_N=as.numeric(substr(ref,6,6)),
                    MES=str_trim(gsub("[0-9]","",ref1))) %>% 
      dplyr::mutate(inflator_aluguel=1/cumprod((valor/100)+1))) %>% 
  # Selecionar variáveis de interesse
  dplyr::select(MES,inflator_aluguel) %>%
  # Ajustar mês de julho
  dplyr::mutate(inflator_aluguel=ifelse(MES=="julho",1,inflator_aluguel)) %>% 
  # Eliminar duplicadas
  dplyr::distinct()

# Calcular inflator geral, para referência de jul/2018
# Mesmo processo que o anterior, alterando somente o índice
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

# Calcular a renda domiciliar
renda_domiciliar <- pdad_pes_2018 %>%
  # Mudar para ausente os valores das variáveis G16,G19
  # com códigos 77777 ou 88888.
  # Vamos também mudar para 0 quando os valores que não se aplicarem
  # ou não forem observados rendimentos (66666,99999)
  dplyr::mutate_at(vars(G16,G19), # Variáveis a serem alteradas
                   # Função a ser aplicada
                   list(~case_when(. %in% c(77777,88888)~NA_real_,
                                   . %in% c(66666,99999)~0,
                                   TRUE~as.numeric(.)))) %>%
  # Mudar para ausente os valores das variáveis G201 até G204
  # com código 77777 e zerar os valores sem rendimento,
  # casos que não se aplica e não sabia responder (66666,88888,99999)
  dplyr::mutate_at(vars(G201:G204), 
                   # Função a ser aplicada
                   list(~case_when(. %in% c(77777)~NA_real_,
                                   . %in% c(66666,88888,99999)~0,
                                   TRUE~as.numeric(.)))) %>%
  # Selecionar apenas as variáveis de interesse
  dplyr::select(A01nficha,E02,G16,G19,G201:G204) %>%
  # Somar as variáveis modificadas para construir a renda individual
  dplyr::mutate(renda_individual=rowSums(.[,c("G16","G19",
                                              "G201","G202",
                                              "G203","G204")],na.rm = F)) %>%
  # Desconsiderar os empregados domesticos moradores e seus parentes
  dplyr::filter(!E02 %in% c(17,18)) %>%
  # Agrupar por domicílio
  dplyr::group_by(A01nficha) %>%
  # Somar os valores por domicílios
  dplyr::summarise(renda_dom=sum(renda_individual, na.rm = F),
                   # Construir o número de pessoas no domicílio, por esse critério de rendiment0
                   pessoas=n(),
                   # Calcular a renda domiciliar per capita
                   renda_pc=renda_dom/pessoas)

# Fazer a junção das bases
pdad_2018 <- pdad_dom_2018 %>%
  # Trazer as informações de pessoas para domicílios
  dplyr::left_join(pdad_pes_2018 %>% 
                     dplyr::select(-FATOR_PROJ),
                   by=c("A01ra"="A01ra",
                        "A01nficha"="A01nficha")) %>%
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
                # Ajustar os setores de interesse da PDAD 2018
                ra_setor=factor(case_when(A01setor==53011~"Asa Norte",
                                          A01setor==53012~"Asa Sul",
                                          A01setor==53013~"Noroeste",
                                          A01setor==53014~"Demais",
                                          A01setor==53020~"Gama",
                                          A01setor==53030~"Taguatinga",
                                          A01setor==53040~"Brazlândia",
                                          A01setor==53050~"Sobradinho",
                                          A01setor==53060~"Planaltina",
                                          A01setor==53070~"Paranoá",
                                          A01setor==53080~"Núcleo Bandeirante",
                                          A01setor==53090~"Ceilândia",
                                          A01setor==53100~"Guará",
                                          A01setor==53110~"Cruzeiro",
                                          A01setor==53120~"Samambaia",
                                          A01setor==53130~"Santa Maria",
                                          A01setor==53140~"São Sebastião",
                                          A01setor==53150~"Recanto Das Emas",
                                          A01setor==53160~"Lago Sul",
                                          A01setor==53170~"Riacho Fundo",
                                          A01setor==53180~"Lago Norte",
                                          A01setor==53190~"Candangolândia",
                                          A01setor==53200~"Águas Claras",
                                          A01setor==53210~"Riacho Fundo II",
                                          A01setor==53220~"Sudoeste/Octogonal",
                                          A01setor==53230~"Varjão",
                                          A01setor==53240~"Park Way",
                                          A01setor==53250~"SCIA-Estrutural",
                                          A01setor==53260~"Sobradinho II",
                                          A01setor==53271~"Jardim Botânico - Tradicional",
                                          A01setor==53272~"Jardim Mangueiral",
                                          A01setor==53280~"Itapoã",
                                          A01setor==53290~"SIA",
                                          A01setor==53300~"Vicente Pires",
                                          A01setor==53310~"Fercal",
                                          A01setor==53320~"Sol Nascente/Pôr do Sol",
                                          A01setor==53330~"Arniqueira")),
                # Criar uma variável com o mês da pesquisa para o join com os inflatores
                MES=months(lubridate::mdy(datavisita)),
                count=1) %>%
  # Pegar informações do inflator do aluguel
  dplyr::left_join(inflator_aluguel) %>% 
  # Pegar informações do inflator geral
  dplyr::left_join(inflator_geral) %>% 
  # Pegar informação da renda domiciliar
  dplyr::left_join(renda_domiciliar)


# Fazer o desenho inicial da PDAD 2018
sample.pdad2018 <- 
  survey::svydesign(id = ~A01nficha,
                    strata = ~A01setor,
                    weights = ~PESO_PRE,
                    nest=TRUE,
                    data=pdad_2018)

# Criar um objeto para pós estrato
post.pop <- pdad_2018 %>%
  # Agrupar por pos estrato
  dplyr::group_by(POS_ESTRATO) %>%
  # Calcular a população
  dplyr::summarise(Freq=first(POP_AJUSTADA_PROJ)) %>%
  # Desagrupar
  dplyr::ungroup() 

# Declarar o objeto de pós-estrato
svy2018_pes <- survey::postStratify(sample.pdad2018,~POS_ESTRATO,post.pop)

# Criar objeto para calcular os erros por bootstrap (Rao and Wu’s(n − 1) bootstrap)
svy2018_pes <- survey::as.svrepdesign(svy2018_pes, type = "subbootstrap")

# Ajustar estratos com apenas uma UPA (adjust=centered)
options( survey.lonely.psu = "adjust")

# Deixar no formato Tibble svy
svy2018_pes <- srvyr::as_survey(svy2018_pes)

```

```{r tabela, echo=FALSE, warning=FALSE, message=FALSE}

habitacao_precaria_ra <- svy2018_pes %>%
  # Filtar para o responsável, para termos estimativa por domicílios
  srvyr::filter(E02==1) %>%
  # Construir variáveis
  srvyr::mutate(domicilio=
                  # Domicílios rústicos
                  case_when(B01==1&B08>=4&B02!=3~"domicílio rústico",
                            # Domicílios improvisados
                            B01==2~"domícílio improvisado")) %>% 
  # Uma ou outra situação
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
  # Agrupar por RA
 srvyr::group_by(RA_nome,domicilio) %>% 
  # Calcular os totais para cada situação
  # Criar uma variável auxiliar para contagem
  srvyr::summarise(n=survey_total(vartype = "ci"),
                   # Calcular o percentual da população
                   pct=survey_mean(vartype = "ci",na.rm = TRUE))
```


```{r, echo=FALSE, warning=FALSE, message=FALSE}
tabelahabprecaria<-habitacao_precaria_ra %>% 
  select(RA_nome,domicilio,n) %>%
  filter(!is.na(domicilio)) %>% 
  srvyr::group_by(RA_nome,domicilio) %>% 
  srvyr::summarise(n=sum(n))
tabelahabprecaria<- spread(tabelahabprecaria,domicilio,n)
tabelahabprecaria[is.na(tabelahabprecaria)]<-0
tabelahabprecaria$'habitação precária' = tabelahabprecaria$`domícílio improvisado`+tabelahabprecaria$`domicílio rústico`
knitr::kable(tabelahabprecaria,fig. = "Precariedade e seus componentes.",col.names = c("Região Administrativa","domícílio improvisado", "domicílio rústico", "habitação precária"), digits = 0) %>%
  kable_styling(full_width = F)
```

```{r, echo=FALSE, warning=FALSE, message=FALSE}
A<-habitacao_precaria_ra%>% 
  #filter(!is.na(domicilio)) %>% 
  ggplot(aes(x=RA_nome, y = pct, fill=domicilio), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))#+
  #facet_wrap(.~job, ncol=2)
  
  
ggplotly(A)
```




