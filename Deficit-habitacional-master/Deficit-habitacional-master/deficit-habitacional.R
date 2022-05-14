# Pacotes necessários
library(DBI)
library(tidyverse)
library(survey)
library(srvyr)
library(sidrar)


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

# Calcular os totais ####

# Habitação precária ####
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
  srvyr::group_by(RA_nome,genero,raca,job,domicilio) %>% 
  # Calcular os totais para cada situação
  # Criar uma variável auxiliar para contagem
  srvyr::mutate(count=1) %>%
  # Calcular o total da população, com seu intervalo de confiança
  srvyr::summarise(n=survey_total(count, vartype = "ci"))


A<-habitacao_precaria_ra %>% 
  filter(!is.na(domicilio)) %>% 
  ggplot(aes(x=RA_nome, y = n, fill =domicilio), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))+
  facet_wrap(.~, ncol=3)

tabelahabprecaria<-habitacao_precaria_ra %>% 
  select(RA_nome,domicilio,n) %>%
  filter(!is.na(domicilio)) %>% 
  srvyr::group_by(RA_nome,domicilio) %>% 
  srvyr::summarise(n=sum(n)) 
tabelahabprecaria<- spread(tabelahabprecaria,domicilio,n)

ggplotly(A)


  # Coabitação Familiar ####
cohabit_familiar_ra <- svy2018_pes %>%
  # Verificar quem declarou ter interesse em constituir um novo domicílio no DF
  srvyr::mutate(novo_dom=case_when(E21==1~1,
                                   TRUE~0)) %>% 
  # Agrupar por domicílio
  srvyr::group_by(A01nficha) %>%
  # Somar a quantidade de moradores por domicílio com intenção de construção
  srvyr::mutate(novo_dom=sum(novo_dom)) %>% 
  # Desagrupar
  srvyr::ungroup() %>%
  # Filtrar para o responsável (estatística por domicílio)
  srvyr::filter(E02==1) %>% 
  srvyr::mutate(
    # Familias conviventes
    fam_conv=case_when(B01==1&A01nfamilias!=1&novo_dom>0~1,
                       TRUE~0),
    # Cômodos
    comodo=case_when(B02==3&B01==1~1,
                     TRUE~0),
    # Uma ou outra situação
    cohabit_familiar=case_when(fam_conv==1|comodo==1~1,
                               TRUE~0)) %>%
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
  srvyr::group_by(RA_nome,job,genero,raca) %>% 
  # Calcular os totais para cada situação
  srvyr::summarise(fam_conv=survey_total(fam_conv,vartype=c("cv")),
                 comodo=survey_total(comodo,vartype=c("cv")),
                 cohabit_familiar=survey_total(cohabit_familiar,vartype=c("cv")))

tabelacoabitacao<-cohabit_familiar_ra %>% 
  select(RA_nome,job,genero,raca,fam_conv,comodo) %>%
  #filter(!is.na(domicilio)) %>% 
  srvyr::group_by(RA_nome,job,genero,raca) %>% 
  srvyr::summarise(fam_conv=sum(fam_conv), 
                   comodo=sum(comodo))
                   
    
    
tabelacoabitacao<- gather(tabelacoabitacao,"componente","n", 5:6)
  
B<-tabelacoabitacao %>% 
  #filter(!is.na(domicilio)) %>% 
  ggplot(aes(x=RA_nome, y = n, fill =componente), size=18)+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))
  #facet_wrap(.~job, ncol=3)


# Ônus excessivo com aluguel urbano ####
onus_aluguel_ra <- svy2018_pes %>%
  # Aplicar filtros
  srvyr::filter(
    # Para o responsável (estatística por domicílio)
    E02==1,
    # Renda domiciliar positiva
    renda_dom*inflator_geral>0,
    # Renda até três salários mínimos
    (renda_dom*inflator_geral)<=3*954,
    # Retirar valores inválidos para aluguel
    !B05 %in% c(77777,88888),
    # Manter somente domicílios com renda informada
    is.na(renda_dom)==F) %>% 
  # Calcular o ônus do aluguel
  srvyr::mutate(onus_aluguel=case_when(B03==3&B02!=3&B01==1&(B05*inflator_aluguel)>=(0.3*renda_dom*inflator_geral)~1,
                                       TRUE~0)) %>% 
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>% 
  # Calcular os totais 
  srvyr::summarise(onus_aluguel=survey_total(onus_aluguel,vartype = c("cv")))

C<-onus_aluguel_ra %>% 
  #filter(!is.na(domicilio)) %>% 
  ggplot(aes(x=fct_reorder(RA_nome,onus_aluguel), y = onus_aluguel, size=18))+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))
#facet_wrap(.~job, ncol=3)
ggplotly(C)


# Adensamento excessivo de domicílios alugados ####
adensamento_excessivo_ra <- svy2018_pes %>%
  # Para o responsável (estatística por domicílio)
  srvyr::filter(E02==1) %>% 
  # Calcular o adensamento excessivo
  srvyr::mutate(adens=case_when(B03==3&B01==1&A01npessoas/B12>3~1,
                                TRUE~0)) %>% 
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>%
  # Calcular os totais
  srvyr::summarise(adens=survey_total(adens, vartype=c("cv")))

D<-adensamento_excessivo_ra %>% 
  #filter(!is.na(domicilio)) %>% 
  ggplot(aes(x=fct_reorder(RA_nome,adens), y = adens, size=18))+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))
#facet_wrap(.~job, ncol=3)
ggplotly(D)




# Fazer todos os cálculos de uma vez para verificar quem esta em, pelo menos, alguma situação ####
deficit_ra <- svy2018_pes %>%
  srvyr::mutate(novo_dom=case_when(E21==1~1,
                                   TRUE~0)) %>% 
  srvyr::group_by(A01nficha) %>%
  srvyr::mutate(novo_dom=sum(novo_dom)) %>% 
  srvyr::ungroup() %>% 
  srvyr::filter(E02==1) %>%
  srvyr::mutate(
    # Domicílios rústicos
    dom_rust=case_when(B01==1&B08>=4&B02!=3~1,
                       TRUE~0),
    # Domicílios improvisados
    dom_imp=case_when(B01==2~1,
                      TRUE~0),
    # Uma ou outra situação
    habit_prec=case_when(dom_rust==1|dom_imp==1~1,
                         TRUE~0),
    # Familias conviventes
    fam_conv=case_when(B01==1&A01nfamilias!=1&novo_dom>0~1,
                       TRUE~0),
    # Cômodos
    comodo=case_when(B02==3&B01==1~1,
                     TRUE~0),
    # Uma ou outra situação
    cohabit_familiar=case_when(fam_conv==1|comodo==1~1,
                               TRUE~0),
    # Ônus aluguel
    onus_aluguel=case_when(B03==3&B02!=3&B01==1&(B05*inflator_aluguel>=0.3*renda_dom*inflator_geral)&renda_dom*inflator_geral>0&(renda_dom*inflator_geral)<=3*954&!B05 %in% c(77777,88888)~1,
                           TRUE~0),
    # Adensamento
    adens=case_when(B03==3&B01==1&A01npessoas/B12>3~1,
                    TRUE~0),
    # Qualquer situação de déficit
    deficit_habit=case_when(habit_prec==1|cohabit_familiar==1|onus_aluguel==1|adens==1~1,
                            TRUE~0)) %>%
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>% 
  # Calcular o déficit habitacional
  srvyr::summarise(deficit_habit=survey_total(deficit_habit,vartype=c("cv")))


# Fazer todos os cálculos de uma vez para verificar quem esta em, pelo menos, alguma situação ####
deficit_ra <- svy2018_pes %>%
  srvyr::mutate(novo_dom=case_when(E21==1~1,
                                   TRUE~0)) %>% 
  srvyr::group_by(A01nficha) %>%
  srvyr::mutate(novo_dom=sum(novo_dom)) %>% 
  srvyr::ungroup() %>% 
  srvyr::filter(E02==1) %>%
  srvyr::mutate(
    # Domicílios rústicos
    dom_rust=case_when(B01==1&B08>=4&B02!=3~1,
                       TRUE~0),
    # Domicílios improvisados
    dom_imp=case_when(B01==2~1,
                      TRUE~0),
    # Uma ou outra situação
    habit_prec=case_when(dom_rust==1|dom_imp==1~1,
                         TRUE~0),
    # Familias conviventes
    fam_conv=case_when(B01==1&A01nfamilias!=1&novo_dom>0~1,
                       TRUE~0),
    # Cômodos
    comodo=case_when(B02==3&B01==1~1,
                     TRUE~0),
    # Uma ou outra situação
    cohabit_familiar=case_when(fam_conv==1|comodo==1~1,
                               TRUE~0),
    # Ônus aluguel
    onus_aluguel=case_when(B03==3&B02!=3&B01==1&(B05*inflator_aluguel>=0.3*renda_dom*inflator_geral)&renda_dom*inflator_geral>0&(renda_dom*inflator_geral)<=3*954&!B05 %in% c(77777,88888)~1,
                           TRUE~0),
    # Adensamento
    adens=case_when(B03==3&B01==1&A01npessoas/B12>3~1,
                    TRUE~0),
    # Qualquer situação de déficit
    deficit_habit=case_when(habit_prec==1|cohabit_familiar==1|onus_aluguel==1|adens==1~1,
                            TRUE~0)) %>%
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>% 
  # Calcular o déficit habitacional
  srvyr::cascade(deficit_habit=survey_mean(deficit_habit,vartype=c("cv")),
                 .fill="DF")






# Calcular os totais ####

# Habitação precária ####
habitacao_precaria_ra <- svy2018_pes %>%
  # Filtar para o responsável, para termos estimativa por domicílios
  srvyr::filter(E02==1) %>%
  # Construir variáveis
  srvyr::mutate(
    # Domicílios rústicos
    dom_rust=case_when(B01==1&B08>=4&B02!=3~1,
                       TRUE~0),
    # Domicílios improvisados
    dom_imp=case_when(B01==2~1,
                      TRUE~0),
    # Uma ou outra situação
    habit_prec=case_when(dom_rust==1|dom_imp==1~1,
                         TRUE~0)) %>%
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>% 
  # Calcular os totais para cada situação
  srvyr::cascade(dom_rust=survey_total(dom_rust,vartype=c("cv")),
                 dom_imp=survey_total(dom_imp,vartype=c("cv")),
                 habit_prec=survey_total(habit_prec,vartype=c("cv")),
                 .fill="DF") 

# Coabitação Familiar ####
cohabit_familiar_ra <- svy2018_pes %>%
  # Verificar quem declarou ter interesse em constituir um novo domicílio no DF
  srvyr::mutate(novo_dom=case_when(E21==1~1,
                                   TRUE~0)) %>% 
  # Agrupar por domicílio
  srvyr::group_by(A01nficha) %>%
  # Somar a quantidade de moradores por domicílio com intenção de construção
  srvyr::mutate(novo_dom=sum(novo_dom)) %>% 
  # Desagrupar
  srvyr::ungroup() %>%
  # Filtrar para o responsável (estatística por domicílio)
  srvyr::filter(E02==1) %>% 
  srvyr::mutate(
    # Familias conviventes
    fam_conv=case_when(B01==1&A01nfamilias!=1&novo_dom>0~1,
                       TRUE~0),
    # Cômodos
    comodo=case_when(B02==3&B01==1~1,
                     TRUE~0),
    # Uma ou outra situação
    cohabit_familiar=case_when(fam_conv==1|comodo==1~1,
                               TRUE~0)) %>% 
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>% 
  # Calcular os totais para cada situação
  srvyr::cascade(fam_conv=survey_total(fam_conv,vartype=c("cv")),
                 comodo=survey_total(comodo,vartype=c("cv")),
                 cohabit_familiar=survey_total(cohabit_familiar,vartype=c("cv")),
                 .fill="DF") 

# Ônus excessivo com aluguel urbano ####
onus_aluguel_ra <- svy2018_pes %>%
  # Aplicar filtros
  srvyr::filter(
    # Para o responsável (estatística por domicílio)
    E02==1,
    # Renda domiciliar positiva
    renda_dom*inflator_geral>0,
    # Renda até três salários mínimos
    (renda_dom*inflator_geral)<=3*954,
    # Retirar valores inválidos para aluguel
    !B05 %in% c(77777,88888),
    # Manter somente domicílios com renda informada
    is.na(renda_dom)==F) %>% 
  # Calcular o ônus do aluguel
  srvyr::mutate(onus_aluguel=case_when(B03==3&B02!=3&B01==1&(B05*inflator_aluguel)>=(0.3*renda_dom*inflator_geral)~1,
                                       TRUE~0)) %>% 
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>% 
  # Calcular os totais 
  srvyr::cascade(onus_aluguel=survey_total(onus_aluguel,vartype = c("cv")),
                 .fill="DF") 

# Adensamento excessivo de domicílios alugados ####
adensamento_excessivo_ra <- svy2018_pes %>%
  # Para o responsável (estatística por domicílio)
  srvyr::filter(E02==1) %>% 
  # Calcular o adensamento excessivo
  srvyr::mutate(adens=case_when(B03==3&B01==1&A01npessoas/B12>3~1,
                                TRUE~0)) %>% 
  # Agrupar por RA
  srvyr::group_by(RA_nome) %>%
  # Calcular os totais
  srvyr::cascade(adens=survey_total(adens, vartype=c("cv")),
                 .fill="DF")

# Montar arquivo final
deficit_final <- habitacao_precaria_ra %>% 
  dplyr::select(RA_nome,habit_prec,habit_prec_cv) %>% 
  dplyr::left_join(adensamento_excessivo_ra) %>% 
  dplyr::left_join(cohabit_familiar_ra %>% 
                     dplyr::select(RA_nome,cohabit_familiar,cohabit_familiar_cv)) %>% 
  dplyr::left_join(onus_aluguel_ra)


deficit<-deficit_final%>% 
  srvyr::select(RA_nome,habit_prec,adens,cohabit_familiar,onus_aluguel)

deficit<- gather(deficit,"componente","n", 2:5)

E<-deficit %>% 
  filter(RA_nome!="DF") %>% 
  ggplot(aes(x=fct_reorder(RA_nome,n), y = n , size=18))+
  coord_flip()+
  labs( y = 'DHDE', x = 'Faixas Etárias (em anos)',fill='Situação Empregatícia')+
  geom_bar(position = "stack", stat = "identity")+
  theme(legend.position="right")+theme(legend.text = element_text(size=14))+
facet_wrap(.~componente, ncol=2)
ggplotly(E)

