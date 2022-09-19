## ----setup, include=FALSE-----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- warning=F, message=F----------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
inicio<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx')
str(inicio)


## ---- warning=F, message=F----------------------------------------------------------------------------------
est.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                    range='B17:AC3842',sheet='ESTIMATES',col_names = T)

str(est.male)



## ---- warning=F, message=F----------------------------------------------------------------------------------

col_types <- c("text","text","skip","numeric","text", rep("numeric",23))

est.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                    range='B17:AC3842',
                sheet='ESTIMATES', col_names = T,
                col_types = col_types)

str(est.male)


## ---- warning=F, message=F----------------------------------------------------------------------------------
colnames<-c('variant','region','cntycode','type','parentcode','year',
            '0-4','5-9','10-14','15-19', '20-24',
            '25-29','30-34','35-39','40-44','45-49',
            '50-54','55-59','60-64','65-69','70-74',
            '75-79','80-84','85-89','90-94','95-99','100+')

colnames(est.male) <-colnames
str(est.male)


## ---- warning=F, message=F----------------------------------------------------------------------------------
est.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                    range='B18:AC3842',
                sheet='ESTIMATES',
                col_names = colnames,
                col_types = col_types)
str(est.male)


## ---- warning=F, message=F----------------------------------------------------------------------------------
est.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                      range='B18:AC3842',
                sheet='ESTIMATES',col_names = colnames,
                col_types = col_types)




## ---- warning=F, message=F----------------------------------------------------------------------------------

medium.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                       range='B18:AC4352',
                sheet='MEDIUM VARIANT',col_names = colnames,
                col_types = col_types)

medium.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                         range='B18:AC4352',
                sheet='MEDIUM VARIANT',col_names = colnames,
                col_types = col_types)

high.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                     range='B18:AC4352',
                sheet='HIGH VARIANT',col_names = colnames,
                col_types = col_types)

high.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                       range='B18:AC4352',
                sheet='HIGH VARIANT',col_names = colnames,
                col_types = col_types)

low.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                    range='B18:AC4352',
                sheet='LOW VARIANT',col_names = colnames,
                col_types = col_types)

low.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                      range='B18:AC4352',
                sheet='LOW VARIANT',col_names = colnames,
                col_types = col_types)




## ---- warning=F, message=F----------------------------------------------------------------------------------
est.male <- est.male %>%
  mutate(sex='Male')

est.female <- est.female %>%
  mutate(sex='Female')

medium.male <- medium.male %>%
  mutate(sex='Male')

medium.female <- medium.female %>%
  mutate(sex='Female')

low.male <- low.male %>%
  mutate(sex='Male')

low.female <- low.female %>%
  mutate(sex='Female')

high.male <- high.male %>%
  mutate(sex='Male')

high.female <- high.female %>%
  mutate(sex='Female')

#### OU

#high.female$sex<-'Female'
# ...



## ---- warning=F, message=F----------------------------------------------------------------------------------

pop<-est.male %>%
  rbind(est.female)%>%
  rbind(medium.male)%>%
  rbind(medium.female)%>%
  rbind(low.male)%>%
  rbind(low.female)%>%
  rbind(high.female)%>%
  rbind(high.male)



## ---- warning=F, message=F----------------------------------------------------------------------------------
library(tidyverse)
dados_wide<-tibble(pais=c('Brasil','Alemanha'),`2002`=c(2,0),`2014`=c(1,7))
dados_wide

dados_long<-dados_wide%>%
  gather(key=ano,value=gols,-pais)

dados_long

dados_wide2 <- dados_long %>%
  spread(key=ano, value=gols)
dados_wide2


## ---- warning=F, message=F----------------------------------------------------------------------------------
pop<-pop %>%
  gather(key=idgr,value=pop,
         -c('variant','region','cntycode','type','parentcode','year','sex'))


## ---- warning=F, message=F----------------------------------------------------------------------------------
pop<-pop %>%
  mutate(variant=factor(variant),
         type=factor(type),
         year=factor(year,ordered=T,
                     levels=seq(1950,2100,5)),
         sex=factor(sex),
         idgr=factor(idgr,ordered=T,
                     levels=c('0-4','5-9','10-14','15-19', '20-24', '25-29',
                            '30-34','35-39','40-44','45-49','50-54','55-59',
                            '60-64','65-69','70-74','75-79','80-84','85-89',
                            '90-94','95-99','100+')))


## ---- warning=F, message=F----------------------------------------------------------------------------------
#World

pop_World<-pop%>%
  filter(region=='WORLD')

library(scales)
library(hrbrthemes)

#th<-theme_ipsum_pub()
th<-theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

temp<-pop_World%>%
  group_by(year,variant)%>%
  summarise(pop=sum(pop))

ggplot(data=temp,aes(x=year,y=pop,group=variant,col=variant))+
  geom_line(size=1)+
  scale_y_continuous(name='População (em bil.)',
                     labels=label_comma(big.mark = '.',
                                        decimal.mark = ',',scale=10^-6))+
  scale_x_discrete(name='Ano')+
  scale_color_discrete(name='Variante')+
  th


## -----------------------------------------------------------------------------------------------------------
temp<-pop_World%>%
  filter(year=='2020')

ggplot(data=temp,aes(x=idgr,y=pop,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária 2020 - Mundo')+
  scale_y_continuous(name='População (Total)',
                     labels=label_number(big.mark = '.',decimal.mark = ',',
                                         scale=10^-6))+
  scale_x_discrete(name='Grupo de Idade')+
  scale_fill_discrete(name='Sexo')+
  th


## -----------------------------------------------------------------------------------------------------------
temp<-pop_World%>%
  filter(year=='2020')%>%
  mutate(poppir=if_else(sex=='Male',-pop,pop))

ggplot(data=temp,aes(x=idgr,y=poppir,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária 2020 - Mundo')+
scale_y_continuous(name='População (Total em Bilhões de Hab.)',
                     labels=label_number(big.mark = '.',decimal.mark = ',',
                                         scale=10^-6))+
  scale_x_discrete(name='Grupo de Idade')+
  scale_fill_discrete(name='Sexo')+
  th


## -----------------------------------------------------------------------------------------------------------
temp<-pop_World%>%
  filter(year=='2020')%>%
  group_by(sex,idgr)%>%
  summarise(pop=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/(sum(pop)))%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))
  

th<-theme_ipsum()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=8))

ggplot(data=temp,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária 2020 - Mundo')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.08,.08,.02),limits=c(-.08,.08))+
  scale_x_discrete(name='Grupo de Idade')+
  scale_fill_discrete(name='Sexo')+
  th


## -----------------------------------------------------------------------------------------------------------
temp<-pop_World%>%
  filter(year %in% seq(1950,2100,50))%>%
  group_by(sex,idgr,year)%>%
  summarise(pop=sum(pop))%>%
  ungroup()%>%
  group_by(year)%>%
  mutate(poprel=pop/(sum(pop)))%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))%>%
  ungroup()

th<-theme_ipsum()+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1,size=8),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1,size=8))

ggplot(data=temp,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  facet_wrap(~year)+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária 2020 - Mundo')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.08,.08,.02),limits=c(-.08,.08))+
  scale_x_discrete(name='Grupo de Idade')+
  scale_fill_discrete(name='Sexo')+
  th


## ---- warning=F, message=F----------------------------------------------------------------------------------

levels(pop$type)

# World Bank income groups
  # 1. High-income countries
  # 2. Middle-income countries
  #   2.2 Upper-middle-income countries
  #   2.2 Lower-middle-income countries
  # 3. Low-income countries
  # 4. No income group available

pop_Income<-pop%>%
  filter(type=="Income Group" & region != 'Middle-income countries')%>%
  mutate(region=factor(region,ordered=T,
         levels=c("Low-income countries",
                  "Lower-middle-income countries",
                  "Upper-middle-income countries",
                  "High-income countries",
                  "No income group available"
                  )))

ggplot(data=pop_Income,aes(x=year,y=pop,group=region,col=region))+
  geom_line(size=1)+
  scale_y_continuous(name='População (em bil.)',
                     labels=label_comma(big.mark = '.',
                                        decimal.mark = ',',scale=10^-6))+
  scale_x_discrete(name='Ano')+
  scale_color_discrete(name='Variante')+
  th



temp<-pop_Income%>%
  filter(type=='Income Group',variant %in% c('Estimates','Medium variant'))%>%
  group_by(year,region)%>%
  summarise(pop=sum(pop))

ggplot(data=temp,aes(x=year,y=pop,group=region,col=region))+
  geom_line(size=1)+
  scale_y_continuous(name='População (em bil.)',
                     labels=label_comma(big.mark = '.',
                                        decimal.mark = ',',scale=10^-6))+
  scale_x_discrete(name='Ano')+
  scale_color_discrete(name='Variante')+
  th




## ---- warning=F, message=F----------------------------------------------------------------------------------

temp<-pop_Income%>%
  filter(type=='Income Group',variant %in% c('Estimates','Medium variant'))%>%
  filter(!(year==2020 & variant=='Estimates'))%>%
  group_by(year,region)%>%
  summarise(pop=sum(pop))


ggplot(data=temp,aes(x=year,y=pop,group=region,col=region))+
  geom_line(size=1)+
  scale_y_continuous(name='População (em bil.)',
                     labels=label_comma(big.mark = '.',
                                        decimal.mark = ',',scale=10^-6))+
  scale_x_discrete(name='Ano')+
  scale_color_discrete(name='Grupo de renda')+
  th

  ##knitr::purl('PreparandoDB.Rmd')
  


## -----------------------------------------------------------------------------------------------------------
library(ggsci)
library(scales)
library(hrbrthemes)
library(gridExtra)

Gapminder<-readRDS('Gapminder.rds')

Gapminder_1880a2020<- Gapminder %>%
  filter(time %in% seq(1900,2040,40))

ggplot(data=Gapminder_1880a2020,aes(y=tfr,x=le,color=WB_income2017))+
  geom_point()+
  scale_x_continuous(labels=comma)+
  labs(x='Esperança de vida', y = 'Taxa de Fecundidade Total',color='Regiões do Mundo')+
  facet_wrap(~time)

ggplot(data=Gapminder_1880a2020,aes(y=tfr,x=ipp,color=six_regions))+
  geom_point()+
  scale_x_log10(labels=comma)+
  labs(x='Renda Per Capita', y = 'Taxa de Fecundidade Total',color='Regiões do Mundo')+
  facet_wrap(~time)

ggplot(data=Gapminder_1880a2020,aes(y=le,x=ipp,color=four_regions))+
  geom_point()+
  scale_x_log10(labels=comma)+
  labs(x='Renda Per Capita', y = 'Esperança de vida',color='Regiões do Mundo')+
  facet_wrap(~time)


## -----------------------------------------------------------------------------------------------------------
Gapminder_Varios<- Gapminder %>%
  filter((name == 'Brazil'|name=='Japan'
          |name=='Nigeria'|name == 'United Kingdom')&
           time<=2050)

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text=element_text(size = 8))

ggtfr<-ggplot(data=Gapminder_Varios,aes(y=tfr,x=time,color=name))+
  geom_line()+
  ggtitle('Taxa de Fecundidade Total por ano e país')+
  scale_x_continuous(breaks = seq(1800,2100,25))+
  scale_color_startrek()+
  labs(x='Ano', y = 'Filhos por mulher',color='País')+
  th

ggmi<-ggplot(data=Gapminder_Varios,aes(y=cm,x=time,color=name))+
  geom_line()+
  ggtitle('Mortalitade Infantil por ano e país')+
  scale_color_startrek()+
  scale_x_continuous(breaks = seq(1800,2100,25))+
  labs(x='Ano', y = 'Mortalitade Infantil',color='País',
       caption = "Fonte: Gapminder Foundation")+
  th

ggtfr
ggmi
ggarr<-grid.arrange(ggtfr, ggmi,
             ncol = 1, nrow = 2)


## -----------------------------------------------------------------------------------------------------------
brazil<-pop%>%
  filter(year==2020 &
           region=='Brazil' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

usa<-pop%>%
  filter(year==2020 &
           region=='United States of America' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

nigeria<-pop%>%
  filter(year==2020 &
           region=='Nigeria' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

chile<-pop%>%
  filter(year==2020 &
           region=='Chile' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text=element_text(size = 10),
        axis.text.x=element_text(size = 10),
        axis.text.y=element_text(size = 10))

ggplot(data=chile,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019",
       title='Chile',
       subtitle='2020')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_manual(name='Sexo',label=c('Mulheres','Homens'),
                      values=c('pink','blue'))+
  th

ggplot(data=brazil,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019",
       title='Brazil',
       subtitle='2020')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_manual(name='Sexo',label=c('Mulheres','Homens'),
                      values=c('pink','blue'))+
  th

ggplot(data=nigeria,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019",
       title='Nigeria',
       subtitle='2020')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_manual(name='Sexo',label=c('Mulheres','Homens'),
                      values=c('pink','blue'))+
  th

ggplot(data=usa,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019",
       title='United States of America',
       subtitle='2020')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_manual(name='Sexo',label=c('Mulheres','Homens'),
                      values=c('pink','blue'))+
  th


## -----------------------------------------------------------------------------------------------------------
paises<-pop%>%
  filter(year==2020 &
           (region=='Nigeria'| region=='Brazil'|
            region=='Japan'| region == 'United Kingdom'))%>%
  mutate(region=factor(region,
    levels=c("Brazil","Japan","Nigeria","United Kingdom")))%>%#,
    #labels=c('Midgard','Vanaheim','Helheim','Asgard')))%>%
  group_by(year,region)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.text=element_text(size = 10),
        axis.text.x=element_text(size = 10),
        axis.text.y=element_text(size = 10))

ggplot(data=paises,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  facet_wrap(~region)+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_manual(name='Sexo',label=c('Mulheres','Homens'),
                      values=c('pink','blue'))+
  th

paises<-pop%>%
  filter(year==2020 &
           (region=='Nigeria'| region=='Brazil'|
            region=='Japan'| region == 'United Kingdom'))%>%
  mutate(region=factor(region,
    levels=c("Brazil","Japan","Nigeria","United Kingdom"),
    labels=c('Midgard','Vanaheim','Helheim','Asgard')))%>%
  group_by(year,region)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

ggplot(data=paises,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  facet_wrap(~region)+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_manual(name='Sexo',label=c('Mulheres','Homens'),
                      values=c('pink','blue'))+
  th
  


## -----------------------------------------------------------------------------------------------------------

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size=10),
        legend.text = element_text(size = 8),
        axis.text.x=element_text(size = 9),
        axis.text.y=element_text(size = 8))

temp<-pop%>%
    filter(year %in% seq(1960,2020,20) & region=='Brazil')%>%
    group_by(year,region)%>%
    mutate(poptotal=sum(pop))%>%
    ungroup()%>%
    mutate(poprel=pop/poptotal)%>%
    mutate(poprel=if_else(sex=='Male',-poprel,poprel))

ggplot(data=temp,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  facet_wrap(~year)+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária - Brazil')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.08,.08,.04),limits=c(-.08,.08))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_startrek(name='Sexo',label=c('Mulheres','Homens'))+
  th



## -----------------------------------------------------------------------------------------------------------

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 20),
        axis.text.x=element_text(size = 12),
        axis.text.y=element_text(size = 12))


for(p in c("Brazil","Japan","Nigeria","United Kingdom")){
  
  df<-pop%>%
    filter(year==2020 & region==p)%>%
    group_by(year)%>%
    mutate(poptotal=sum(pop))%>%
    ungroup()%>%
    mutate(poprel=pop/poptotal)%>%
    mutate(poprel=if_else(sex=='Male',-poprel,poprel))
  
  ggp<-ggplot(data=df,aes(x=idgr,y=poprel,group=sex,fill=sex))+
    geom_bar(stat='identity')+
    coord_flip()+
    labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
    ggtitle('Pirâmide Etária',subtitle=paste(p,'- 2020'))+
    scale_y_continuous(name='População (%)',
                       labels=label_percent(big.mark = '.',decimal.mark = ','),
                       breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
    scale_x_discrete(name='Grupo de Idade')+
    scale_fill_startrek(name='Sexo',label=c('Mulheres','Homens'))+
    th
  
  pf<-paste0('./piramides/',p,'-2020.png')
  ggsave(pf,plot=ggp,width = 20, height = 14, dpi = 300, 
         units = "cm", device='png')
}



## -----------------------------------------------------------------------------------------------------------

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 12),
        axis.text.x=element_text(size = 10),
        axis.text.y=element_text(size = 10))

for(p in c("Brazil","Japan","Nigeria","United Kingdom")){
  
  df<-pop%>%
    filter(year %in% seq(1960,2020,20) & region==p)%>%
    group_by(year,region)%>%
    mutate(poptotal=sum(pop))%>%
    ungroup()%>%
    mutate(poprel=pop/poptotal)%>%
    mutate(poprel=if_else(sex=='Male',-poprel,poprel))
  
  ggp<-ggplot(data=df,aes(x=idgr,y=poprel,group=sex,fill=sex))+
    geom_bar(stat='identity')+
    facet_wrap(~year)+
    coord_flip()+
    labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
    ggtitle('Pirâmide Etária',subtitle=paste(p,'- 1960 a 2020'))+
    scale_y_continuous(name='População (%)',
                       labels=label_percent(big.mark = '.',decimal.mark = ','),
                       breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
    scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
    scale_fill_startrek(name='Sexo',label=c('Mulheres','Homens'))+
    th
  
  pf<-paste0('./piramides/',p,'-1960a2020.png')
  ggsave(pf,plot=ggp,width = 20, height = 14, dpi = 300, units = "cm", device='png')
}

#knitr::purl('Descrevendo_dados.Rmd')

