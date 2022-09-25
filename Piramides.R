library(tidyverse)
library(readxl)

col_types <- c("text","text","skip","numeric","text", rep("numeric",23))
colnames<-c('variant','region','cntycode','type','parentcode','year',
            '0-4','5-9','10-14','15-19', '20-24',
            '25-29','30-34','35-39','40-44','45-49',
            '50-54','55-59','60-64','65-69','70-74',
            '75-79','80-84','85-89','90-94','95-99','100+')

est.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                    range='B18:AC3842',
                    sheet='ESTIMATES',
                    col_names = colnames,
                    col_types = col_types)%>%
  mutate(sex='Male')

est.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                      range='B18:AC3842',
                      sheet='ESTIMATES',col_names = colnames,
                      col_types = col_types)%>%
  mutate(sex='Female')

medium.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                       range='B18:AC4352',
                       sheet='MEDIUM VARIANT',col_names = colnames,
                       col_types = col_types)%>%
  mutate(sex='Male')

medium.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                         range='B18:AC4352',
                         sheet='MEDIUM VARIANT',col_names = colnames,
                         col_types = col_types)%>%
  mutate(sex='Female')

high.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                     range='B18:AC4352',
                     sheet='HIGH VARIANT',col_names = colnames,
                     col_types = col_types)%>%
  mutate(sex='Male')

high.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                       range='B18:AC4352',
                       sheet='HIGH VARIANT',col_names = colnames,
                       col_types = col_types)%>%
  mutate(sex='Female')

low.male<-read_xlsx('./DB/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx',
                    range='B18:AC4352',
                    sheet='LOW VARIANT',col_names = colnames,
                    col_types = col_types)%>%
  mutate(sex='Male')

low.female<-read_xlsx('./DB/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx',
                      range='B18:AC4352',
                      sheet='LOW VARIANT',col_names = colnames,
                      col_types = col_types)%>%
  mutate(sex='Female')

pop<-est.male %>%
  rbind(est.female)%>%
  rbind(medium.male)%>%
  rbind(medium.female)%>%
  rbind(low.male)%>%
  rbind(low.female)%>%
  rbind(high.female)%>%
  rbind(high.male)

# dados_wide<-tibble(pais=c('Brasil','Alemanha'),`2002`=c(2,0),`2014`=c(1,7))
# dados_wide
# 
# dados_long<-dados_wide%>%
#   gather(key=ano,value=gols,-pais)
# 
# dados_long
# 
# dados_wide2 <- dados_long %>%
#   spread(key=ano, value=gols) 
# dados_wide2

library(ggsci)
library(scales)
library(hrbrthemes)
library(gridExtra)

pop<-pop %>%
  gather(key=idgr,value=pop,
         -c('variant','region','cntycode','type','parentcode','year','sex'))%>%
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

paises<-pop%>%
  filter(year==2025 &
           (region=='EUROPE'
            | region=='SUB-SAHARAN AFRICA'
            | region=='CENTRAL AND SOUTHERN ASIA'
            | region == 'NORTHERN AMERICA') &
           variant=='Medium variant')%>%
  mutate(region=factor(region,
                       levels=c("EUROPE",
                                "SUB-SAHARAN AFRICA",
                                "CENTRAL AND SOUTHERN ASIA",
                                "NORTHERN AMERICA"),
                       #labels=c('Midgard','Vanaheim','Helheim','Asgard')))%>%
                       #labels=c('Mars','Saturn','Venus','Moon')))%>%
                       labels=c('Kronos','Vulcano','Romulo','Cardassia')))%>%
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

if (interactive()) {
p<-ggplot(paises) +
  aes(x = idgr, y = poprel, fill = sex) +
  geom_col() +
  scale_fill_manual(values = c(Female = "#F6C7E3", 
                               Male = "#8F8EFF")) +
  coord_flip() +
  theme_minimal() +
  facet_wrap(vars(region))

ggplot_to_ppt("p")
}



