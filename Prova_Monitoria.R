Gapminder<-readRDS('gapminder.rds')

library(ggsci)
library(scales)
library(hrbrthemes)
library("gridExtra")
library(tidyverse)

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

Gapminder_Brics<- Gapminder %>%
  filter((name == 'China'|name == 'Brazil'|name=='South Africa'
         |name=='Russia')&time>=1900)

Gapminder_Varios<- Gapminder %>%
  filter((name == 'Brazil'|name=='Japan'
          |name=='Nigeria'|name == 'United Kingdom')&
           time<=2050)

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text=element_text(size = 8))

Gapminder_Varios<- Gapminder %>%
  filter((name %in%  'Brazil'|name == 'United Kingdom')&
           time<=2050)



df<- Gapminder %>%
  filter(name %in% c("Brazil","Japan","Nigeria","United Kingdom")&
          time<=2050) 


ggtfr<-ggplot(data=df,aes(y=tfr,x=time,color=name))+
  geom_line()+
  ggtitle('Taxa de Fecundidade Total por ano e país')+
  scale_x_continuous(breaks = seq(1800,2100,25))+
  scale_color_startrek()+
  labs(x='Ano', y = 'Filhos por mulher',color='País')+
  th

ggmi<-ggplot(data=df,aes(y=cm,x=time,color=name))+
  geom_line()+
  ggtitle('Mortalitade Infantil por ano e país')+
  scale_color_startrek()+
  scale_x_continuous(breaks = seq(1800,2100,25))+
  labs(x='Ano', y = 'Mortalitade Infantil',color='País',
       caption = "Fonte: Gapminder Foundation")+
  th

ggtfr
ggmi

ggarrange(ggtfr,ggmi,common.legend=T,nrow=2)
ggarr<-grid.arrange(ggtfr, ggmi,
             ncol = 1, nrow = 2)


brazil<-pop%>%
  filter(year==2022 &
           region=='Brazil' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

usa<-pop%>%
  filter(year==2022 &
           region=='United States of America' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

nigeria<-pop%>%
  filter(year==2022 &
           region=='Nigeria' &
           variant %in% c('Medium variant','Estimates'))%>%
  group_by(year)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

paises<-pop%>%
  filter(year==2020 &
           (region=='Russian Federation'| region=='SUB-SAHARAN AFRICA'|
            region=='Central Asia'| region == 'Western Europe'))%>%
  mutate(region=factor(region,
    levels=c('Russian Federation',"SUB-SAHARAN AFRICA","Central Asia","Western Europe"),
    labels=c('Westeros','Kingslanding','Winterfell','Meeren')))%>%
  group_by(year,region)%>%
  mutate(poptotal=sum(pop))%>%
  ungroup()%>%
  mutate(poprel=pop/poptotal)%>%
  mutate(poprel=if_else(sex=='Male',-poprel,poprel))

library(kableExtra)
library(knitr)

# paises%>%
#   filter(region=='Angola')%>%
#   mutate(pop=pop*1000)%>%
#   select(idgr,sex,pop,region)%>%
#   kbl()

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
                      values=c('grey20','grey80'))+
  th


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
  ggsave(pf,plot=ggp,width = 20, height = 14, dpi = 300, units = "cm", device='png')
}

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

ggplot(data=brazil,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária',subtitle=' - 2022')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.1,.1,.04),limits=c(-.1,.1))+
  scale_x_discrete(name='Grupo de Idade')+
  scale_fill_startrek(name='Sexo',label=c('Mulheres','Homens'))+
  th

ggplot(data=usa,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  facet_wrap(~year)+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária - Helheim')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.08,.08,.04),limits=c(-.08,.08))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_startrek(name='Sexo',label=c('Mulheres','Homens'))+
  th

ggplot(data=nigeria,aes(x=idgr,y=poprel,group=sex,fill=sex))+
  geom_bar(stat='identity')+
  coord_flip()+
  facet_wrap(~year)+
  labs(caption = "Fonte: United Nations. World Population Prospects 2019")+
  ggtitle('Pirâmide Etária - Midgard')+
  scale_y_continuous(name='População (%)',
                     labels=label_percent(big.mark = '.',decimal.mark = ','),
                     breaks = seq(-.08,.08,.04),limits=c(-.08,.08))+
  scale_x_discrete(name='Grupo de Idade',breaks=levels(pop$idgr)[seq(1,19,2)])+
  scale_fill_startrek(name='Sexo',label=c('Mulheres','Homens'))+
  th