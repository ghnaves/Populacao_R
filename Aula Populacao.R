
library(tidyverse)
library(ggpubr)

Gapminder<-readRDS('gapminder.rds')

th<-theme_ipsum()+
  theme(plot.title = element_text(size = 24),
        plot.subtitle = element_text(size=18),
        legend.text = element_text(size = 12),
        axis.text.x=element_text(size = 10),
        axis.text.y=element_text(size = 10))

df<- Gapminder %>%
  filter(name %in% 
           c("China","Brazil","Japan","United Kingdom","Germany","USA")&
           time<=2050) 


ggtfr<-ggplot(data=df,aes(y=tfr,x=time,color=name))+
  geom_line(size=1)+
  ggtitle('Taxa de Fecundidade Total por ano e país')+
  scale_x_continuous(breaks = seq(1800,2100,25))+
  scale_color_startrek()+
  labs(x='Ano', y = 'Filhos por mulher',color='País')+
  th

ggmi<-ggplot(data=df,aes(y=cm,x=time,color=name))+
  geom_line(size=1)+
  ggtitle('Mortalitade Infantil por ano e país')+
  scale_color_startrek()+
  scale_x_continuous(breaks = seq(1800,2100,25))+
  labs(x='Ano', y = 'Mortalitade Infantil',color='País',
       caption = "Fonte: Gapminder Foundation")+
  th

ggarrange(ggtfr,ggmi,common.legend=T,nrow=2)


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