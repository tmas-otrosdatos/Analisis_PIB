library(tidyverse)
library(readr)
library(ggthemes)
library(plotly)
library(reshape2)

df <- read_csv("Data/PIB_INPC.csv")


df$Trimestre_2 <- as.Date(format(df$Trimestre, "%Y Q%q"))

###########
#Line Plot#
###########

titulo: "PIB Mexico a precios de 2018"
gg <- ggplot(df[41:165,], aes(x = Trimestre, y = PIB, group = 1)) + 
  geom_line(size=1,color="#0AC5A8")+ theme_classic()+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
gg 

ggplotly(gg) %>% 
  layout( 
    xaxis = list(automargin=TRUE), 
    yaxis = list(automargin=TRUE)
  )

###########
#Stacked Bar Plot#
###########
 
df_long <- df[,1:5]
#df_long <- select(df_long, -PIB)
df_long <- df_long %>% 
  drop_na() %>% 
  melt(id=c('Trimestre'))

df_long$variable<- factor(df_long$variable, levels = c("PIB","Sector Terciario", "Sector Secundario", "Sector Primario"))
#df_long$variable<- factor(df_long$variable, levels = c("Sector Terciario", "Sector Secundario", "Sector Primario"))

title = "Evolución del PIB en México por Sectores"
subtitle = 'Impacto COVID'

gg_area <- ggplot(df_long, aes(x = Trimestre, y = value, group=variable, fill = variable)) + 
  geom_area(position='stack', alpha=0.6)+
  theme_classic()+
  scale_fill_brewer(palette = "PuBuGn", direction = -1) +
  geom_line(position = "stack", size = 1, color='white')+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  scale_y_continuous(labels = scales::dollar)+
  labs(title = title , subtitle= subtitle )+
  ylab("Millones de Pesos")+
  guides(fill = guide_legend(title = NULL))+
  annotate("text", x = "1995-T4", y = 9000000, label = "Devaluación 1994", alpha = .8, size = 5, color = 'white')+
  annotate("text", x = "2008-T4", y = 12500000, label = "Crisis 2009", alpha = .8, size = 5, color = 'white')+
  annotate("text", x = "2019-T4", y = 14000000, label = "Covid-19", alpha = .8, size = 5, color = 'white')+
  
  annotate("text", x = "1995-T4", y = 11500000, label = "Caída PIB Total\n -6.3%", alpha = .8, size = 4, color = 'black')+
  annotate("text", x = "2008-T4", y = 15300000, label = "Caída PIB Total\n -2.1%", alpha = .8, size = 4, color = 'black')+
  annotate("text", x = "2019-T2", y = 19100000, label = "Caída PIB Total\n -8.2%", alpha = .8, size = 4, color = 'black')+
  annotate("text", x = "1995-T4", y = 8000000, label =  "Caída de -3.6%", alpha = .8, size = 4, color =  'black')+
  annotate("text", x = "2008-T4", y = 11500000, label = "Caída de -4.1%", alpha = .8, size = 4, color = 'black')+
  annotate("text", x = "2019-T4", y = 13000000, label = "Caída de -7.7%", alpha = .8, size = 4, color = 'black')+
  annotate("text", x = "1995-T4", y = 3300000, label =  "Caída de -10.9%", alpha = .8,size = 4, color = 'black')+
  annotate("text", x = "2008-T4", y = 4500000, label =  "Caída de -7.5%", alpha = .8, size = 4, color = 'black')+
  annotate("text", x = "2019-T4", y = 4500000, label =  "Caída de -10%", alpha = .8,  size = 4, color = 'black')+
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

gg_area



gg_area_plty <- ggplotly(gg_area)

gg_area_plty <- gg_area_plty %>% 
  layout(xaxis = list(autotick = F, dtick = 15))

gg_area_plty

Sys.setenv('plotly_username' = 'valente_quintana')
Sys.setenv('plotly_api_key' = 'tTf2uq3ZXBwt9BWVuiX8')

api_create(gg_area, 'PIB_MX_Area')