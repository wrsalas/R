# CODIGO PARA EL ANALISIS DE DATOS PARA EL INFORME DE REVISION POR LA DIRECCION
# Sistema de Vigilancia de Calidad del Aire de Santiago de Cali
# Fecha 2021/07/12 V.1

#### 0 Librerias ####


library("plyr")
library("tidyverse")
library("openair")
library("scales")
library("lattice")
library("RColorBrewer")
library("ggnewscale")
library("grDevices")
library("lubridate")
library("janitor")
library("ggridges")
library("timetk")
library("lazyeval")

library("agricolae")


#### 0.1 cargue de datos####


data.2021 <- readRDS("data_2021.RDS")
data.2021  <-  data.2021$data_mediciones



#### 0.2 Filtros, etiquetas, colores, etc ####

filter.param <- c("pm25","pm10","o3","so2",
                 "temperatura","humedad","presion",
                 "lluvia","radiacion_solar","velocidad_viento","direccion_viento")

filter.cont <- c("pm25","pm10","o3","so2")
filter.meteo <- c("temperatura","humedad","presion",
                  "lluvia","radiacion_solar","velocidad_viento","direccion_viento")


col.est <- c("base_aerea"="#543005",
             "canaveralejo"="#8c510a",
             "compartir"="#bf812d",
             "era_obrero"="#f6e8c3",
             "ermita"="#f5f5f5",
             "flora"="#80cdc1",
             "pance"="#35978f",
             "transitoria"="#01665e",
             "univalle"="#003c30")


col.est2 <- c("base_aerea"="#a63603",
             "canaveralejo"="#e6550d",
             "compartir"="#fd8d3c",
             "era_obrero"="#fdbe85",
             "ermita"="#f7f7f7",
             "flora"="#92c5de",
             "pance"="#4393c3",
             "transitoria"="#2166ac",
             "univalle"="#053061")

lab.est <- c("base_aerea"="Base Aerea",
             "canaveralejo"="Cañaveralejo",
             "compartir"="Compartir",
             "era_obrero"="ERA-Obrero",
             "ermita"="Ermita",
             "flora"="Flora",
             "pance"="Pance",
             "transitoria"="Transitoria",
             "univalle"="Univalle")

lab.hora <- c("0" = "0","1" = "","2" = "2","3" = "",
              "4" = "4","5" = "","6" = "6","7" = "",
              "8" = "8","9" = "","10" = "10","11" = "",
              "12" = "12","13" = "","14" = "14","15" = "",
              "16" = "16","17" = "","18" = "18","19" = "",
              "20" = "20","21" = "","22" = "22","23" = "")

lab.hora2 <- c("0" = "0","4" = "4",
               "8" = "8","12" = "12",
               "16" = "16","20" = "20")

lab.hora3 <- c("0" = "0","6" = "6",
               "12" = "12","18" = "18")

#### 0.3 funciones ####

#### 0.3.1 media movil ####

f.media_movil <- function(bd_df){
  
  stat_movil_tbl <- bd_df %>% 
    group_by(estacion, parametro) %>%
    mutate(conteo_movil = case_when(
      parametro %in% c("pm10", "pm25", "so2") ~ slidify_vec(medicion_val, .f = ~ sum(!is.na(.x))/24*100,
                                                           .period = 24, .align = 'right'),
      parametro == "o3"                       ~ slidify_vec(medicion_val, .f = ~ sum(!is.na(.x))/8*100,
                                                           .period = 8, .align = 'right'),
      TRUE ~ NaN
    )) %>% 
    mutate(media_movil = case_when(
      parametro %in% c("pm10", "pm25", "so2") ~ if_else(conteo_movil >= 75, 
                                                       slidify_vec(medicion_val, .f = mean, na.rm = TRUE, 
                                                                   .period = 24, .align = 'right'), NaN),
      parametro == "o3"                       ~ if_else(conteo_movil >= 75, 
                                                       slidify_vec(medicion_val, .f = mean, na.rm = TRUE, 
                                                                   .period = 8, .align = 'right'), NaN),
      TRUE ~ NaN
    ))
  
  return(stat_movil_tbl)
  
}



#### 0.3.2  ICA ####

f.ICA <- function(df){
  
  ICA_tbl <- read_csv("ICA_tbl.csv")
  
  
  ICA_datos_tbl <- df %>%  
    inner_join(ICA_tbl) %>% 
    mutate(ica_flag = case_when(
      parametro %in% c("pm10", "pm25", "o3") ~ if_else((PC_B <= round(media_movil))&(round(media_movil) <= PC_A), 1, 0),
      parametro %in% c("so2", "no2") ~ if_else((PC_B <= round(medicion_val))&(round(medicion_val) <= PC_A), 1, 0),
      TRUE ~ NaN
    )) %>% 
    filter(ica_flag == 1) %>% 
    mutate(ICA = case_when(
      parametro %in% c("pm10", "pm25", "o3") ~ ((I_A - I_B)/(PC_A - PC_B))*(media_movil - PC_B) + I_B,
      parametro %in% c("so2", "no2") ~ ((I_A - I_B)/(PC_A - PC_B))*(medicion_val - PC_B) + I_B)
    ) %>% 
    arrange(fecha) %>% 
    select(fecha, estacion, parametro, ICA)
  
  return(df %>% 
           left_join(ICA_datos_tbl))
  
}


##### 0.3.3 Exc ####

limite <- tibble(parametro = c("pm10","pm25","so2","o3","no2"),
                 limite = c(75,37,100,100,200))


f.excedencia <- function(df){
  
  exc_datos_tbl <- df %>%  
    inner_join(limite) %>% 
    mutate(exc_flag = case_when(
      parametro %in% c("pm10", "pm25","so2", "o3","no2") ~ if_else(media_movil > limite, 1, 0),
      TRUE ~ NaN
    )) %>% 

    arrange(fecha)
  
  return((exc_datos_tbl))
  
}


#### 0.4 creacion DF ####

#### 0.4.1 DF global ####

colnames(data.2021) <- c("fecha","estacion","parametro","medicion_clean","medicion_val")
data.2021 <- data.2021 %>% select("fecha","estacion","parametro","medicion_val")

data.2021 <- data.2021 %>% filter (parametro %in% filter.param) %>% arrange(fecha,estacion)

data.2021 <- data.2021 %>% f.media_movil() %>% f.ICA()

#limpieza valores negativos
data.2021 <- data.2021 %>% mutate(ifelse (ICA <= 0,0,ICA))

colnames(data.2021) <- c("fecha","estacion","parametro","medicion_val","conteo_movil","media_movil","pre_ica","ICA")

data.2021 <- data.2021 %>% select(fecha,estacion,parametro,medicion_val,media_movil,ICA)

data.2021 <- data.2021 %>% mutate(categoria = case_when(
  (round(ICA) >= 0) & (round(ICA) <= 50) ~ "Buena",
  (round(ICA) >= 51) & (round(ICA) <= 100) ~ "Aceptable",
  (round(ICA) >= 101) & (round(ICA) <= 150) ~ "Danina a grupos sensibles",
  (round(ICA) >= 151) & (round(ICA) <= 200) ~ "Danina a la salud",
  (round(ICA) >= 201) & (round(ICA) <= 300) ~ "Muy danina a la salud",
  (round(ICA) >= 301) & (round(ICA) <= 500) ~ "Peligrosa")) %>% 
  mutate(categoria = factor(categoria, 
                            levels = c('Buena', 'Aceptable', 'Danina a grupos sensibles',
                                       'Danina a la salud', 'Muy danina a la salud', 'Peligrosa')))

colnames(data.2021) <- c("fecha","estacion","parametro","medicion","media_movil","ICA","categoria")
data.2021$media_movil[is.na(data.2021$media_movil)] <- NA
data.2021$ICA[is.na(data.2021$ICA)] <- NA

data.2021 <- data.2021 %>% f.excedencia()

data.2021$fecha <- c(data.2021$fecha - seconds(1))

data.2021$a <- year(data.2021$fecha)
data.2021$m <- month(data.2021$fecha)
data.2021$d <- day(data.2021$fecha)
data.2021$h <- hour(data.2021$fecha)

#### 0.4.2 DF contador ICA ####

data.2021.ica <- data.2021 %>% filter(parametro %in% filter.cont) %>% 
                               arrange(fecha,estacion)  %>% select(fecha,estacion,parametro,ICA) %>% 
                               pivot_wider(names_from = c(estacion,parametro),values_from = ICA)


data.2021.ica$ba <- pmax(data.2021.ica$base_aerea_o3,data.2021.ica$base_aerea_pm25,data.2021.ica$base_aerea_so2, na.rm = TRUE)
data.2021.ica$ca <- pmax(data.2021.ica$canaveralejo_pm25,data.2021.ica$canaveralejo_pm10,data.2021.ica$canaveralejo_so2, na.rm = TRUE)
data.2021.ica$co <- pmax(data.2021.ica$compartir_o3,data.2021.ica$compartir_pm10,data.2021.ica$compartir_pm25, na.rm = TRUE)
data.2021.ica$era <- pmax(data.2021.ica$era_obrero_o3,data.2021.ica$era_obrero_pm10,data.2021.ica$era_obrero_pm25, na.rm = TRUE)
data.2021.ica$erm <- pmax(data.2021.ica$ermita_pm25,data.2021.ica$ermita_pm10,data.2021.ica$ermita_so2, na.rm = TRUE)
data.2021.ica$flo <- pmax(data.2021.ica$flora_o3,data.2021.ica$flora_pm10,data.2021.ica$flora_pm25,data.2021.ica$flora_so2, na.rm = TRUE)
data.2021.ica$pa <- pmax(data.2021.ica$pance_o3,data.2021.ica$pance_pm10,data.2021.ica$pance_pm25, na.rm = TRUE)
data.2021.ica$t <- pmax(data.2021.ica$transitoria_o3,data.2021.ica$transitoria_pm10,data.2021.ica$transitoria_pm25,data.2021.ica$transitoria_so2, na.rm = TRUE)
data.2021.ica$uv <- pmax(data.2021.ica$univalle_o3,data.2021.ica$univalle_pm10,data.2021.ica$univalle_pm25, na.rm = TRUE)

data.2021.ica <- data.2021.ica %>% select(fecha,ba,ca,co,era,erm,flo,pa,t,uv)

data.2021.ica <- data.2021.ica %>% pivot_longer(!fecha, names_to = "est", values_to = "ICA")

data.2021.ica <- data.2021.ica %>% mutate(estacion = ifelse(est=="ba","base_aerea",
                                                     ifelse(est=="ca","canaveralejo",
                                                     ifelse(est=="co","compartir",
                                                     ifelse(est=="era","era_obrero",
                                                     ifelse(est=="erm","ermita",
                                                     ifelse(est=="flo","flora",
                                                     ifelse(est=="pa","pance",
                                                     ifelse(est=="t","transitoria","univalle")))))))))

data.2021.ica <- data.2021.ica %>% mutate(categoria = case_when(
  (round(ICA) >= 0) & (round(ICA) <= 50) ~ "Buena",
  (round(ICA) >= 51) & (round(ICA) <= 100) ~ "Aceptable",
  (round(ICA) >= 101) & (round(ICA) <= 150) ~ "Danina a grupos sensibles",
  (round(ICA) >= 151) & (round(ICA) <= 200) ~ "Danina a la salud",
  (round(ICA) >= 201) & (round(ICA) <= 300) ~ "Muy danina a la salud",
  (round(ICA) >= 301) & (round(ICA) <= 500) ~ "Peligrosa")) %>% 
  mutate(categoria = factor(categoria, 
                            levels = c('Buena', 'Aceptable', 'Danina a grupos sensibles',
                                       'Danina a la salud', 'Muy danina a la salud', 'Peligrosa')))

data.2021.ica <- data.2021.ica %>% select(fecha,estacion,ICA,categoria)

#limpieza sin datos
levels <- levels(data.2021.ica$categoria)
levels[length(levels) + 1] <- "SD"

data.2021.ica$categoria <- factor(data.2021.ica$categoria, levels = levels)
data.2021.ica$categoria[is.na(data.2021.ica$categoria)] <- "SD"

data.2021.ica <- data.2021.ica %>% filter(categoria != "SD")

#### 0.4.3 otros ####

resumen_ica <- data.2021.ica %>% 
  mutate(ano = year(fecha)) %>% 
  group_by(ano,estacion) %>% 
  count(categoria) %>%
  arrange(categoria)



data.2021.mes <- data.2021 %>% pivot_longer(cols=c(medicion,media_movil,ICA),names_to = "tipo",values_to = "valor")  %>%
                               group_by(estacion,parametro,tipo,m) %>% 
                              summarise(media = mean(valor,na.rm=TRUE),
                                        quantile = scales::percent(c(0.05,0.25,0.5,0.75,0.95)),
                                        p = quantile(valor,c(0.05,0.25,0.5,0.75,0.95),na.rm=TRUE))

data.2021.mes <- data.2021.mes %>% pivot_wider(names_from = c(quantile),values_from = p)

data.2021.mes <- data.2021.mes %>% pivot_longer(cols=c(media,'5%','25%','50%','75%','95%'),names_to = "estadistico",values_to = "valor")
data.2021.mes.l <- data.2021.mes %>% pivot_wider(names_from = estadistico , values_from = valor)
data.2021.mes.l$media[is.na(data.2021.mes.l$media)] <- NA
data.2021.mes.l <- clean_names(data.2021.mes.l)

##### ica estacion mes ####

data.2021.ica$a <- year(data.2021.ica$fecha)
data.2021.ica$m <- month(data.2021.ica$fecha)
data.2021.ica$d <- day(data.2021.ica$fecha)
data.2021.ica$h <- hour(data.2021.ica$fecha)


data.2021.ica.mes <- data.2021.ica %>% pivot_longer(cols=c(ICA),names_to = "tipo",values_to = "valor")  %>%
  group_by(estacion,tipo,m) %>% 
  summarise(media = mean(valor,na.rm=TRUE),
            quantile = scales::percent(c(0.05,0.25,0.5,0.75,0.95)),
            p = quantile(valor,c(0.05,0.25,0.5,0.75,0.95),na.rm=TRUE))

data.2021.ica.mes <- data.2021.ica.mes %>% pivot_wider(names_from = c(quantile),values_from = p)

data.2021.ica.mes <- data.2021.ica.mes %>% pivot_longer(cols=c(media,'5%','25%','50%','75%','95%'),names_to = "estadistico",values_to = "valor")
data.2021.ica.mes.l <- data.2021.ica.mes %>% pivot_wider(names_from = estadistico , values_from = valor)
data.2021.ica.mes.l$media[is.na(data.2021.ica.mes.l$media)] <- NA
data.2021.ica.mes.l <- clean_names(data.2021.ica.mes.l)


write_excel_csv2(data.2021.ica.mes.l,"ica.csv")

#### ica

data.2021.hora <- data.2021 %>% pivot_longer(cols=c(medicion,media_movil,ICA),names_to = "tipo",values_to = "valor")  %>%
  group_by(estacion,parametro,tipo,h) %>% 
  summarise(media = mean(valor,na.rm=TRUE),
            quantile = scales::percent(c(0.05,0.25,0.5,0.75,0.95)),
            p = quantile(valor,c(0.05,0.25,0.5,0.75,0.95),na.rm=TRUE))

data.2021.hora <- data.2021.hora %>% pivot_wider(names_from = c(quantile),values_from = p)

data.2021.hora <- data.2021.hora %>% pivot_longer(cols=c(media,'5%','25%','50%','75%','95%'),names_to = "estadistico",values_to = "valor")
data.2021.hora.l <- data.2021.hora %>% pivot_wider(names_from = estadistico , values_from = valor)
data.2021.hora.l$media[is.na(data.2021.hora.l$media)] <- NA
data.2021.hora.l <- clean_names(data.2021.hora.l)


##### 1 Analisis contaminantes ####
##### 1.0 ####
f0.1 <- ggplot(data.2021 %>% filter(parametro=="pm25"),
             aes(as.factor(estacion),medicion))+
  geom_bar(fill="#0070c0",color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  #geom_bar(data=data.2021 %>% filter(parametro=="pm25",year(fecha)==2019,estacion=="univalle"),
           #fill="#0070c0",color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  #geom_bar(aes(fill=Estacion2),alpha=0.5,stat="summary",fun.y = "mean",show.legend = FALSE)+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim = c(0,35))+
  geom_hline(aes(yintercept = 25,lty="1 Res. 2254"),color=c("#000000"),lwd=1.5)+
  scale_linetype_manual(name="Limites normativos",values=c(1),
                        labels=c(expression(paste("Res. 2254 de 2017: 25 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  #scale_fill_manual(values=c(col.est))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  ylab(expression(paste("Promedio Anual PM"[2.5]," (",mu,"g/", m^3,")")))+
  facet_grid(.~year(fecha))+
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f0.1.png",plot = f0.1,device = "png",scale = 1.4,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  



f0.2 <- ggplot(data.2021 %>% filter(parametro=="pm10"),aes(as.factor(estacion),medicion))+
  geom_bar(fill="#0070c0",color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  #geom_bar(data=data.2021 %>% filter(parametro=="pm10",
  #                                   year(fecha)==c(2018),
  #                                   estacion %in% c("era_obrero")),
  #         fill="#d73027",color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  #geom_bar(data=data.2021 %>% filter(parametro=="pm10",
  #                                   year(fecha)==c(2019,2020),
  #                                  estacion %in% c("era_obrero","compartir")),
  #       fill="#d73027",color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  #geom_bar(aes(fill=Estacion2),alpha=0.5,stat="summary",fun.y = "mean",show.legend = FALSE)+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim = c(0,65))+
  geom_hline(aes(yintercept = 50,lty="1 Res. 2254"),color=c("#000000"),lwd=1.5)+
  scale_linetype_manual(name="Limites normativos",values=c(1),
                        labels=c(expression(paste("Res. 2254 de 2017: 50 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  #scale_fill_manual(values=c(col.est))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  ylab(expression(paste("Promedio Anual PM"[10]," (",mu,"g/", m^3,")")))+
  facet_grid(.~year(fecha))+
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))  

ggsave("f0.2.png",plot = f0.2,device = "png",scale = 1.4,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  



##### 1.1 ####

f1.1 <- ggplot(data.2021 %>% filter(parametro=="pm25"),
               aes(as.factor(estacion),medicion))+
  geom_bar(aes(fill=estacion),color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  scale_fill_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 25,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  geom_hline(aes(yintercept = 15,lty="2 Meta 2030"),color=c("#000000"),size=1)+
  geom_hline(aes(yintercept = 10,lty="3 Objetivo 4 OMS"),color=c("#000000"),size=1)+
  geom_hline(aes(yintercept = 5, lty="4 Guia OMS"),color=c("#000000"),size=1)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limites normativos",values=c("F1","44","22","11"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 25 ",mu,"g/", m^3)),
                                 bquote(paste("Meta 2030: 15 ",mu,"g/", m^3)),
                                 bquote(paste("Objetivo Int. 4 OMS: 10 ",mu,"g/", m^3)),
                                 bquote(paste("Guia Calidad de Aire OMS 2021 5: ",mu,"g/",m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+

  coord_cartesian(ylim = c(0,35))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Promedio Anual PM"[2.5]," (",mu,"g/", m^3,")")))+
  
  facet_grid(.~year(fecha))+
  guides(lty=guide_legend(override.aes = list(size = c(1.5,1,1,1)),
                          nrow=2,byrow=TRUE),size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f1.1.png",plot = f1.1,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  



ggplot(data.2021 %>% filter(parametro=="pm10"),
               aes(as.factor(estacion),medicion))+
  
  geom_bar(aes(fill=estacion),color="black",stat="summary",fun.y = "mean",show.legend = FALSE, width = 0.5)+
  scale_fill_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 50,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  geom_hline(aes(yintercept = 30,lty="2 Meta 2030"),color=c("#000000"),size=1)+
  geom_hline(aes(yintercept = 20,lty="3 Objetivo 4 OMS"),color=c("#000000"),size=1)+
  geom_hline(aes(yintercept = 15, lty="4 Guia OMS"),color=c("#000000"),size=1)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limites normativos",values=c("F1","44","22","11"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 50 ",mu,"g/", m^3)),
                                 bquote(paste("Meta 2030: 30 ",mu,"g/", m^3)),
                                 bquote(paste("Objetivo Int. 4 OMS: 20 ",mu,"g/", m^3)),
                                 bquote(paste("Guia Calidad de Aire OMS 2021 15: ",mu,"g/",m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,65))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Promedio Anual PM"[10]," (",mu,"g/", m^3,")")))+
  
  facet_grid(.~year(fecha))+
  guides(lty=guide_legend(override.aes = list(size = c(1.5,1,1,1)),
                          nrow=2,byrow=TRUE),size=FALSE)+

  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0")) 

ggsave("f1.2.png",plot = f1.2,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  


##### 1.2 ####


f2.1 <- ggplot(data.2021 %>% filter(parametro=="pm25"),
               aes(as.factor(estacion),media_movil))+
  stat_boxplot(geom = "errorbar", width = 0.2,color="black",show.legend = FALSE)+
  geom_boxplot(aes(fill=estacion),color="black",outlier.size = .8,show.legend = FALSE, width = 0.5)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.95)},show.legend = FALSE,size=0.8,pch=3)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.9)},show.legend = FALSE,size=0.8,pch=3)+
  
  scale_fill_manual(values=c(col.est2))+
  scale_color_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 37,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  geom_hline(aes(yintercept = 0,lty="2 blanco"),color=c("#ffffff"),size=0)+
  geom_hline(aes(yintercept = 25,lty="3 Objetivo 4 OMS"),color=c("#000000"),size=1)+
  geom_hline(aes(yintercept = 15,lty="3 Guia OMS"),color=c("#000000"),size=1)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limites normativos",values=c("F1","1F","11","44"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 37 ",mu,"g/", m^3)),
                                 "",
                                 bquote(paste("Objetivo Int. 4 OMS: 25 ",mu,"g/", m^3)),
                                 bquote(paste("Guia Calidad de Aire OMS 2021 15: ",mu,"g/",m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000","#ffffff","#000000","#000000"))))+
  
  coord_cartesian(ylim = c(0,45))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 24h PM"[2.5]," (",mu,"g/", m^3,")")))+
  
  facet_grid(.~year(fecha))+
  guides(color=FALSE,
         lty=guide_legend(override.aes = list(size = c(1.5,0,1,1)),
                          nrow=2,byrow=TRUE),
         size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f2.1.png",plot = f2.1,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  



f2.2 <- ggplot(data.2021 %>% filter(parametro=="pm10"),
               aes(as.factor(estacion),media_movil))+
  
  stat_boxplot(geom = "errorbar", width = 0.2,color="black",show.legend = FALSE)+
  geom_boxplot(aes(fill=estacion),color="black",outlier.size = .8,show.legend = FALSE, width = 0.5)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.95)},show.legend = FALSE,size=0.8,pch=3)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.9)},show.legend = FALSE,size=0.8,pch=3)+
  
  scale_fill_manual(values=c(col.est2))+
  scale_color_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 75,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  geom_hline(aes(yintercept = 0,lty="2 blanco"),color=c("#ffffff"),size=0)+
  geom_hline(aes(yintercept = 50,lty="3 Objetivo 4 OMS"),color=c("#000000"),size=1)+
  geom_hline(aes(yintercept = 45,lty="3 Guia OMS"),color=c("#000000"),size=1)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limites normativos",values=c("F1","1F","44","22"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 75 ",mu,"g/", m^3)),
                                 "",
                                 bquote(paste("Objetivo Int. 4 OMS: 50 ",mu,"g/", m^3)),
                                 bquote(paste("Guia Calidad de Aire OMS 2021 45: ",mu,"g/",m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000","#ffffff","#000000","#000000"))))+
  
  coord_cartesian(ylim = c(0,115))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 24h PM"[10]," (",mu,"g/", m^3,")")))+
  
  facet_grid(.~year(fecha))+
  guides(color=FALSE,
         lty=guide_legend(override.aes = list(size = c(1.5,0,1,1)),
                          nrow=2,byrow=TRUE),
         size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f2.2.png",plot = f2.2,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE) 



##### 1.3 ####


f3.1 <- ggplot(data.2021.mes %>% filter(m < 12, parametro == "pm25",tipo == "media_movil"),
       aes(fill = estacion, color = estacion))+
  
  geom_ribbon(data = data.2021.mes.l %>% filter(m < 12, parametro == "pm25", tipo == "media_movil"),
              aes( x = m, ymin= x5_percent, ymax = x95_percent, fill = estacion), alpha = 0.5, show.legend = FALSE)+
  
  geom_point(data = data.2021.mes  %>% filter(m < 12, parametro == "pm25",tipo == "media_movil",estadistico %in% c("media")),
             aes(fill = estacion, color = estacion, x = m, y = valor), size = 2, color = "#000000", shape = 21)+
  
  geom_line(data = data.2021.mes  %>% filter(m < 12,parametro == "pm25",tipo == "media_movil",estadistico %in% c("media")),
            aes(color = estacion, group = 1, x = m, y = valor), show.legend = FALSE)+
  geom_line(data = data.2021.mes  %>% filter(m < 12, parametro == "pm25",tipo == "media_movil",estadistico %in% c("5%")),
            aes(group = 1, x = m, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  geom_line(data = data.2021.mes  %>% filter(m < 12, parametro == "pm25",tipo == "media_movil",estadistico %in% c("95%")),
            aes(group = 1, x = m, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  
  geom_hline(aes(yintercept = 37, lty = "2"), lwd = 1.05, color = "#000000")+
  
  #si la leyenda es toda horizontal
  #geom_hline(aes(yintercept = 100, lty = "3"), alpha = 0, lwd = 0, color = "#ffffff")+
  #geom_hline(aes(yintercept = 100, lty = "1"), alpha = 0, lwd = 0, color = "#ffffff")+
  #scale_linetype_manual(name="Limite normativo",values=c("blank","F1","blank"),
  #                      labels=c("",
  #                               bquote(paste("Res. 2254 de 2017: 75 ",mu,"g/", m^3)),
  #                               ""),
  #                      guide=guide_legend(override.aes = list(color=c("#ffffff","#000000","#ffffff"))))+
  
  scale_color_manual(values=c(col.est2))+
  scale_fill_manual(values=c(col.est2), labels = lab.est,
                    bquote(Promedio~Mensual~Media~Movil~"24h"~PM[2.5] ~ (mu*g/m^{-3})))+
  scale_linetype_manual(bquote("Limite normativo"~phantom(0)^{phantom(0)}),values=c("F1"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 37 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,52))+
  scale_x_continuous("Meses", breaks = seq(1, 11, 1))+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 24h PM"[2.5]," (",mu,"g/", m^3,")")))+
  facet_wrap(nrow = 3, .~estacion, labeller = labeller(estacion=lab.est))+
  
  guides(fill = guide_legend(title.position="top",
                             order = 1,
                             override.aes = list(size = 3),
                             nrow = 3, byrow = TRUE),
         #lty = guide_legend(title.position="top",
         #                  override.aes = list(color=c("#ffffff","#000000","#ffffff")),
         #                  nrow = 3))+
         lty = guide_legend(title.position="top",
                            order = 2,
                            override.aes = list(color=c("#000000"))))+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(5,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.box.background = element_blank(),
          legend.box.just = "top",
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.margin = margin(c(0,0,0,0),"point"),
          legend.spacing.y = unit(1, "point"),
          legend.key.height = unit(0, "point"),
          legend.key.width = unit(15,"point"),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          #legend.box = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=10),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f3.1.png",plot = f3.1,device = "png",scale = 1.4,width = 15,height = 15,units = c("cm"),dpi = 300,limitsize = TRUE) 


f3.2 <- ggplot(data.2021.mes %>% filter(m < 12, parametro == "pm10",tipo == "media_movil"),
               aes(fill = estacion, color = estacion))+
  
  geom_ribbon(data = data.2021.mes.l %>% filter(m < 12, parametro == "pm10", tipo == "media_movil"),
              aes( x = m, ymin= x5_percent, ymax = x95_percent, fill = estacion), alpha = 0.5, show.legend = FALSE)+
  
  geom_point(data = data.2021.mes  %>% filter(m < 12, parametro == "pm10",tipo == "media_movil",estadistico %in% c("media")),
             aes(fill = estacion, color = estacion, x = m, y = valor), size = 2, color = "#000000", shape = 21)+
  
  geom_line(data = data.2021.mes  %>% filter(m < 12,parametro == "pm10",tipo == "media_movil",estadistico %in% c("media")),
            aes(color = estacion, group = 1, x = m, y = valor), show.legend = FALSE)+
  geom_line(data = data.2021.mes  %>% filter(m < 12, parametro == "pm10",tipo == "media_movil",estadistico %in% c("5%")),
            aes(group = 1, x = m, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  geom_line(data = data.2021.mes  %>% filter(m < 12, parametro == "pm10",tipo == "media_movil",estadistico %in% c("95%")),
            aes(group = 1, x = m, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  
  geom_hline(aes(yintercept = 75, lty = "2"), lwd = 1.05, color = "#000000")+
  
  #si la leyenda es toda horizontal
  #geom_hline(aes(yintercept = 100, lty = "3"), alpha = 0, lwd = 0, color = "#ffffff")+
  #geom_hline(aes(yintercept = 100, lty = "1"), alpha = 0, lwd = 0, color = "#ffffff")+
  #scale_linetype_manual(name="Limite normativo",values=c("blank","F1","blank"),
  #                      labels=c("",
  #                               bquote(paste("Res. 2254 de 2017: 75 ",mu,"g/", m^3)),
  #                               ""),
  #                      guide=guide_legend(override.aes = list(color=c("#ffffff","#000000","#ffffff"))))+
  
  scale_color_manual(values=c(col.est2))+
  scale_fill_manual(values=c(col.est2), labels = lab.est,
                    bquote(Promedio~Mensual~Media~Movil~"24h"~PM[10] ~ (mu*g/m^{-3})))+
  scale_linetype_manual(bquote("Limite normativo"~phantom(0)^{phantom(0)}),values=c("F1"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 75 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,102))+
  scale_x_continuous("Meses", breaks = seq(1, 11, 1))+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 24h PM"[10]," (",mu,"g/", m^3,")")))+
  facet_wrap(nrow = 2, .~estacion, labeller = labeller(estacion=lab.est))+
  
  guides(fill = guide_legend(title.position="top",
                             order = 1,
                             override.aes = list(size = 3),
                             nrow = 3, byrow = TRUE),
         #lty = guide_legend(title.position="top",
         #                  override.aes = list(color=c("#ffffff","#000000","#ffffff")),
         #                  nrow = 3))+
         lty = guide_legend(title.position="top",
                            order = 2,
                            override.aes = list(color=c("#000000"))))+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(5,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.box.background = element_blank(),
          legend.box.just = "top",
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.margin = margin(c(0,0,0,0),"point"),
          legend.spacing.y = unit(1, "point"),
          legend.key.height = unit(0, "point"),
          legend.key.width = unit(15,"point"),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          #legend.box = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=10),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f3.2.png",plot = f3.2,device = "png",scale = 1.4,width = 15,height = 15,units = c("cm"),dpi = 300,limitsize = TRUE) 


##### 1.4 ####


##### ANOVAS #####

anova.pm25.h.ba <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("base_aerea"),parametro %in% c("pm25")))
anova.pm25.h.ca <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("canaveralejo"),parametro %in% c("pm25")))
anova.pm25.h.co <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("compartir"),parametro %in% c("pm25")))
anova.pm25.h.era <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("era_obrero"),parametro %in% c("pm25")))
anova.pm25.h.er <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("ermita"),parametro %in% c("pm25")))
anova.pm25.h.flo <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("flora"),parametro %in% c("pm25")))
anova.pm25.h.pa <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("pance"),parametro %in% c("pm25")))
anova.pm25.h.t <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("transitoria"),parametro %in% c("pm25")))
anova.pm25.h.uv <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("univalle"),parametro %in% c("pm25")))

summary(anova.pm25.h.ba)
duncan.test(anova.pm25.h.ba,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.ca)
duncan.test(anova.pm25.h.ca,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.co)
duncan.test(anova.pm25.h.co,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.era)
duncan.test(anova.pm25.h.era,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.er)
duncan.test(anova.pm25.h.er,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.flo)
duncan.test(anova.pm25.h.flo,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.pa)
duncan.test(anova.pm25.h.pa,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.t)
duncan.test(anova.pm25.h.t,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm25.h.uv)
duncan.test(anova.pm25.h.uv,"h",group=TRUE,console=TRUE,duncan)


anova.pm10.h.ca <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("canaveralejo"),parametro %in% c("pm10")))
anova.pm10.h.co <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("compartir"),parametro %in% c("pm10")))
anova.pm10.h.era <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("era_obrero"),parametro %in% c("pm10")))
anova.pm10.h.er <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("ermita"),parametro %in% c("pm10")))
anova.pm10.h.flo <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("flora"),parametro %in% c("pm10")))
anova.pm10.h.pa <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("pance"),parametro %in% c("pm10")))
anova.pm10.h.uv <- aov(medicion~h, data=data.2021 %>% filter(estacion %in% c("univalle"),parametro %in% c("pm10")))


summary(anova.pm10.h.ca)
duncan.test(anova.pm10.h.ca,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm10.h.co)
duncan.test(anova.pm10.h.co,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm10.h.era)
duncan.test(anova.pm10.h.era,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm10.h.er)
duncan.test(anova.pm10.h.er,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm10.h.flo)
duncan.test(anova.pm10.h.flo,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm10.h.pa)
duncan.test(anova.pm10.h.pa,"h",group=TRUE,console=TRUE,duncan)
summary(anova.pm10.h.uv)
duncan.test(anova.pm10.h.uv,"h",group=TRUE,console=TRUE,duncan)


##### grafica #####

f4.1 <- ggplot(data.2021.hora %>% filter( parametro == "pm25",tipo == "medicion"),
               aes())+
  
  geom_ribbon(data = data.2021.hora.l %>% filter( parametro == "pm25", tipo == "medicion"),
              aes(x = h, ymin= x5_percent, ymax = x95_percent), fill = "grey50", alpha = 0.5, show.legend = FALSE)+
  
  geom_line(data = data.2021.hora  %>% filter(parametro == "pm25",tipo == "medicion",estadistico %in% c("media")),
            aes(group = 1, x = h, y = valor), color = "#000000",show.legend = FALSE)+
  
  geom_point(data = data.2021.hora  %>% filter( parametro == "pm25",tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "grey50", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("base_aerea","canaveralejo"), parametro == "pm25", h %in% c(6,7,8,9,10),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("compartir"), parametro == "pm25", h %in% c(5,6,7,8,19,20),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("era_obrero"), parametro == "pm25", h %in% c(6,7,8,9,10,11),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("ermita"), parametro == "pm25", h %in% c(5,6,7,8,9,10,11,12,13,14),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("flora"), parametro == "pm25", h %in% c(6,7,8,9,10,11,12),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("pance"), parametro == "pm25", h %in% c(8,9,10,13,14),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("transitoria"), parametro == "pm25", h %in% c(5,6,7,8),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("univalle"), parametro == "pm25", h %in% c(8,9,10,11,12,13,14),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  
  

  geom_line(data = data.2021.hora  %>% filter( parametro == "pm25",tipo == "medicion",estadistico %in% c("5%")),
            aes(group = 1, x = h, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  geom_line(data = data.2021.hora  %>% filter( parametro == "pm25",tipo == "medicion",estadistico %in% c("95%")),
            aes(group = 1, x = h, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  
  scale_color_manual(values=c(col.est2))+
  scale_fill_manual(values=c(col.est2), labels = lab.est,
                    bquote(Concentracion~Horaria~Promedio~PM[2.5] ~ (mu*g/m^{-3})))+

  coord_cartesian(ylim = c(0,62))+
  scale_x_continuous("Horas", breaks = seq(0, 23, 4),
                     labels = lab.hora2)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Concentracion Horaria PM"[2.5]," (",mu,"g/", m^3,")")))+
  facet_wrap(nrow = 3, .~estacion, labeller = labeller(estacion=lab.est))+
  
  guides(fill = guide_legend(title.position="top",
                             order = 1,
                             override.aes = list(size = 3),
                             nrow = 1, byrow = TRUE))+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(5,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.box.background = element_blank(),
          legend.box.just = "top",
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.margin = margin(c(0,0,0,0),"point"),
          legend.spacing.y = unit(1, "point"),
          legend.key.height = unit(0, "point"),
          legend.key.width = unit(15,"point"),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          #legend.box = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=10),
          legend.title.align = 0.5,
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f4.1.png",plot = f4.1,device = "png",scale = 1.4,width = 15,height = 15,units = c("cm"),dpi = 300,limitsize = TRUE) 


f4.2 <- ggplot(data.2021.hora %>% filter( parametro == "pm10",tipo == "medicion"),
       aes())+
  
  geom_ribbon(data = data.2021.hora.l %>% filter( parametro == "pm10", tipo == "medicion"),
              aes(x = h, ymin= x5_percent, ymax = x95_percent), fill = "grey50", alpha = 0.5, show.legend = FALSE)+
  
  geom_line(data = data.2021.hora  %>% filter(parametro == "pm10",tipo == "medicion",estadistico %in% c("media")),
            aes(group = 1, x = h, y = valor), color = "#000000",show.legend = FALSE)+
  
  geom_point(data = data.2021.hora  %>% filter( parametro == "pm10",tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "grey50", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("base_aerea","canaveralejo"), parametro == "pm10", h %in% c(6,7,8,9,10),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("compartir"), parametro == "pm10", h %in% c(5,6,7,8,19,20),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("era_obrero"), parametro == "pm10", h %in% c(6,7,8,9,10,11),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("ermita"), parametro == "pm10", h %in% c(5,6,7,8,9,10,11,12,13,14),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("flora"), parametro == "pm10", h %in% c(6,7,8,9,10,11,12),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("pance"), parametro == "pm10", h %in% c(8,9,10,13,14),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("transitoria"), parametro == "pm10", h %in% c(5,6,7,8),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  geom_point(data = data.2021.hora  %>% filter(estacion %in% c("univalle"), parametro == "pm10", h %in% c(8,9,10,11,12,13,14),
                                               tipo == "medicion",estadistico %in% c("media")),
             aes(x = h, y = valor), fill = "red", size = 2, color = "#000000", shape = 21)+
  
  
  
  geom_line(data = data.2021.hora  %>% filter( parametro == "pm10",tipo == "medicion",estadistico %in% c("5%")),
            aes(group = 1, x = h, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  geom_line(data = data.2021.hora  %>% filter( parametro == "pm10",tipo == "medicion",estadistico %in% c("95%")),
            aes(group = 1, x = h, y = valor), color = "#000000", lty = 2, show.legend = FALSE)+
  
  scale_color_manual(values=c(col.est2))+
  scale_fill_manual(values=c(col.est2), labels = lab.est,
                    bquote(Concentracion~Horaria~Promedio~PM[10] ~ (mu*g/m^{-3})))+
  
  coord_cartesian(ylim = c(0,155))+
  scale_x_continuous("Horas", breaks = seq(0, 23, 4),
                     labels = lab.hora2)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Concentracion Horaria PM"[10]," (",mu,"g/", m^3,")")))+
  facet_wrap(nrow = 2, .~estacion, labeller = labeller(estacion=lab.est))+
  
  guides(fill = guide_legend(title.position="top",
                             order = 1,
                             override.aes = list(size = 3),
                             nrow = 1, byrow = TRUE))+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(5,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.box.background = element_blank(),
          legend.box.just = "top",
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.margin = margin(c(0,0,0,0),"point"),
          legend.spacing.y = unit(1, "point"),
          legend.key.height = unit(0, "point"),
          legend.key.width = unit(15,"point"),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          #legend.box = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=10),
          legend.title.align = 0.5,
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f4.2.png",plot = f4.2,device = "png",scale = 1.4,width = 15,height = 15,units = c("cm"),dpi = 300,limitsize = TRUE) 

#### anova ####

anova.pm10.1 <- aov(medicion~h, data=data.2021 %>%  filter(pm10))
anova.pm10.m <- aov(PM10~ m+Estacion + m:Estacion, data=pm10)




duncan.test(anova.pm10.1,"Estacion",console=TRUE)
summary(anova.pm10.m)
summary(anova.pm10.1)
summary(anova.pm10.h)
duncan.test(anova.pm10.m,"m",console=TRUE)

duncan.test(anova.pm10.h,"h",group=TRUE,console=TRUE,duncan)

######################## f5 ####


f5 <- resumen_ica %>% 
  ggplot(aes(as_factor(año), n, fill = categoria)) +
  geom_col(position = position_stack(reverse = TRUE),width = 0.8) +
  scale_fill_manual(values = c("green", "yellow", "orange", "red","purple","brown")) +
  labs(x = "Año", y = "Horas monitoreadas") +
  coord_cartesian(ylim=c(0,9000))+
  scale_y_continuous(expand = c(0, 0),breaks=seq(0,9000,1500))+
  guides(fill=guide_legend("Indice de Calidad de Aire"))+
  facet_grid(.~estacion,labeller=labeller(estacion=lab.est))+
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(3,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=8),
          legend.title = element_text(size=10),
          strip.text = element_text(size=8,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#008000"))

ggsave("f5.png",plot = f5,device = "png",scale = 1.75,width = 11,height = 7.5,units = c("cm"),dpi = 300,limitsize = TRUE) 


########################


meses <- c(1,2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12)
planeado <- c(51,48,46,57,49,45,48,60,62,48,45,51,104,90,100,119,94,87,93,97,106,97,81,122)
ejecutado <- c(3,28,36,38,12,10,0,44,41,28,26,3,1,11,18,27,11,9,2,21,32,38,31,41)
actividad <- c(rep("calibracion",12),rep("mantenimieto",12))


mvc <- data.frame(meses,planeado,ejecutado,actividad)
mvc <- as_tibble(mvc)
mvc <- mvc %>% pivot_longer(cols =c(planeado,ejecutado), names_to = "indicador", values_to = "valor")

mvc <-  mvc %>% mutate(indicador = fct_relevel(indicador,"planeado","ejecutado"))
mvc <-  mvc %>% arrange(meses,actividad)


g.mvc <- ggplot(mvc,aes(as.factor(meses),valor,fill=indicador))+
  geom_col(position="dodge",width = 0.5)+
  scale_y_continuous("Actividades",expand = c(0,0))+
  scale_x_discrete("Meses")+
  scale_fill_manual("Convenciones",values=c("#0070c0","#d7301f"))+
  facet_wrap(actividad~.,scales="free_y")+
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(5,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.box.background = element_blank(),
          legend.box.just = "top",
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.margin = margin(c(0,0,0,0),"point"),
          legend.spacing.y = unit(1, "point"),
          legend.key.height = unit(0, "point"),
          legend.key.width = unit(15,"point"),
          legend.key = element_blank(),
          legend.direction = "horizontal",
          legend.position = "bottom",
          #legend.box = "vertical",
          legend.text = element_text(size=9),
          legend.title = element_text(size=10),
          legend.title.align = 0.5,
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("g.mvc.png",plot = g.mvc,device = "png",scale = 1.4,width = 15,height = 6,units = c("cm"),dpi = 300,limitsize = TRUE) 



ggplot(data.2021 %>% filter (parametro %in% c("o3")), aes(fecha,medicion))+
  geom_line()+
  facet_wrap(estacion~.)




############### o3 ########################


f5.1 <- ggplot(data.2021 %>% filter(estacion != "flora",
                            parametro=="o3"),
               aes(as.factor(estacion),media_movil))+
  stat_boxplot(geom = "errorbar", width = 0.2,color="black",show.legend = FALSE)+
  geom_boxplot(aes(fill=estacion),color="black",outlier.size = .8,show.legend = FALSE, width = 0.5)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.95)},show.legend = FALSE,size=0.8,pch=3)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.9)},show.legend = FALSE,size=0.8,pch=3)+
  
  scale_fill_manual(values=c(col.est2))+
  scale_color_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 100,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+

  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limite normativo",values=c("F1"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 100 ",mu,"g/", m^3))),
                                 guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,105))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 8h O"[3]," (",mu,"g/", m^3,")")))+
  
  facet_grid(.~year(fecha))+
  guides(color=FALSE,
         lty=guide_legend(override.aes = list(size = c(1.5)),
                          nrow=1,byrow=TRUE),
         size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f5.1.png",plot = f5.1,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  



f5.2 <- ggplot(data.2021 %>% filter(estacion != "flora",
                                    parametro=="o3"),
               aes(as.factor(m),media_movil))+
  stat_boxplot(geom = "errorbar", width = 0.2,color="black",show.legend = FALSE)+
  geom_boxplot(aes(fill=estacion),color="black",outlier.size = .8,show.legend = FALSE, width = 0.5)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.95)},show.legend = FALSE,size=0.8,pch=3)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.9)},show.legend = FALSE,size=0.8,pch=3)+
  
  scale_fill_manual(values=c(col.est2))+
  scale_color_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 100,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limite normativo",values=c("F1"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 100 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,105))+
  scale_x_discrete("Meses")+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 8h O"[3]," (",mu,"g/", m^3,")")))+
  
  facet_wrap(.~estacion, ncol = 3,labeller = labeller(estacion = lab.est))+
  guides(color=FALSE,
         lty=guide_legend(override.aes = list(size = c(1.5)),
                          nrow=1,byrow=TRUE),
         size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f5.2.png",plot = f5.2,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  

###################### so2  #######################



f6.1 <- ggplot(data.2021 %>% filter(estacion %in% c("base_aerea","canaveralejo","ermita"),
                                    parametro=="so2"),
               aes(as.factor(estacion),media_movil))+
  stat_boxplot(geom = "errorbar", width = 0.2,color="black",show.legend = FALSE)+
  geom_boxplot(aes(fill=estacion),color="black",outlier.size = .8,show.legend = FALSE, width = 0.5)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.95)},show.legend = FALSE,size=0.8,pch=3)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.9)},show.legend = FALSE,size=0.8,pch=3)+
  
  scale_fill_manual(values=c(col.est2))+
  scale_color_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 50,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limite normativo",values=c("F1"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 50 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,55))+
  scale_x_discrete("Estaciones",label=lab.est)+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 24h SO"[2]," (",mu,"g/", m^3,")")))+
  
  facet_grid(.~year(fecha))+
  guides(color=FALSE,
         lty=guide_legend(override.aes = list(size = c(1.5)),
                          nrow=1,byrow=TRUE),
         size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f6.1.png",plot = f6.1,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  



f6.2 <- ggplot(data.2021 %>% filter(estacion %in% c("base_aerea","canaveralejo","ermita"),
                                      parametro=="so2"),
               aes(as.factor(m),media_movil))+
  stat_boxplot(geom = "errorbar", width = 0.2,color="black",show.legend = FALSE)+
  geom_boxplot(aes(fill=estacion),color="black",outlier.size = .8,show.legend = FALSE, width = 0.5)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.95)},show.legend = FALSE,size=0.8,pch=3)+
  geom_point(aes(color=estacion),stat="summary", fun = function(z){quantile(z,0.9)},show.legend = FALSE,size=0.8,pch=3)+
  
  scale_fill_manual(values=c(col.est2))+
  scale_color_manual(values=c(col.est2))+
  
  geom_hline(aes(yintercept = 50,lty="1 Res. 2254"),color=c("#000000"),size=1.5)+
  
  #scale_size_manual(values=c(1.5,1,1,1))+
  
  scale_linetype_manual(name="Limite normativo",values=c("F1"),
                        labels=c(bquote(paste("Res. 2254 de 2017: 100 ",mu,"g/", m^3))),
                        guide=guide_legend(override.aes = list(color=c("#000000"))))+
  
  coord_cartesian(ylim = c(0,55))+
  scale_x_discrete("Meses")+  
  scale_y_continuous(expand=c(0,0))+
  ylab(expression(paste("Media Movil 24h SO"[2]," (",mu,"g/", m^3,")")))+
  
  facet_wrap(.~estacion, ncol = 3,labeller = labeller(estacion = lab.est))+
  guides(color=FALSE,
         lty=guide_legend(override.aes = list(size = c(1.5)),
                          nrow=1,byrow=TRUE),
         size=FALSE)+
  
  theme(  panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour="grey90") ,
          panel.background = element_blank(),
          panel.border = element_rect(colour="black",fill = NA),
          panel.spacing.x = unit(0,"point"),
          panel.spacing.y = unit(10,"point"),
          axis.title = element_text(size=12),
          axis.text = element_text(size=8),
          #axis.text.x = element_text(angle=45,hjust=1),
          axis.line = element_line(colour="black"),
          plot.title = element_text(hjust=0.5),
          legend.box.background = element_blank(),
          legend.key.width = unit(30,"point"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          plot.margin = margin(c(2,10,2,2),"point"),
          legend.margin = margin(c(2,0,0,2),"point"),
          legend.box.margin = margin(c(-10,0,0,0),"point"),
          legend.text = element_text(size=10),
          legend.title = element_text(size=12),
          strip.text = element_text(size=11,face="bold",colour="white"),
          strip.background = element_rect(colour="black",fill="#0070c0"))

ggsave("f6.2.png",plot = f6.2,device = "png",scale = 1.5,width = 15,height = 7,units = c("cm"),dpi = 300,limitsize = TRUE)  
