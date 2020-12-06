library(tidyverse)
library(sf)
library(fields)
library(jsonlite)
library(dplyr)
library(rgdal)
library(gganimate)
library(animation)
library(rmapshaper)


data <- read.csv(url("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data_concelhos.csv"))

data2 <- gather(data,key="Municipio",value="cases",-data)

data3 <- spread(data2, key="data",value=cases)


concelhos <- sf::st_read("concelho_nao_militar.shp",quiet=TRUE)

concelhos_s <- rmapshaper::ms_simplify(input = as(concelhos, 'Spatial')) %>%
  st_as_sf()

concelhos_s$Municipio<-as.character(concelhos_s$Municipio)


concelhos2<-left_join(concelhos_s,data3,by="Municipio")


concelhos3 <- gather(concelhos2,key="date",value="cases",-c(Dicofre,Municipio,
                                                            Distrito,TAA,Area_EA_Ha,
                                                            Area_T_Ha,geometry))


concelhos3$date = as.Date(concelhos3$date, format = "%d-%m-%Y")

g <- ggplot(concelhos3)+geom_sf(aes(fill=cases))+
  scale_fill_gradientn(colours=tim.colors(99))+
  transition_states(date)+
  labs(title = 'Dia: {closest_state}')


animate(g,fps=5,nframes=368)

anim_save("Casos.Conf4.gif")


g.log <- ggplot(concelhos3)+geom_sf(aes(fill=log(cases)))+
  scale_fill_gradientn(colours=tim.colors(99))+
  transition_states(date)+
  labs(title = 'Dia: {closest_state}')

animate(g.log,fps=5,nframes=368)

anim_save("log.Casos.Conf4.gif")


