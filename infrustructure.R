df<-read.csv("C:/Users/chikazhet/infrastructure.csv")

# All mitigations plotted
MyData<- df
#MyData$nlosschange <- is.numeric(MyData$nlosschange)
library(tidyverse)
library(ggplot2)
library("reshape2")
library("ggplot2")
library(KernSmooth)
library(dplyr)

ggplot()+        
  geom_point(data=MyData, mapping=aes(y = profitchange, x = nlosschange, colour=mitigation))

# filter de-intensification mitigations
temp0 <- MyData %>% 
  filter(mitigation %in% c("base", "gmp" , "gmp_10%" , "gmp_20%" , "gmp_30%"))
ggplot()+
geom_point(data=temp0, mapping=aes(y = profitchange, x = nlosschange, colour=mitigation))

# filter feed pad mitigation
temp1 <- MyData %>%
  filter(mitigation %in% c("base", "gmp" ,"feedpad",  "feedpad_10%" , "feedpad_20%" , "feedpad_30%"))

# filter stand-off pad mitigation
temp2 <- MyData %>%
  filter(mitigation %in% c("base", "gmp" ,"stand-off pad",  "stand-off pad_10%" , "stand-off pad_20%" , "stand-off pad_30%"))

# filter barn mitigation
temp3 <- MyData %>%
  filter(mitigation %in% c("base", "gmp" ,"barn"))


# Binding farms needing a barn together. 
temp <-  rbind(temp0 %>% mutate(Option = "No off-paddock structure"), 
               temp1 %>% mutate(Option = "Feedpad"),
               #temp2 %>% mutate(Option = "stand-off pad"),
               temp3 %>% mutate(Option = "barn"))
temp$Option<-  factor(temp$Option, levels=c("No off-paddock structure","Feedpad","barn")) # Ordering for the legend.

temps <- temp %>%
  filter(farm %in% c("Farm1_N surplus235kg/ha", "Farm2_N surplus253kg/ha" ,"Farm3_N surplus289kg/ha"))

# Binding farms needing a stand-off pad. 
tempo <-  rbind(temp0 %>% mutate(Option = "No off-paddock structure"), 
               temp1 %>% mutate(Option = "Feedpad"),
               temp2 %>% mutate(Option = "stand-off pad"))#,
               #temp3 %>% mutate(Option = "barn"))
tempo$Option<-  factor(tempo$Option, levels=c("No off-paddock structure","Feedpad","stand-off pad")) #Ordering for the legend.

tempk <- tempo %>%
  filter(farm %in% c("Farm4_N surplus182kg/ha", "Farm5_N surplus150kg/ha" ,"Farm6_N surplus149kg/ha"))


 # ggplot for high N surplus, barn requiring farms.
ggplot(data = temps, mapping = aes(y = profitchange, x = nlosschange, group= Option,colour=Option)) +
  geom_line()+
 geom_point(aes(shape=Option), size=3)+scale_shape_manual(values=c("No off-paddock structure"=16,"Feedpad"=17,"barn"=7))+  
  labs(y="% change in operating profit", x="% change in N leaching") + theme(legend.title=element_blank()) + theme(legend.position = "bottom") +
  theme(plot.title = element_text(face = "bold", size = 100))+
  theme(axis.text.x = element_text(size=10, face="bold", color = "black"),
        axis.text.y = element_text(size=10, face="bold", color = "black"))+
  xlim(-45,0)+
  ylim(-110,0)+
   facet_wrap(~ farm, scales = "free")

# ggplot for low N surplus, stand-off pad requiring farms.
ggplot(data = tempk, mapping = aes(y = profitchange, x = nlosschange, group= Option, colour=Option)) +
  geom_line()+
  geom_point(aes(shape=Option), size=3)+scale_shape_manual(values=c("No off-paddock structure"=16,"Feedpad"=17,"stand-off pad"=15))+  
  labs(y="% change in operating profit", x="% change in N leaching") + theme(legend.title=element_blank()) + theme(legend.position = "bottom") +
  theme(plot.title = element_text(face = "bold", size = 100))+
  theme(axis.text.x = element_text(size=10, face="bold", color = "black"),
        axis.text.y = element_text(size=10, face="bold", color = "black"))+
   xlim(-45,0)+
  ylim(-110,0)+

  facet_wrap(~ farm, scales = "free")
 



    