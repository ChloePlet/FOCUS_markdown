library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(ggplot2)
library(gganimate)
theme_set(theme_bw())

S_Cobar<-read_csv("data/NSWSouthCobar_Entity-WaterSample.csv")

Geochem1<- read_csv("data/NSWGeochemistry1_Entity-WaterSample.csv")


Geochem1_tidy<-Geochem1 %>%
  select(7:9,12,14,17,20:27,42:45) %>%                               # xtract columns of interest
  unite(1,2,3,4, col= "SampleID", sep="-")%>%                  # combine column to get sampleID
  mutate("SampleName"=toupper(SiteName))%>%                    # homogenise samplename to capital
  separate_rows(SampleName, sep=" ")  %>%                      # ????? need to split row into 2 when duplicate sampling
  filter(str_detect(SampleName,"MX")|is.na(SampleName)) %>%    # need to delete row that do not correspond to samples
  rename(m_asl=ElevationMetresAboveSeaLevel)%>%                 #rename column for clarity
  mutate("StationDeposit"=toupper(StationDeposit)) %>% 
  #mutate(WaterTable_depth_m = sub("[^0-9]*([0-9\\.]*).*","\\1",Minex_S_Cobar$WaterTable))
  mutate(Conductivity= as.numeric(Conductivity))%>%
  select(16,1,2,5:10,12:15)

S_Cobar_tidy<-S_Cobar %>% 
  select(7:9,12,14,17,20:27,42:45) %>%                               # xtract columns of interest
  unite(1,2,3,4, col= "SampleID", sep="-")%>%                  # combine column to get sampleID
  mutate("SampleName"=toupper(SiteName))%>%                    # homogenise samplename to capital
  separate_rows(SampleName, sep=" ")  %>%                      # ????? need to split row into 2 when duplicate sampling
  filter(str_detect(SampleName,"MX")|is.na(SampleName)) %>%    # need to delete row that do not correspond to samples
  rename(m_asl=ElevationMetresAboveSeaLevel)%>%                 #rename column for clarity
  mutate("StationDeposit"=toupper(StationDeposit)) %>% 
  #mutate(WaterTable_depth_m = sub("[^0-9]*([0-9\\.]*).*","\\1",Minex_S_Cobar$WaterTable))
  select(16,1,2,5:10,12:15)

Field_NSW<-full_join(Geochem1_tidy, S_Cobar_tidy) %>% 
    rename(pH = PreferredPH) %>% 
    mutate(Eh = round(PreferredEh, digits = 0)) %>% 
    select(-SampleID, - StationDeposit, -Accuracy, -m_asl, -PreferredEh, -Conductivity)

write_csv(Obs_Field_NSW, path = "resources/results/Obs_Field_simple.csv", na = "NA", append = FALSE, col_names = TRUE,
          quote_escape = FALSE)

Temperature_plot<- ggplot(data = Field_NSW,
       mapping=aes(x = Longitude, 
                   y = Latitude,
                   colour = Temperature,
                  ))+
  geom_point()+
    scale_color_gradient2(
      low = "lightblue1", mid = "royalblue1", high = "firebrick1",midpoint = 18, space = "Lab", guide = "colourbar", aesthetics = "colour")+



  
Temperature_plot+
  labs(title = "Water Temperature",
     x = "longitude",
     y = "latitude")+
  transition_states(Temperature, transition_length = 5, state_length = 3)

pHEh_plot<- ggplot(data = Field_NSW,
                mapping=aes(x = Longitude, 
                            y = Latitude,
                            size = pH,
                            colour = Eh ))+
  geom_point(alpha = 0.6)+
  scale_color_gradient2(low = "black", mid = "blue2", high = "red")
  
                