#This script is not that important, it is here just to look at the temperature data and see why 
#certain smooth functions seem to deviate significantly from the others

## Falaise

falaise_subset <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference")

 Falaise_all <- subset(falaise_subset, DayTime == "Day")
 
 Falaise_all_summary <- Falaise_all %>%
   group_by(Date, Sensor_Name) %>%
   summarize(meanT = mean(Temperature))
 
 Falaise_all_summary %>%
   ggplot()+
   geom_line(aes(x=Date, y=meanT))+
   geom_smooth(method="loess", span=.6, se=F, linewdith=2.5, aes(x=Date, y=meanT))+
   theme_classic()+
   ggtitle("Falaise_ALL")+
   facet_wrap(~Sensor_Name)
 

 # Falaise_subset <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference") 
 # 
 # Falaise_1 <- subset(falaise_subset, Sensor_Name == "Falaise_1")
 # 
 # Falaise_1_summary <- Falaise_1 %>%
 #   group_by(Date, DayTime) %>%
 #   summarize(meanT = mean(Temperature))
 # 
 # Falaise_1_summary %>%
 #   ggplot()+
 #   geom_line(aes(x=Date , y= meanT))+
 #   geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5, aes(x = Date, y = meanT,)) +
 #   theme_classic()+
 #   ggtitle("Falaise_1")+
 #   facet_wrap(~DayTime)
 

## CdesP
 
 CdesP_subset <- subset(edited_temp_data, Park == "CdesP" | Park == "CdesP_Reference") 
 
 
 CdesP_all <- subset(CdesP_subset, DayTime == "Day")
 
 CdesP_all_summary <- CdesP_all %>%
   group_by(Date, Sensor_Name) %>%
   summarize(meanT = mean(Temperature))
 
 CdesP_all_summary %>%
   ggplot()+
   geom_line(aes(x=Date, y=meanT))+
   geom_smooth(method="loess", span=.6, se=F, linewidth=2.5, aes(x=Date, y=meanT))+
   theme_classic()+
   ggtitle("CdesP_ALL")+
   facet_wrap(~Sensor_Name)
 
 
# CdesP_subset <- subset(edited_temp_data, Park == "CdesP" | Park == "CdesP_Reference")
# 
#  CdesP_2 <- subset(CdesP_subset, Sensor_Name == "CdesP_2")
# 
#  CdesP_2_summary <- CdesP_2 %>%
#    group_by(Date, DayTime) %>%
#    summarize(meanT = mean(Temperature))
# 
#  CdesP_2_summary %>%
#    ggplot()+
#    geom_smooth(method = "loess", span=.7, se=F, linewidth=2.5, aes(x = Date, y = meanT,)) +
#     geom_line(aes(x=Date , y= meanT))+
#    theme_classic()+
#    ggtitle("CdesP_2")
#    facet_wrap(~DayTime)
#  
### MHM
 
 MHM_subset <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference") 
 
 MHM_all <- subset(MHM_subset, DayTime == "Day")
 
 MHM_all_summary <- MHM_all %>%
   group_by(Date, Sensor_Name) %>%
   summarize(meanT = mean(Temperature))
 
 MHM_all_summary %>%
   ggplot()+
   geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5, aes(x = Date, y = meanT,)) +
   geom_line(aes(x=Date , y= meanT))+
   theme_classic()+
   ggtitle("MHM_ALL")+
   facet_wrap(~Sensor_Name)
 
 ### TP
 
 TP_subset <- subset(edited_temp_data, Park == "TP" | Park == "TP_Reference") 
 
 TP_all <- subset(TP_subset, DayTime == "Day")
 
 TP_all_summary <- TP_all %>%
   group_by(Date, Sensor_Name) %>%
   summarize(meanT = mean(Temperature))
 
 TP_all_summary %>%
   ggplot()+
   geom_line(aes(x=Date, y=meanT))+
   geom_smooth(method="loess", span=.6, se=F, linewidth=2.5, aes(x=Date, y=meanT))+
   theme_classic()+
   ggtitle("TP_ALL")+
   facet_wrap(~Sensor_Name)
 

  
 
