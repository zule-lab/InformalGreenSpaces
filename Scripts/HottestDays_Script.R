################################################################################
############################ HOTTEST DAYS PLOTS ################################

#RUN INTRO SCRIPT FIRST!

#This script is all the code that went into investigating the sensor data on days where it was very hot (>=30C)
#It was mostly uninteresting in the way that it just was hot everywhere with very little difference 

##### ATTEMPT #1 #####

#The following code is for the MHM site, a similar graph to the MHM_subplot_simple graph 
hottest_days_sub <- subset(edited_temp_data, Temperature >= 30.0 & DayTime == "Day")

Friche_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_4" | Sensor_Name == "MHM_3") #separating sensors to their respective MHM park subtypes

Friche_hottest_summarized <- Friche_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Friche")

Boise_Vimont_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_1" | Sensor_Name == "MHM_2")#separating sensors to their respective MHM park subtypes

Boise_Vimont_hottest_summarized <- Boise_Vimont_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Vimont")

Boise_Steinberg_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_5" | Sensor_Name == "MHM_6" | Sensor_Name == "MHM_7")#separating sensors to their respective MHM park subtypes

Boise_Steinberg_hottest_summarized <- Boise_Steinberg_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Steinberg")

MHM_Reference_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_Reference")#separating sensors to their respective MHM park subtypes

MHM_Reference_hottest_summarized <- MHM_Reference_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Reference")

hottest_combined <- rbind(Friche_hottest_summarized, Boise_Vimont_hottest_summarized, Boise_Steinberg_hottest_summarized, MHM_Reference_hottest_summarized) #combining into one data frame

##Hottest Plot##

MHM_Sep__hottest_Simple_summarized <- hottest_combined %>% 
  group_by(Park_Sub_Type, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))

MHM__hot_Sep_Simple_Plot <- MHM_Sep__hottest_Simple_summarized %>%
  ggplot() +
  aes(x= Park_Sub_Type, y= meanT2, colour = Park_Sub_Type) +
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), strip.background = element_rect(fill = "yellowgreen"))+
  theme(legend.position = "none")+
  labs(x ="Park Sub Type", y ="Mean Temperature (°C)")

MHM__hot_Sep_Simple_Plot_changed <- MHM__hot_Sep_Simple_Plot + scale_color_manual(values=c("green","green3", "darkgreen", "gray48")) + scale_fill_manual(values=c("green","forestgreen", "darkgreen", "gray48"))

MHM__hot_Sep_Simple_Plot_changed

#ggsave("TP_All_Sensors_SimplePlot.png", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

#not really interesting.. seems like on the really hot days it was just hot everywhere


##### ATTEMPT #2 #####


#Hottest days (>=30C) but for all sites

hottest_days_sub <- subset(edited_temp_data, Temperature >= 30.0 & DayTime == "Day")

park_hottest_summarized <- hottest_days_sub %>%
  group_by(Park, Date) %>%
  summarize(meanT = mean(Temperature))

park_hottest_summarized %>%
  group_by(Park) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3)

park_hottest_sm_max = park_hottest_summarized %>% 
  group_by(Park) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.5))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

park_hottest_plot <- park_hottest_summarized %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.5, se=F, linewidth=2.5) +
  geom_label_repel(data = park_hottest_sm_max, aes(y=smooth, label= round(smooth,1), color = Park)) + 
  theme_classic() + 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))


park_hottest_plot

#ggsave("Hottest_Roughdraft.png", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

#very interesting looking plot but again shows pretty much the same thing, everything is very hot! 

##### ATTEMPT #3 #####

#This third attempt look at the hottest day of the year (while the sensors were all up), July 6th
#The graphs are SUPER bare bones
#At one point before the exhibit we were thinking about looking at this data but in the end it wasn't really pursued 
#More interesting than the 2 other attempts
#Ended up making graph for each site and the references was nicely above the park (for most sites)

edited_temp_data2 <- edited_temp_data %>%
  separate(Date, c("Year", "Month", "Day"), sep="-", remove = FALSE) #sep can split a column into two using a value, in this case " " is giving [space] as what will split it


july6_subset <- subset(edited_temp_data2, Month == "07" & Day == "06" & DayTime == "Day")


##Falaise##

july6_falaise <- subset(july6_subset, Park == "Falaise" | Park == "Falaise_Reference")

july6_falaise_sum <- july6_falaise %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_falaise_plot <- july6_falaise_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line(linewidth=2)+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "Falaise - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_falaise_plot

#ggsave("Falaise_July6_Unfinished.png", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


##MHM##

july6_MHM <- subset(july6_subset, Park == "MHM" | Park == "MHM_Reference")

july6_MHM_sum <- july6_MHM %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_MHM_plot <- july6_MHM_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line(linewidth=2)+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "MHM - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_MHM_plot

#ggsave("MHM_July6_Unfinished.png", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


##CdesP##

july6_CdesP <- subset(july6_subset, Park == "CdesP" | Park == "CdesP_Reference")

july6_CdesP_sum <- july6_CdesP %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_CdesP_plot <- july6_CdesP_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line(linewidth=2)+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "CdesP - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_CdesP_plot

#ggsave("CdesP_July6_Unfinished.png", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


##TP##

july6_TP <- subset(july6_subset, Park == "TP" | Park == "TP_Reference")

july6_TP_sum <- july6_TP %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_TP_plot <- july6_TP_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line(linewidth=2)+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "TP - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_TP_plot

#ggsave("TP_July6_Unfinished.png", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


