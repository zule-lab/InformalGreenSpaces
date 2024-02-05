################################################################################
##################### R SCRIPT FOR GEOM SMOOTH GRAPHS ##########################
################################################################################

#RUN INTRO SCRIPT FIRST!

                               #############
                               ## FALAISE ##
                               #############
#This code sets the base language of R to something, in this case english for the english graphs
Sys.setlocale(category = "LC_ALL", "English")

falaise_subset <- subset(summarized, Park == "Falaise" | Park == "Falaise_Reference") 
#Need a subset of data to work with that just includes the Falaise and it's reference

#The following lines of script first edit the falaise subset, and create a data frame 
#that we can then use the geom_label_repel function to place visual maxes on the graphs
#comments will only appear here since all the code is the same except for small differences 
#such as park specific subsets and names, different spans (for the loess function), etc

falaise_subset %>%
  group_by(Park) %>% #grouping it by park to separate reference and non-reference data
  mutate(is_max = meanT == max(meanT)) %>% #need max to create the repel lables 
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) + #create the smooth model to extract max
  geom_smooth(method = "loess") + 
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

falaise_sm_max = falaise_subset %>% #creating a data set with the max values of the smooth model 
  group_by(Park, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.50))) %>% #span kept at .5 unless data fits better with different spans (visually clearer)
  slice_max(order_by = smooth) %>% #extracting the max values 
  distinct(smooth, .keep_all =  T) 

labeled_falaise_smooth_plot <- falaise_subset %>% #plotting the graph 
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.50, se=F, linewidth=2.5) + #span kept at 0.5 since it fit the data better and made the graphs the smoothest 
  geom_label_repel(data = falaise_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  #Tip: Put repel labels AFTER the code for the smooth function makes it so the labels appear over the data (not hidden behind)!
    theme_classic(base_size = 15) + #increases the base size of all text 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1), 
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+ 
  facet_wrap(vars(DayTime)) 
  

labeled_falaise_smooth_plot_changed <- labeled_falaise_smooth_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray48")) #manually edits the colours of the graph

labeled_falaise_smooth_plot_changed 

#ggsave("Falaise_Smooth_Plot.tiff", plot=labeled_falaise_smooth_plot_changed, path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")
#Use the code above so save this graph 

##

###No tags###

#falaise_nightday_plot <- falaise_subset %>%
#  ggplot() +
#  aes(x = Date, y = meanT, fill = Park, colour = Park) +
#  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
#  scale_fill_hue(direction = 1) +
#  scale_color_hue(direction = 1) +
#  theme_classic() +
#  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
#  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ #this line allows you to adjust the position of the axis titles 
#  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + #legend position and background + removes "parks" which is there for some reason
#  facet_wrap(vars(DayTime)) +
#  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
#  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

#falaise_nightday_plot_changed <- falaise_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

#falaise_nightday_plot_changed

##

#Investigating each sensor in the park individually now 

falaise_allsensors_subset <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference")

falaise_allsensors_summary <- falaise_allsensors_subset %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

falaise_allsensors_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

falaise_allsensors_sm_max = falaise_allsensors_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_falaise_allsensors_plot <- falaise_allsensors_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = falaise_allsensors_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  scale_color_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + #my own colour palette that is easy to tell apart and is also colourblind friendly!
  scale_fill_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + #found in the intro :)
  #scale fill/colour are used to manually colour in the line as well as their labeles. ORDER MATTERS! With this data set, the last colour is the reference
    labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_falaise_allsensors_plot

#ggsave("Falaise_All_Sensors_Smooth_Plot.tiff", plot=labeled_falaise_allsensors_plot, path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


#####

                                 #########
                                 ## MHM ##
                                 #########

MHM_subset <- subset(summarized, Park == "MHM" | Park == "MHM_Reference")

MHM_subset %>%
  group_by(Park) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

MHM_sm_max = MHM_subset %>% 
  group_by(Park, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.55))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_MHM_smooth_plot <- MHM_subset %>% 
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.55, se=F, linewidth=2.5) +  
  geom_label_repel(data = MHM_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1), 
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+ 
  facet_wrap(vars(DayTime))

labeled_MHM_smooth_plot_changed <- labeled_MHM_smooth_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray48"))

labeled_MHM_smooth_plot_changed 

#ggsave("MHM_Smooth_Plot.tiff", plot=labeled_MHM_smooth_plot_changed, path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

##

###NOT TAGGED###
  
#MHM_nightday_plot <- MHM_subset %>%
#  ggplot() +
#  aes(x = Date, y = meanT, fill = Park, colour = Park) +
#  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
#  scale_fill_hue(direction = 1) +
# scale_color_hue(direction = 1) +
#  theme_classic() +
# theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+
#  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank()) +
#  facet_wrap(vars(DayTime)) +
#  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"), legend.box.background = element_rect(colour = "black", linewidth = 1))+
#  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

#MHM_nightday_plot_changed <- MHM_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

##

#Investigating the MHM's three separate areas in the park and comparing them 

MHM_sensor_subset_investigation <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference")

#Friche
Friche_data <- subset(MHM_sensor_subset_investigation, Sensor_Name == "MHM_4" | Sensor_Name == "MHM_3")
#Making subsets for each location and then giving them a park sub type, I'm sure there is a better way
#of doing this that is more efficient but this is what I've done lol

Friche_data_summarized <- Friche_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Friche")


##Boise Vimont
Boise_Vimont_data <- subset(MHM_sensor_subset_investigation, Sensor_Name == "MHM_1" | Sensor_Name == "MHM_2")

Boise_Vimont_summarized <- Boise_Vimont_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Vimont")

##Boise Steinberg
Boise_Steinberg_data <- subset(MHM_sensor_subset_investigation, Sensor_Name == "MHM_5" | Sensor_Name == "MHM_6" | Sensor_Name == "MHM_7")

Boise_Steinberg_summarized <- Boise_Steinberg_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Steinberg")

##REFERENCE
MHM_Reference_data <- subset(MHM_sensor_subset_investigation, Sensor_Name == "MHM_Reference")

MHM_Reference_summarized <- MHM_Reference_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Reference")


#Combine these

parksubtypes_combined <- rbind(Friche_data_summarized, Boise_Vimont_summarized, Boise_Steinberg_summarized, MHM_Reference_summarized)


#Plotting geom_smooth
parksubtypes_combined %>%
  group_by(Park_Sub_Type) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park_Sub_Type, fill = Park_Sub_Type)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

#create the smooth and retain rows with max of smooth, using slice_max
parksubtypes_sm_max = parksubtypes_combined %>% 
  group_by(Park_Sub_Type, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.5))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

#finally plot everything
MHM_ParkSubTypes_plot <- parksubtypes_combined %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park_Sub_Type, color = Park_Sub_Type)) +
  geom_smooth(method = "loess", span=.5, se=F, linewidth=2.5) +
  #geom_label_repel(data = sm_max, aes(y=smooth, label= round(smooth,1), color = Park_Sub_Type)) + 
  geom_label_repel(data = parksubtypes_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park_Sub_Type), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  scale_color_manual(values = c( '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))
  
MHM_ParkSubTypes_plot

#ggsave("MHM_ParkSubType_Smooth_Plot.tiff", plot=MHM_ParkSubTypes_plot, path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

##

#Investigating the MHM's individual sensors compared to the reference 

MHM_allsensors_subset <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference")

MHM_allsensors_summary <- MHM_allsensors_subset %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

MHM_allsensors_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

MHM_allsensors_sm_max = MHM_allsensors_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_MHM_allsensors_plot <- MHM_allsensors_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = MHM_allsensors_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name), size=5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
    labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_MHM_allsensors_plot

#ggsave("MHM_All_Sensors_Smooth_Plot.tiff", plot=labeled_MHM_allsensors_plot, path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

#####

                                ###########
                                ## CdesP ##
                                ###########

CdesP_subset <- subset(summarized, Park == "CdesP" | Park == "CdesP_Reference")

CdesP_subset %>%
  group_by(Park) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

CdesP_sm_max = CdesP_subset %>% 
  group_by(Park, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.7))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all = T)

labeled_CdesP_smooth_plot <- CdesP_subset %>% 
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.7, se=F, linewidth=2.5) +  
  geom_label_repel(data = CdesP_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1), 
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+ 
  facet_wrap(vars(DayTime))

labeled_CdesP_smooth_plot_changed <- labeled_CdesP_smooth_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

labeled_CdesP_smooth_plot_changed

#ggsave("CdesP_Smooth_Plot.tiff", plot=labeled_CdesP_smooth_plot_changed, path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

##

#Not Tagged 

#CdesP_nightday_plot <- CdesP_subset %>%
#  ggplot() +
#  aes(x = Date, y = meanT, fill = Park, colour = Park) +
#  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
#  scale_fill_hue(direction = 1) +
#  scale_color_hue(direction = 1) +
#  theme_classic() +
#  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
#  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+
#  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) +
#  facet_wrap(vars(DayTime)) +
#  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
#  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

#CdesP_nightday_plot_changed <- CdesP_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

#CdesP_nightday_plot_changed

##

#Investigating each sensor in the park individually now 

CdesP_allsensors_subset <- subset(edited_temp_data, Park == "CdesP" | Park == "CdesP_Reference")

CdesP_allsensors_summary <- CdesP_allsensors_subset %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

CdesP_allsensors_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

CdesP_allsensors_sm_max = CdesP_allsensors_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_CdesP_allsensors_plot <- CdesP_allsensors_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = CdesP_allsensors_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_CdesP_allsensors_plot

#ggsave("CdesP_All_Sensors_Smooth_Plot.tiff", plot=labeled_CdesP_allsensors_plot, path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


#####

                                  ########
                                  ## TP ##
                                  ########

TP_subset <- subset(summarized, Park == "TP" | Park == "TP_Reference")

###WITH TAGS###

TP_subset %>%
  group_by(Park) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

TP_sm_max = TP_subset %>% 
  group_by(Park, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.55))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_TP_smooth_plot <- TP_subset %>% 
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.55, se=F, linewidth=2.5) +  
  geom_label_repel(data = TP_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1), 
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+ 
  facet_wrap(vars(DayTime))

labeled_TP_smooth_plot_changed <- labeled_TP_smooth_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray48"))

labeled_TP_smooth_plot_changed 

#ggsave("TP_Smooth_Plot.tiff", plot=labeled_TP_smooth_plot_changed, path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

##

###NO TAGS###

#TP_nightday_plot <- TP_subset %>%
#  ggplot() +
#  aes(x = Date, y = meanT, fill = Park, colour = Park) +
#  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
#  scale_fill_hue(direction = 1) +
#  scale_color_hue(direction = 1) +
#  theme_classic() +
#  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
#  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+
#  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) +
#  facet_wrap(vars(DayTime)) +
#  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
#  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

#TP_nightday_plot_changed <- TP_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

##

#Investigating each sensor in the park individually now 

TP_allsensors_subset <- subset(edited_temp_data, Park == "TP" | Park == "TP_Reference")

TP_allsensors_summary <- TP_allsensors_subset %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

TP_allsensors_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

TP_allsensors_sm_max = TP_allsensors_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_TP_allsensors_plot <- TP_allsensors_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = TP_allsensors_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_TP_allsensors_plot

#ggsave("TP__Plot.tiff", plot=labeled_TP_smooth_plot_changed, path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")
