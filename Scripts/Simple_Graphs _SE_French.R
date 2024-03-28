################################################################################
######################## R SCRIPT FOR SIMPLE GRAPHS ############################
################################################################################

#RUN INTRO SCRIPT FIRST!

#THIS CODE WILL GENERATE GRAPHS WITH FRENCH TEXT 

                                #############
                                ## FALAISE ##
                                #############

falaise_subset_F <- subset(edited_temp_data_F, Park == "Falaise" | Park == "Falaise_Référence")
#Need a subset of data to work with that just includes the Falaise and it's reference

#The following lines of script first edit the falaise subset, and create a data frame that we can simply plot as points 
#We can then use the geom_label_repel function to place visual values on the graphs for readability
#Comments will only appear here since all the code is the same except for small differences such as park specific subsets, names, etc

falaise_simpleSE_summarized_F <- falaise_subset_F %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) #need both mean and SE for the graph in order to have error bars 


falaise_simpleSE_plot_F <- falaise_simpleSE_summarized_F %>%
  ggplot() +
  aes(x= Park, y= meanT, colour = Park) +
   geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=0.25, linewidth=1.25)+ #code for the error bars, specified by taking the mean +/- the SE
  geom_label_repel(data = falaise_simpleSE_summarized_F, aes(y=meanT, label= round(meanT,1), fill = factor(Park), size=5), 
                   fontface = 'bold', color = 'white', force=50, segment.color="black", segment.size=1, show.legend = F, min.segment.length = 0) + #geom_label_repel function puts a label with the specified information (from a data frame) 
  #Tip: Put repel labels AFTER the code for the points and error bars makes it so the labels appear above the data (unobstructed)! (I am silly and had no idea this worked liked this lol)
  scale_color_manual(values = c("forestgreen", "gray48")) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), #size and colour of text of the facet strip on top of graphs
        strip.background = element_rect(fill = "yellowgreen"), #colour of the facet strip on top of graphs 
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1), #add panel border colour to box off the plots
        legend.position = "none")+ #removes legend
  labs(x ="Location", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

falaise_simpleSE_plot_F

#ggsave("Falaise_SimpleSE_Plot_FRENCH.png", plot=falaise_simpleSE_plot_F, path = "Graphics", dpi = 600, width = 15, height = 20, units = "cm")

##

#Investigating each sensor in the park individually now 

falaise_subset_F <- subset(edited_temp_data_F, Park == "Falaise" | Park == "Falaise_Référence")

falaise_allsensors_simpleSE_summary_F <- falaise_subset_F %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) #need both mean and SE for the graph in order to have error bars 

falaise_simpleSE_allsensors_plot_F <- falaise_allsensors_simpleSE_summary_F %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5)+ #code for the error bars, specified by taking the mean +/- the SE
  geom_label_repel(data = falaise_allsensors_simpleSE_summary_F, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), 
                   fontface = 'bold', color = 'white', show.legend = F, force=50, segment.color="black", segment.size=1, min.segment.length = 0 ) + #geom_label_repel function puts a label with the specified information (from a data frame) 
  #Tip: Put repel labels AFTER the code for the points and error bars makes it so the labels appear above the data (unobstructed)!
  scale_color_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  scale_fill_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Capteur", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

falaise_simpleSE_allsensors_plot_F

#ggsave("Falaise_SimpleSE_AllSensors_Plot_FRENCH.png", plot=falaise_simpleSE_allsensors_plot_F, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

#####


                                  #########
                                  ## MHM ##
                                  #########

MHM_subset_F <- subset(edited_temp_data_F, Park == "MHM" | Park == "MHM_Référence") 

MHM_simpleSE_summary_F <- MHM_subset_F %>% 
  group_by(Park, DayTime) %>% 
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) 

MHM_simpleSE_plot_F <- MHM_simpleSE_summary_F %>%
  ggplot() +
  aes(x=Park , y= meanT, colour = Park) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = MHM_simpleSE_summary_F, aes(y=meanT, label= round(meanT,1), fill = factor(Park), size=5), 
                   fontface = 'bold', color = 'white', show.legend = F, force = 50, segment.color="black", segment.size=1, min.segment.length = 0 ) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_classic(base_size = 20)+ 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none", axis.text.x=element_text(angle = 45, hjust =1))+
  labs(x ="Location", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

MHM_simpleSE_plot_F



#ggsave("MHM_SimpleSE_Plot_FRENCH.png", plot=MHM_simpleSE_plot_F, path = "Graphics", dpi = 600, width = 15, height = 20, units = "cm")

##

# Investigating the MHM in its 3 subsets

MHM_subset_F <- subset(edited_temp_data_F, Park == "MHM" | Park == "MHM_Référence") 

#Friche
Friche_data_F <- subset(MHM_subset_F, Sensor_Name == "MHM_4" | Sensor_Name == "MHM_3")
#Making subsets for each location and then giving them a park sub type, I'm sure there is a better way
#of doing this that is more efficient but this is what I've done lol

Friche_data_summarized_F <- Friche_data_F %>%
  group_by(DayTime, Park) %>%
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) %>%
  mutate(Park_Sub_Type = "Friche")

##Boisé Vimont
Boise_Vimont_data_F <- subset(MHM_subset_F, Sensor_Name == "MHM_1" | Sensor_Name == "MHM_2")

Boise_Vimont_summarized_F <- Boise_Vimont_data_F %>%
  group_by(DayTime, Park) %>%
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) %>%
  mutate(Park_Sub_Type = "Boisé_Vimont")

##Boisé Steinberg
Boise_Steinberg_data_F <- subset(MHM_subset_F, Sensor_Name == "MHM_5" | Sensor_Name == "MHM_6" | Sensor_Name == "MHM_7")

Boise_Steinberg_summarized_F <- Boise_Steinberg_data_F %>%
  group_by(DayTime, Park) %>%
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) %>%
  mutate(Park_Sub_Type = "Boisé_Steinberg")

##REFERENCE
MHM_Reference_data_F <- subset(MHM_subset_F, Sensor_Name == "MHM_Référence")

MHM_Reference_summarized_F <- MHM_Reference_data_F %>%
  group_by(DayTime, Park) %>%
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) %>%
  mutate(Park_Sub_Type = "Référence")


#Combine these

parksubtypes_combined_F <- rbind(Friche_data_summarized_F, Boise_Vimont_summarized_F, Boise_Steinberg_summarized_F, MHM_Reference_summarized_F)

#finally plot everything

MHM_ParkSubTypes_SimpleSE_plot_F <- parksubtypes_combined_F %>%
  ggplot() +
  aes(x=Park_Sub_Type , y= meanT, colour = Park_Sub_Type) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = parksubtypes_combined_F,aes(y=meanT, label= round(meanT,1), fill = factor(Park_Sub_Type), size=5), 
                   fontface = 'bold', color = 'white', show.legend = F, force = 100, segment.color="black", segment.size=1, min.segment.length = 0 ) + 
  scale_color_manual(values = c( '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  theme_classic(base_size = 20)+ 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none", axis.text.x=element_text(angle = 45, hjust =1))+
  labs(x ="Location", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

MHM_ParkSubTypes_SimpleSE_plot_F

#ggsave("MHM_ParkSubTypes_SimpleSE_plot_FRENCH.png", plot=MHM_ParkSubTypes_SimpleSE_plot_F, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

##

#Investigating each sensor in the park individually now 

MHM_subset_F <- subset(edited_temp_data_F, Park == "MHM" | Park == "MHM_Référence")

MHM_allsensors_simpleSE_summary_F <- MHM_subset_F %>% 
  group_by(Sensor_Name, DayTime) %>% 
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) 

MHM_simpleSE_allsensors_plot_F <- MHM_allsensors_simpleSE_summary_F %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5)+ 
  geom_label_repel(data = MHM_allsensors_simpleSE_summary_F, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), 
                   fontface = 'bold', color = 'white', show.legend = F, force=50, segment.color="black", segment.size=1, min.segment.length = 0) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) +
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Capteur", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

MHM_simpleSE_allsensors_plot_F

#ggsave("MHM_SimpleSE_AllSensors_Plot_FRENCH.png", plot=MHM_simpleSE_allsensors_plot_F, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

#####

                                ###########
                                ## CdesP ##
                                ###########

CdesP_subset_F <- subset(edited_temp_data_F, Park == "CdesP" | Park == "CdesP_Référence") 

CdesP_simpleSE_summarized_F <- CdesP_subset_F %>% 
  group_by(Park, DayTime) %>% 
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) 


CdesP_simpleSE_plot_F <- CdesP_simpleSE_summarized_F %>%
  ggplot() +
  aes(x= Park, y= meanT, colour = Park) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=0.25, linewidth=1.25)+ 
  geom_label_repel(data = CdesP_simpleSE_summarized_F, aes(y=meanT, label= round(meanT,1), fill = factor(Park), size=5), 
                   fontface = 'bold', color = 'white', force=100, segment.color="black", segment.size=1, show.legend = F, min.segment.length = 0) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_classic(base_size = 20)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")+
  labs(x ="Location", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

CdesP_simpleSE_plot_F

#ggsave("CdesP_SimpleSE_Plot_FRENCH.png", plot=CdesP_simpleSE_plot_F, path = "Graphics", dpi = 600, width = 15, height = 20, units = "cm")

##

#Investigating each sensor in the park individually now 

CdesP_subset_F <- subset(edited_temp_data_F, Park == "CdesP" | Park == "CdesP_Référence") 

CdesP_allsensors_simpleSE_summary_F <- CdesP_subset_F %>% 
  group_by(Sensor_Name, DayTime) %>% 
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) 

CdesP_simpleSE_allsensors_plot_F <- CdesP_allsensors_simpleSE_summary_F %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5)+ 
  geom_label_repel(data = CdesP_allsensors_simpleSE_summary_F, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), 
                   fontface = 'bold', color = 'white', show.legend = F, force=70, segment.color="black", segment.size=1, min.segment.length = 0) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) +
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Capteur", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

CdesP_simpleSE_allsensors_plot_F

#ggsave("CdesP_SimpleSE_AllSensors_Plot_FRENCH.png", plot=CdesP_simpleSE_allsensors_plot_F, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

#####

                                  ########
                                  ## TP ##
                                  ########

TP_subset_F <- subset(edited_temp_data_F, Park == "TP" | Park == "TP_Référence") 

TP_simpleSE_summarized_F <- TP_subset_F %>% 
  group_by(Park, DayTime) %>% 
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) 


TP_simpleSE_plot_F <- TP_simpleSE_summarized_F %>%
  ggplot() +
  aes(x= Park, y= meanT, colour = Park) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=0.25, linewidth=1.25)+ 
  geom_label_repel(data = TP_simpleSE_summarized_F, aes(y=meanT, label= round(meanT,1), fill = factor(Park), size=5), 
                   fontface = 'bold', color = 'white', force=100, segment.color="black", segment.size=1, show.legend = F, min.segment.length = 0) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_classic(base_size = 20)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")+
  labs(x ="Location", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

TP_simpleSE_plot_F

#ggsave("TP_SimpleSE_Plot_FRENCH.png", plot=TP_simpleSE_plot_F, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

##

#Investigating each sensor in the park individually now 

TP_subset_F <- subset(edited_temp_data_F, Park == "TP" | Park == "TP_Référence") 

TP_allsensors_simpleSE_summary_F <- TP_subset_F %>% 
  group_by(Sensor_Name, DayTime) %>% 
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) 

TP_simpleSE_allsensors_plot_F <- TP_allsensors_simpleSE_summary_F %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5)+ 
  geom_label_repel(data = TP_allsensors_simpleSE_summary_F, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), 
                   fontface = 'bold', color = 'white', show.legend = F, force=100, segment.color="black", segment.size=1, min.segment.length = 0) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) +
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Capeur", y ="Température Moyenne (°C)") +
  facet_wrap(vars(DayTime))

TP_simpleSE_allsensors_plot_F

#ggsave("TP_SimpleSE_AllSensors_Plot_FRENCH.png", plot=TP_simpleSE_allsensors_plot_F, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")
