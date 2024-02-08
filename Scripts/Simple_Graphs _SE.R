################################################################################
######################## R SCRIPT FOR SIMPLE GRAPHS ############################
################################################################################

#RUN INTRO SCRIPT FIRST!

                                #############
                                ## FALAISE ##
                                #############

falaise_subset <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference")
#Need a subset of data to work with that just includes the Falaise and it's reference

#The following lines of script first edit the falaise subset, and create a data frame that we can simply plot as points 
#We can then use the geom_label_repel function to place visual values on the graphs for readability
#Comments will only appear here since all the code is the same except for small differences such as park specific subsets, names, etc

falaise_simpleSE_summarized <- falaise_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) #need both mean and SE for the graph in order to have error bars 


falaise_simpleSE_plot <- falaise_simpleSE_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT, colour = Park) +
   geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=0.25, linewidth=1.25)+ #code for the error bars, specified by taking the mean +/- the SE
  geom_label_repel(data = falaise_simpleSE_summarized, aes(y=meanT, label= round(meanT,1), fill = factor(Park), size=5), fontface = 'bold', color = 'white', force=100, segment.color="black", segment.size=1, show.legend = F) + #geom_label_repel function puts a label with the specified information (from a data frame) 
  #Tip: Put repel labels AFTER the code for the points and error bars makes it so the labels appear above the data (unobstructed)!
  scale_color_manual(values = c("forestgreen", "gray48")) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  scale_fill_manual(values = c("forestgreen", "gray48")) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  theme_classic(base_size = 15)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

falaise_simpleSE_plot

#ggsave("Falaise_SimpleSE_Plot.tiff", plot=falaise_simpleSE_plot, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

##

#Investigating each sensor in the park individually now 

falaise_allsensors_simple_subset <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference")

falaise_allsensors_simpleSE_summary <- falaise_allsensors_simple_subset %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) #need both mean and SE for the graph in order to have error bars 

falaise_simpleSE_allsensors_plot <- falaise_allsensors_simpleSE_summary %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5)+ #code for the error bars, specified by taking the mean +/- the SE
  geom_label_repel(data = falaise_allsensors_simpleSE_summary, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), fontface = 'bold', color = 'white', show.legend = F, force=100, segment.color="black", segment.size=1 ) + #geom_label_repel function puts a label with the specified information (from a data frame) 
  #Tip: Put repel labels AFTER the code for the points and error bars makes it so the labels appear above the data (unobstructed)!
  scale_color_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  scale_fill_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

falaise_simpleSE_allsensors_plot

#ggsave("Falaise_SimpleSE_AllSensors_Plot.tiff", plot=falaise_simpleSE_allsensors_plot, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

#####


                                  #########
                                  ## MHM ##
                                  #########

MHM_subset <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference") 

MHM_simpleSE_summary <- MHM_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SE = std.error(Temperature, na.rm = TRUE)) #need both mean and SE for the graph in order to have error bars 

MHM_simpleSE_plot <- MHM_simpleSE_summary %>%
  ggplot() +
  aes(x=Park , y= meanT, colour = Park) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SE, ymax=meanT+SE), width=.5, linewidth=1.5, 
                position=position_dodge(0.05))+ #code for the error bars, specified by taking the mean +/- the SE
  geom_label_repel(data = MHM_simpleSE_summary, aes(y=meanT, label= round(meanT,1), fill = factor(Park), size=5), fontface = 'bold', color = 'white', show.legend = F, segment.color="black", segment.size=1 ) + #geom_label_repel function puts a label with the specified information (from a data frame) 
  scale_color_manual(values = c("forestgreen", "gray48")) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  scale_fill_manual(values = c("forestgreen", "gray48")) + #Manually specifying the colours we want will make both the label and point colours be cohesive
  theme_classic(base_size = 20)+ #increases the base size of all text 
  theme(strip.text = element_text(size= 20, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

MHM_simpleSE_plot



#ggsave("MHM_SimpleSE_Plot.tiff", plot=MHM_simpleSE_plot, path = "Graphics", dpi = 600, width = 20, height = 25, units = "cm")

##

#Investigating each sensor in the park individually now 

MHM_allsensors_simple_subset <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference")

MHM_allsensors_simple_summary <- MHM_allsensors_simple_subset %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE)) #need both mean and SD for the graph in order to have error bars 

MHM_simple_allsensors_plot <- MHM_allsensors_simple_summary %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = MHM_allsensors_simple_summary, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), fontface = 'bold', color = 'white', show.legend = F) +  
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) +
  theme_classic(base_size = 15)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

MHM_simple_allsensors_plot

#ggsave("MHM_Simple_AllSensors_Plot.tiff", plot=MHM_simple_allsensors_plot, path = "Graphics", dpi = 600, width = 20, height = 15, units = "cm")

#####

                                ###########
                                ## CdesP ##
                                ###########

CdesP_subset <- subset(summarized, Park == "CdesP" | Park == "CdesP_Reference") 


CdesP_simple_summarized <- CdesP_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))#good if you need to look at mean temp of a day for a specific sensor


CdesP_simple_plot <- CdesP_simple_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT2, colour = Park) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = CdesP_simple_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park), size=5), fontface = 'bold', color = 'white', show.legend = F) +  
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_classic(base_size = 15)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

CdesP_simple_plot

#ggsave("CdesP_Simple_Plot.tiff", plot=CdesP_simple_plot, path = "Graphics", dpi = 600, width = 20, height = 15, units = "cm")

##

#Investigating each sensor in the park individually now 

CdesP_allsensors_simple_subset <- subset(edited_temp_data, Park == "CdesP" | Park == "CdesP_Reference")

CdesP_allsensors_simple_summary <- CdesP_allsensors_simple_subset %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE)) #need both mean and SD for the graph in order to have error bars 

CdesP_simple_allsensors_plot <- CdesP_allsensors_simple_summary %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = CdesP_allsensors_simple_summary, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), fontface = 'bold', color = 'white', show.legend = F) +  
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) +
  theme_classic(base_size = 15)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

CdesP_simple_allsensors_plot

#ggsave("CdesP_Simple_AllSensors_Plot.tiff", plot=CdesP_simple_allsensors_plot, path = "Graphics", dpi = 600, width = 20, height = 15, units = "cm")

#####

                                  ########
                                  ## TP ##
                                  ########

TP_subset <- subset(summarized, Park == "TP" | Park == "TP_Reference") 

TP_simple_summarized <- TP_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))#good if you need to look at mean temp of a day for a specific sensor


TP_simple_plot <- TP_simple_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT2, colour = Park) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = TP_simple_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park), size=5), fontface = 'bold', color = 'white', show.legend = F) +  
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_classic(base_size = 15)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

TP_simple_plot

#ggsave("TP_Simple_Plot.tiff", plot=TP_simple_plot, path = "Graphics", dpi = 600, width = 20, height = 15, units = "cm")

##

#Investigating each sensor in the park individually now 

TP_allsensors_simple_subset <- subset(edited_temp_data, Park == "TP" | Park == "TP_Reference")

TP_allsensors_simple_summary <- TP_allsensors_simple_subset %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE)) #need both mean and SD for the graph in order to have error bars 

TP_simple_allsensors_plot <- TP_allsensors_simple_summary %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_point(size = 4)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1.5, 
                position=position_dodge(0.05))+ 
  geom_label_repel(data = TP_allsensors_simple_summary, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name), size=5), fontface = 'bold', color = 'white', show.legend = F) +  
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) +
  theme_classic(base_size = 15)+  
  theme(strip.text = element_text(size= 18, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        axis.text.x=element_text(angle = 45, hjust =1),
        legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)") +
  facet_wrap(vars(DayTime))

TP_simple_allsensors_plot

#ggsave("TP_Simple_AllSensors_Plot.tiff", plot=TP_simple_allsensors_plot, path = "Graphics", dpi = 600, width = 20, height = 15, units = "cm")
