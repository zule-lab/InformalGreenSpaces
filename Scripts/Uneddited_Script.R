################################################################################
############################## INTRO R SCRIPT ##################################
################################################################################

#first install the following packages, all of these should be used in the code
#(some might not if the code is commented out or if it was deleted in this final version)

#install.packages(c("dplyr","tidyr","stringr","ggplot2","chron","viridis","esquisse","patchwork","mgcv", "MetBrewer", "ggrepel"))

#Time to load the packages
library("dplyr", "tidyr","stringr","ggplot2","chron","esquisse","patchwork","MetBrewer","gghighlight","ggrepel","mgcv")

#The following colour codes is the colourblind-friendly palette i've compiled for this project 
#the last two colours I've reserved for reference sensors, the other are used for the park sensors if there are multiple displayed on one graph
c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')


##Time to load up the data!

##### ENGLISH #####

temperature_data <- read.csv("Input/Compiled_Data2023.csv")
#View(temperature_data)

##Editing the data to make the data frame that will be used for analysis 
edited_temp_data <- temperature_data %>%
  mutate(DateTime = str_replace_all(DateTime, '[\\.]',''), #'[\\.]','' is a string of code that replaces all instances of '.' by 'x', in this case since it's not defined its nothing 
         #(this causes a problem with strptime if its a.m.)
         DateTime = strptime(DateTime, format = "%Y-%m-%d %I:%M:%S %p")) %>% #strptime allows our data to be viewed as TIME and not like a character 
  separate(DateTime, c("Date", "Time"), sep =" ", remove = FALSE) %>% #sep can split a column into two using a value, in this case " " is giving [space] as what will split it
  mutate(Date = as.Date(Date),
         Time = replace_na(Time, "00:00:01"),Time = times(Time)) %>% #for some reason time is wonky at midnight so I replaced all values with 00:00:01
  mutate(DayTime = case_when(Time > "6:00:00" & Time < "20:30:00" ~ 'Day',
                             Time <= "6:00:00" | Time >= "20:30:00" ~ 'Night')) #this logical sequences tells r to associate day or night with data that is between (or is) these time frames 

summarized <- edited_temp_data %>% 
  group_by(Park, Date, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(maxT = max(Temperature), #good if you need to look at mean temp of a day for a specific sensor
            meanT = mean(Temperature))

##### FRENCH #####

temperature_data_french <- read.csv("Input/Compiled_Data2023.csv")


edited_temp_data_F <- temperature_data_french %>%
  mutate(DateTime = str_replace_all(DateTime, '[\\.]',''), #'[\\.]','' is a string of code that replaces all instances of '.' by 'x', in this case since it's not defined its nothing 
         #(this causes a problem with strptime if its a.m.)
         DateTime = strptime(DateTime, format = "%Y-%m-%d %I:%M:%S %p")) %>% #strptime allows our data to be viewed as TIME and not like a character 
  separate(DateTime, c("Date", "Time"), sep =" ", remove = FALSE) %>% #sep can split a column into two using a value, in this case " " is giving [space] as what wil split it
  mutate(Date = as.Date(Date),
         Time = replace_na(Time, "00:00:01"),Time = times(Time)) %>% #for some reason time is wonky at midnight so I replaced all values with 00:00:01
  mutate(DayTime = case_when(Time > "6:00:00" & Time < "20:30:00" ~ 'Journée',
                             Time <= "6:00:00" | Time >= "20:30:00" ~ 'Nuit')) #this logical sequences tells r to associate day or night with data that is between (or is) time frames 

summarized_F <- edited_temp_data_F %>% 
  group_by(Park, Date, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(maxT = max(Temperature), #good if you need to look at mean temp of a day for a specific sensor
            meanT = mean(Temperature))

#esquisser(summarized) #view the data and manipulate it which is pretty cool, thanks Rikka!

##Plot Each Park With Their References Faceted by the daytime 

#Falaise

falaise_subset <- subset(summarized, Park == "Falaise" | Park == "Falaise_Reference") #Need a subset of data to work with that just includes the Falaise and it's reference

###doing the tags###
falaise_subset %>%
  group_by(Park) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

falaise_sm_max = falaise_subset %>% 
  group_by(Park, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.50))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_falaise_nightday_plot <- falaise_subset %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.50, se=F, linewidth=2.5) +
  geom_label_repel(data = falaise_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  facet_wrap(vars(DayTime)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

labeled_falaise_nightday_plot_changed <- labeled_falaise_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

labeled_falaise_nightday_plot_changed 

#ggsave("Falaise_ParkandReference.tiff", path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")


###No tags 

falaise_nightday_plot <- falaise_subset %>%
  ggplot() +
  aes(x = Date, y = meanT, fill = Park, colour = Park) +
  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_classic() +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ #this line allows you to adjust the position of the axis titles 
  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + #legend position and background + removes "parks" which is there for some reason
  facet_wrap(vars(DayTime)) +
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

falaise_nightday_plot_changed <- falaise_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

falaise_nightday_plot_changed

#CdesP

CdesP_subset <- subset(summarized, Park == "CdesP" | Park == "CdesP_Reference")

###WITH TAGS###

CdesP_subset %>%
  group_by(Park) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park, fill = Park)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

CdesP_subset_sm_max = CdesP_subset %>% 
  group_by(Park, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.7))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all = T)

labeled_CdesP_subset_nightday_plot <- CdesP_subset %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.7, se=F, linewidth=2.5) +
  geom_label_repel(data = CdesP_subset_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size = 5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  facet_wrap(vars(DayTime)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

labeled_CdesP_subset_nightday_plot_changed <- labeled_CdesP_subset_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

labeled_CdesP_subset_nightday_plot_changed 

#ggsave("CdesP_ParkandReference.tiff", path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

#Not Tagged 

CdesP_nightday_plot <- CdesP_subset %>%
  ggplot() +
  aes(x = Date, y = meanT, fill = Park, colour = Park) +
  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_classic() +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+
  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) +
  facet_wrap(vars(DayTime)) +
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))


CdesP_nightday_plot_changed <- CdesP_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

CdesP_nightday_plot_changed

#MHM

MHM_subset <- subset(summarized, Park == "MHM" | Park == "MHM_Reference")

###WITH TAGS###

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

labeled_MHM_nightday_plot <- MHM_subset %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.55, se=F, linewidth=2.5) +
  geom_label_repel(data = MHM_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size =5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  facet_wrap(vars(DayTime)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

labeled_MHM_nightday_plot_changed <- labeled_MHM_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

labeled_MHM_nightday_plot_changed 

#ggsave("MHM_ParkandReference.tiff", path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

###NOT TAGGED###

MHM_nightday_plot <- MHM_subset %>%
  ggplot() +
  aes(x = Date, y = meanT, fill = Park, colour = Park) +
  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_classic() +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+
  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank()) +
  facet_wrap(vars(DayTime)) +
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"), legend.box.background = element_rect(colour = "black", linewidth = 1))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

MHM_nightday_plot_changed <- MHM_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

#TP 

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

labeled_TP_nightday_plot <- TP_subset %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park, color = Park)) +
  geom_smooth(method = "loess", span=.55, se=F, linewidth=2.5) +
  geom_label_repel(data = TP_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park), size =5), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic(base_size = 15) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  facet_wrap(vars(DayTime)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

labeled_TP_nightday_plot_changed <- labeled_TP_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

labeled_TP_nightday_plot_changed 

#ggsave("TP_ParkandReference.tiff", path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

###NO TAGS###

TP_nightday_plot <- TP_subset %>%
  ggplot() +
  aes(x = Date, y = meanT, fill = Park, colour = Park) +
  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_classic() +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+
  theme(legend.position = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) +
  facet_wrap(vars(DayTime)) +
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

TP_nightday_plot_changed <- TP_nightday_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

#Lets see all plots
falaise_nightday_plot_changed
CdesP_nightday_plot_changed
MHM_nightday_plot_changed
TP_nightday_plot_changed

#ggsave(filename ="Falaise_ParkandReference1.tiff", plot = falaise_nightday_plot_changed, path ="Graphics", dpi=600)   USE THIS TO SAVE STUFF
#ggsave("CdesP_ParkandReference.tiff", path = "Graphics", dpi = 600, width = 25, height = 15, units = "cm")

##Making the differences plot##

#Doing it all together

wide_summary <- pivot_wider(summarized, names_from = Park, values_from = c(maxT, meanT)) #pivot_widers allows us to perform the difference by making the dataframe
#wide instead of long. So now each date has a value for each park! 

#wide_summary_day <- subset(wide_summary, DayTime != "Night") #We are interested in the Day values mostly 

wide_summary_day_with_difference <- wide_summary %>% #now we mutate the wide data set with the differences. Reference - Park to get +'ve values
  mutate(Falaise_Mean_Difference = meanT_Falaise_Reference - meanT_Falaise) %>%
  mutate(CdesP_Mean_Difference = meanT_CdesP_Reference - meanT_CdesP) %>%
  mutate(MHM__Mean_Difference = meanT_MHM_Reference - meanT_MHM) %>%
  mutate(TP_Mean_Difference = meanT_TP_Reference - meanT_TP) %>% 
  pivot_longer(cols = c(Falaise_Mean_Difference, CdesP_Mean_Difference, MHM__Mean_Difference, TP_Mean_Difference), values_to = 'temp') %>% 
  select(c(Date, DayTime, name, temp))

#We can now plot the data using ggplot2 

#ggplot(wide_summary_day_with_difference, aes(x=Date)) + 
#  geom_line(aes(y = Falaise_Mean_Difference), linewidth = 1) + 
#  geom_line(aes(y = CdesP_Mean_Difference), linewidth = 1)+
#  geom_line(aes(y = MHM__Mean_Difference), linewidth = 1)+
#  geom_line(aes(y= TP_Mean_Difference), linewidth = 1)+
# scale_colour_met_d('Egypt') +
#    theme_classic()

Difference_allplots <- ggplot(wide_summary_day_with_difference, aes(x=Date, y = temp, colour = DayTime)) + 
  geom_line(linewidth = 1.5) + 
  theme_classic() +
  labs(x ="Date (Month)", y ="Temperature (Mean Difference in °C)")+
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", linewidth = 1))+
  scale_colour_met_d('Egypt')+
  facet_wrap(~ name)+
  geom_hline(yintercept = 0, linetype = 5, colour = 'purple4', show.legend =T, linewidth = 1)+
  theme(strip.text = element_text(size= 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

Difference_allplots

#ggsave("Difference_Plot_All_Sites.tiff", path = "Graphics", dpi = 600, width = 30, height = 20, units = "cm")


all

split_all <- split(wide_summary_day_with_difference, wide_summary_day_with_difference$name)

my_plot <- function(dat) {
  ggplot(dat, aes(x = Date, y = temp, colour=DayTime)) +
    geom_line(linewidth=1) +
    theme_classic()+
    theme(legend.positio = "bottom", legend.background = element_rect("lightsteelblue"), legend.title = element_blank())+
    labs(title = unique(dat$name), x ="Date (Month)", y ="Temperature (Mean Difference in °C)")+
    scale_colour_met_d('Egypt') +
    
    theme(strip.text = element_text(size= 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))
}

lapply(split_all, my_plot)

################################################################
#### Lets do simple mean graphs between reference and parks ####
################################################################

#Falaise

falaise_subset

#mean(falaise_subset$meanT, falaise_subset$Park == "Falaise")

falaise_summarized <- falaise_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))#good if you need to look at mean temp of a day for a specific sensor


falaise_simple <- falaise_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT2, colour = Park) +
  geom_label_repel(data = falaise_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), strip.background = element_rect(fill = "yellowgreen"))+
  theme(legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)")

falaise_simple_changed <- falaise_simple + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

falaise_simple_changed

#ggsave("Falaise_Simple_Plot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

#CdesP

CdesP_summarized <- CdesP_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))#good if you need to look at mean temp of a day for a specific sensor

CdesP_simple <- CdesP_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT2, colour = Park) +
  geom_label_repel(data = CdesP_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), strip.background = element_rect(fill = "yellowgreen"))+
  theme(legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)")

CdesP_simple_changed <- CdesP_simple + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

CdesP_simple_changed

#MHM

MHM_summarized <- MHM_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))#good if you need to look at mean temp of a day for a specific sensor


MHM_simple <- MHM_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT2, colour = Park) +
  geom_label_repel(data = MHM_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), strip.background = element_rect(fill = "yellowgreen"))+
  theme(legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)")

MHM_simple_changed <- MHM_simple + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

MHM_simple_changed

#TP

TP_summarized <- TP_subset %>% 
  group_by(Park, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))#good if you need to look at mean temp of a day for a specific sensor

TP_simple <- TP_summarized %>%
  ggplot() +
  aes(x= Park, y= meanT2, colour = Park) +
  geom_label_repel(data = TP_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c("forestgreen", "gray48")) + 
  scale_fill_manual(values = c("forestgreen", "gray48")) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), strip.background = element_rect(fill = "yellowgreen"))+
  theme(legend.position = "none")+
  labs(x ="Sensor Type", y ="Mean Temperature (°C)")

TP_simple_changed <- TP_simple + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

TP_simple_changed


##############################
###INVESTIGATING FRICHE/MHM###
##############################

MHM_subset_investigation <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference")

Friche_data <- subset(MHM_subset_investigation, Sensor_Name == "MHM_4" | Sensor_Name == "MHM_3")

Friche_data_summarized <- Friche_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Friche")

friche_plot <- Friche_data_summarized %>%
  ggplot() +
  aes(x = Date, y = meanT, fill = Park, colour = Park) +
  geom_smooth(method='gam', alpha=0.3, linewidth=1.2) +
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_classic() +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  facet_wrap(vars(DayTime)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))



friche_plot_changed <- friche_plot + scale_color_manual(values=c("forestgreen", "gray48")) + scale_fill_manual(values=c("forestgreen", "gray55"))

friche_plot_changed

##Boise Vimont
Boise_Vimont_data <- subset(MHM_subset_investigation, Sensor_Name == "MHM_1" | Sensor_Name == "MHM_2")

Boise_Vimont_summarized <- Boise_Vimont_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Vimont")

##Boise Steinberg
Boise_Steinberg_data <- subset(MHM_subset_investigation, Sensor_Name == "MHM_5" | Sensor_Name == "MHM_6" | Sensor_Name == "MHM_7")

Boise_Steinberg_summarized <- Boise_Steinberg_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Steinberg")

##REFERENCE
MHM_Reference_data <- subset(MHM_subset_investigation, Sensor_Name == "MHM_Reference")

MHM_Reference_summarized <- MHM_Reference_data %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature),
            maxT = max(Temperature)) %>%
  mutate(Park_Sub_Type = "Reference")


#Combine these

combined <- rbind(Friche_data_summarized, Boise_Vimont_summarized, Boise_Steinberg_summarized, MHM_Reference_summarized)

# ALL ON ONE
#MHM_Seperated_Plot <- combined %>%
#  ggplot() +
#  aes(x = Date, y = meanT, fill = Park_Sub_Type, colour = Park_Sub_Type) +
#  geom_smooth(method='loess', alpha=0, linewidth=1.8) +
#    geom_point(size = 1)+
#  scale_fill_hue(direction = 1) +
#  scale_color_hue(direction = 1) +
#  theme_classic() +
#  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
#  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
#  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
#  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
#  facet_wrap(vars(DayTime)) +
#  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))


MHM_Seperated_Plot

MHM_Seperated_Plot_changed <- MHM_Seperated_Plot + scale_color_manual(values=c("green","green3", "darkgreen", "gray48")) + scale_fill_manual(values=c("green","forestgreen", "darkgreen", "gray48"))

MHM_Seperated_Plot_changed

###PLOTTING TO GET NICE LITTLE VALUES ON GRAPH###
# Plotting geom_smooth
combined %>%
  group_by(Park_Sub_Type) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Park_Sub_Type, fill = Park_Sub_Type)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime))

# create the smooth and retain rows with max of smooth, using slice_max
sm_max = combined %>% 
  group_by(Park_Sub_Type, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.5))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

# Plot, using the same smooth as above (default is loess, span set at set above)
labeled <- combined %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Park_Sub_Type, color = Park_Sub_Type)) +
  geom_smooth(method = "loess", span=.5, se=F, linewidth=1.8) +
  #geom_label_repel(data = sm_max, aes(y=smooth, label= round(smooth,1), color = Park_Sub_Type)) + 
  geom_label_repel(data = sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Park_Sub_Type)), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic() + 
  scale_color_manual(values = c( '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2))+ 
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) + 
  theme(strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"))+
  facet_wrap(vars(DayTime)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))

labeled

#ggsave("MHM_ParkSubType_Plot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


#labeled_changed <- labeled + scale_color_manual(values=c("green","green3", "darkgreen", "gray48")) + scale_fill_manual(values=c("green","forestgreen", "darkgreen", "gray48"))
#not sure if im a fan of this colour scheme 
labeled_changed

labeled

#MetBrewer::display_all(colorblind_only = TRUE) to view colours in MetBrewer

# ALL IN ONE SIMPLE

MHM_Sep_Simple_summarized <- combined %>% 
  group_by(Park_Sub_Type, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))

MHM_Sep_Simple_Plot <- MHM_Sep_Simple_summarized %>%
  ggplot() +
  geom_label_repel(data = MHM_Sep_Simple_summarized, aes(y=meanT2, label= round(meanT2,1), fill = factor(Park_Sub_Type)), fontface = 'bold', color = 'white', show.legend = F) + 
  aes(x= Park_Sub_Type, y= meanT2, colour = Park_Sub_Type) +
  theme_bw()+
  scale_color_manual(values = c( '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) +
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT2-SD, ymax=meanT2+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), strip.background = element_rect(fill = "yellowgreen"))+
  theme(legend.position = "none")+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  labs(x ="Park Sub Type", y ="Mean Temperature (°C)")

MHM_Sep_Simple_Plot

#ggsave("MHM_ParkSubType_SimplePlot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


##############################
###INVESTIGATING TECHNOPARK###
##############################

technopark_subset <- subset(edited_temp_data, Park == "TP" | Park == "TP_Reference")

technopark_summary <- technopark_subset %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

technopark_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

technopark_sm_max = technopark_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_technopark_plot <- technopark_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = technopark_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic() + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_technopark_plot

#ggsave("TP_All_Sensors_Plot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


##############################
#####INVESTIGATING CdesP######
##############################

CdesP_subset2 <- subset(edited_temp_data, Park == "CdesP" | Park == "CdesP_Reference")

CdesP_subset2_summary <- CdesP_subset2 %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

CdesP_subset2_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

CdesP_subset2_sm_max = CdesP_subset2_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_CdesP_plot <- CdesP_subset2_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = CdesP_subset2_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic() + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_CdesP_plot

#ggsave("CdesP_All_Sensors_Plot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

##############################
####INVESTIGATING FALAISE#####
##############################

falaise_subset2 <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference")

falaise_subset2_summary <- falaise_subset2 %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

falaise_subset2_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

falaise_subset2_sm_max = falaise_subset2_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_falaise_plot <- falaise_subset2_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = falaise_subset2_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic() + 
  scale_color_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  scale_fill_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_falaise_plot

#ggsave("Falaise_All_Sensors_Plot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

##############################
######INVESTIGATING MHM#######
##############################

MHM_subset2 <- subset(edited_temp_data, Park == "MHM" | Park == "MHM_Reference")

MHM_subset2_summary <- MHM_subset2 %>%
  group_by(DayTime, Date, Sensor_Name) %>%
  summarize(meanT = mean(Temperature))

MHM_subset2_summary %>%
  group_by(Sensor_Name) %>%
  mutate(is_max = meanT == max(meanT)) %>%
  ggplot(mapping = aes(Date, meanT, color = Sensor_Name, fill = Sensor_Name)) +
  geom_smooth(method = "loess") +
  geom_point(size = 3) +
  facet_wrap(vars(DayTime)) 

MHM_subset2_sm_max = MHM_subset2_summary %>% 
  group_by(Sensor_Name, DayTime) %>%
  mutate(smooth =predict(loess(meanT ~ as.numeric(Date), span=.6))) %>%
  slice_max(order_by = smooth) %>%
  distinct(smooth, .keep_all =  T)

labeled_MHM_plot <- MHM_subset2_summary %>%
  ggplot(combined, mapping = aes(Date, meanT, group = Sensor_Name, color = Sensor_Name)) +
  geom_smooth(method = "loess", span=.6, se=F, linewidth=2.5) +
  geom_label_repel(data = MHM_subset2_sm_max, aes(y=smooth, label= round(smooth,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  theme_classic() + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  facet_wrap(~ DayTime) +
  labs(x= "Date (Month)", y= "Mean Daily Temperature (°C)") +
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 12, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))+
  facet_wrap(vars(DayTime))

labeled_MHM_plot

#ggsave("MHM_All_Sensors_Plot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

# library(colorblindr)
cvd_grid(labeled_technopark_plot)

#########################
##Simple of all sensors##
#########################

##Falaise##

falaise_simple_sum <- falaise_subset2 %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE))


falaise_simple_all <- falaise_simple_sum %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_label_repel(data = falaise_simple_sum, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) +
  scale_fill_manual(values = c('#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')) +
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        legend.position = "none")+
  labs(x ="Sensor Name", y ="Mean Temperature (°C)")

falaise_simple_all

#ggsave("Falaise_All_Sensors_SimplePlot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


##MHM##

MHM_simple_sum <- MHM_subset2 %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE))

MHM_simple_all <- MHM_simple_sum %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_label_repel(data = MHM_simple_sum, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB')) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        legend.position = "none")+
  labs(x ="Sensor Name", y ="Mean Temperature (°C)")

MHM_simple_all

#ggsave("MHM_All_Sensors_SimplePlot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


##CdesP##

CdesP_simple_sum <- CdesP_subset2 %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE))

CdesP_simple_all <- CdesP_simple_sum %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_label_repel(data = CdesP_simple_sum, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        legend.position = "none")+
  labs(x ="Sensor Name", y ="Mean Temperature (°C)")

CdesP_simple_all

#ggsave("CdesP_All_Sensors_SimplePlot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")


##TP##

TP_simple_sum <- technopark_subset %>% 
  group_by(Sensor_Name, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature),
            SD = sd(Temperature, na.rm = TRUE))

TP_simple_all <- TP_simple_sum %>%
  ggplot() +
  aes(x=Sensor_Name , y= meanT, colour = Sensor_Name) +
  geom_label_repel(data = TP_simple_sum, aes(y=meanT, label= round(meanT,1), fill = factor(Sensor_Name)), fontface = 'bold', color = 'white', show.legend = F) + 
  scale_color_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  scale_fill_manual(values = c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB', '#000000')) + 
  theme_bw()+
  geom_point(size = 3)+ 
  geom_errorbar(aes(ymin=meanT-SD, ymax=meanT+SD), width=.2, linewidth=1, 
                position=position_dodge(0.05))+
  facet_wrap(vars(DayTime))+
  theme(strip.text = element_text(size= 12, face="bold", colour = "black"), 
        strip.background = element_rect(fill = "yellowgreen"),
        legend.position = "none")+
  labs(x ="Sensor Name", y ="Mean Temperature (°C)")

TP_simple_all

#ggsave("TP_All_Sensors_SimplePlot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")



####HOTEST DAYS####

hottest_days_sub <- subset(edited_temp_data, Temperature >= 30.0 & DayTime == "Day")

Friche_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_4" | Sensor_Name == "MHM_3")

Friche_hottest_summarized <- Friche_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Friche")

Boise_Vimont_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_1" | Sensor_Name == "MHM_2")

Boise_Vimont_hottest_summarized <- Boise_Vimont_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Vimont")

Boise_Steinberg_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_5" | Sensor_Name == "MHM_6" | Sensor_Name == "MHM_7")

Boise_Steinberg_hottest_summarized <- Boise_Steinberg_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Boise_Steinberg")

MHM_Reference_hottest <- subset(hottest_days_sub, Sensor_Name == "MHM_Reference")

MHM_Reference_hottest_summarized <- MHM_Reference_hottest %>%
  group_by(DayTime, Park, Date) %>%
  summarize(meanT = mean(Temperature)) %>%
  mutate(Park_Sub_Type = "Reference")

hottest_combined <- rbind(Friche_hottest_summarized, Boise_Vimont_hottest_summarized, Boise_Steinberg_hottest_summarized, MHM_Reference_hottest_summarized)

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

#ggsave("TP_All_Sensors_SimplePlot.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")

#not really interesting.. seems like on the really hot days it was just hot everywhere

###HOTTEST BUT WITH ALL SITES###

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

#ggsave("Hottest_Roughdraft.tiff", path = "Graphics", dpi = 600, width = 35, height = 20, units = "cm")
#very interesting looking plot but not very useful

MHM_Sep__hottest_Simple_summarized <- hottest_combined %>% 
  group_by(Park_Sub_Type, DayTime) %>% #groups the data in these groups to perform the following operations
  summarize(meanT2 = mean(meanT),
            SD = sd(meanT, na.rm = TRUE))

#### Last look at hottest temp July 6th (2nd hottest day of the summer, hottest day when all
#### sensors were up)

edited_temp_data2 <- edited_temp_data %>%
  separate(Date, c("Year", "Month", "Day"), sep="-", remove = FALSE) #sep can split a column into two using a value, in this case " " is giving [space] as what wil split it


july6_subset <- subset(edited_temp_data2, Month == "07" & Day == "06" & DayTime == "Day")


##Falaise##

july6_falaise <- subset(july6_subset, Park == "Falaise" | Park == "Falaise_Reference")

july6_falaise_sum <- july6_falaise %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_falaise_plot <- july6_falaise_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line()+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "Falaise - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_falaise_plot

#ggsave("Falaise_July6_Unfinished.tiff", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


##MHM##

july6_MHM <- subset(july6_subset, Park == "MHM" | Park == "MHM_Reference")

july6_MHM_sum <- july6_MHM %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_MHM_plot <- july6_MHM_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line()+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "MHM - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_MHM_plot

#ggsave("MHM_July6_Unfinished.tiff", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


##CdesP##

july6_CdesP <- subset(july6_subset, Park == "CdesP" | Park == "CdesP_Reference")

july6_CdesP_sum <- july6_CdesP %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_CdesP_plot <- july6_CdesP_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line()+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "CdesP - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_CdesP_plot

#ggsave("CdesP_July6_Unfinished.tiff", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")


##TP##

july6_TP <- subset(july6_subset, Park == "TP" | Park == "TP_Reference")

july6_TP_sum <- july6_TP %>% 
  group_by(Park, Time) %>% #groups the data in these groups to perform the following operations
  summarize(meanT = mean(Temperature)) 


july6_TP_plot <- july6_TP_sum %>%
  ggplot()+
  (aes(x= Time, y= meanT, colour = Park))+
  geom_line()+
  theme_classic() + 
  labs(x= "Time (Hour)", y= "Mean Hourly Temperature (°C)", title = "TP - July 6th") +
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), legend.box.background = element_rect(colour = "black", linewidth = 1)) 

july6_TP_plot

#ggsave("TP_July6_Unfinished.tiff", path = "Graphics", dpi = 600, width = 20, height = 20, units = "cm")
