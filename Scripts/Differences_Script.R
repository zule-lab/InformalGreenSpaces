################################################################################
#################### R SCRIPT FOR THE DIFFERENCES GRAPH ########################
################################################################################

#RUN INTRO SCRIPT FIRST!

wide_summary <- pivot_wider(summarized, names_from = Park, values_from = c(maxT, meanT)) #pivot_wider allows us to perform the difference by making the data frame
#wide instead of long. So now each date has a value for each park! 

wide_summary_difference <- wide_summary %>% #now we mutate the wide data set with the differences. Reference - Park to get +'ve values
  mutate(Falaise_Mean_Difference = meanT_Falaise_Reference - meanT_Falaise) %>%
  mutate(CdesP_Mean_Difference = meanT_CdesP_Reference - meanT_CdesP) %>%
  mutate(MHM__Mean_Difference = meanT_MHM_Reference - meanT_MHM) %>%
  mutate(TP_Mean_Difference = meanT_TP_Reference - meanT_TP) %>% 
  pivot_longer(cols = c(Falaise_Mean_Difference, CdesP_Mean_Difference, MHM__Mean_Difference, TP_Mean_Difference), values_to = 'temp') %>% #pivot back to be able to plot the data
  select(c(Date, DayTime, name, temp))#selects specified columns to keep or drop in the final data frame

Differences_Plot <- ggplot(wide_summary_difference, aes(x=Date, y = temp, colour = DayTime)) + 
  geom_line(linewidth = 1.5) + 
    labs(x ="Date (Month)", y ="Temperature (Mean Difference in °C)")+
  theme(legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(),
        legend.box.background = element_rect(colour = "black", linewidth = 2))+
  scale_colour_met_d('Egypt')+
  facet_wrap(~ name)+
  geom_hline(yintercept = 0, linetype = 5, colour = 'purple4', show.legend =T, linewidth = 1)+
  theme_classic(base_size = 15)+
  theme(axis.title.x = element_text(vjust = -1.5), axis.title.y = element_text(vjust = 2),
        legend.position = "bottom", legend.background = element_rect("white"), legend.title = element_blank(), 
        legend.box.background = element_rect(colour = "black", linewidth = 1),
        strip.text = element_text(size = 18, face="bold"), strip.background = element_rect(fill = "yellowgreen", colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 0.5))
  
Differences_Plot

#ggsave("Difference_Plot_All_Sites.tiff", plot=Differences_Plot, path = "Graphics", dpi = 600, width = 25, height = 17, units = "cm")

 