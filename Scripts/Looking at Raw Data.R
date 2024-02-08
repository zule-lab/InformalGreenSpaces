
 Falaise_subset <- subset(edited_temp_data, Park == "Falaise" | Park == "Falaise_Reference") 
 
 Falaise_1 <- subset(falaise_subset, Sensor_Name == "Falaise_1")
 
 Falaise_1_summary <- Falaise_1 %>%
   group_by(Date, DayTime) %>%
   summarize(meanT = mean(Temperature))
 
 Falaise_1_summary %>%
   ggplot()+
   geom_line(aes(x=Date , y= meanT))+
   theme_classic()
