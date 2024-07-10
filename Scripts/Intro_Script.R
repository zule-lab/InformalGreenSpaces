################################################################################
############################## INTRO R SCRIPT ##################################
################################################################################

      ## HAS BOTH ENGLISH AND FRENCH CODE FOR RUNNING THE OTHER SCRIPTS ##

#first install the following packages, all of these should be used in the code
#(some might not if the code is commented out or if it was deleted in this final version)

#install.packages(c("dplyr","tidyr","stringr","ggplot2","chron","viridis","esquisse","patchwork","mgcv", "MetBrewer", "ggrepel", "plotrix"))

#Time to load the packages
library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")
library("chron")
library("esquisse")
library("patchwork")
library("MetBrewer")
library("gghighlight")
library("ggrepel")
library("mgcv")
library("plotrix")
library("readr")

#The following colour codes is the colourblind-friendly palette I've compiled for this project 
#the last two colours I've reserved for reference sensors, the other are used for the park sensors if there are multiple displayed on one graph
c('#4477AA', '#EE6677', '#228833', '#CCBB44', '#66CCEE', '#AA3377', '#8100CC', '#BBBBBB', '#000000')


##Time to load up the data!

##### ENGLISH #####

#This code sets the base language of R to something, in this case English if you are in French
Sys.setlocale(category = "LC_ALL", "English")

temperature_data <- read.csv("Input/Compiled_Data2023_English.csv")
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

#This code sets the base language of R to something, in this case french for the french graphs
Sys.setlocale(category = "LC_ALL", "French")

guess_encoding("Input/Compiled_Data2023_French.csv") #Needed to obtain the type of encoding needed to make the accented e's work

temperature_data_french <- read.csv("Input/Compiled_Data2023_French.csv", encoding = "ASCII-1") #ASCII-1 is the encoding type gotten from above


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

###########################

#esquisser(summarized) #view the data and manipulate it which is pretty cool, thanks Rikka!  

