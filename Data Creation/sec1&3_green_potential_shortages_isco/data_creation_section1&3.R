############################################
# Purpose: Use this script to get the data #      
#          on green potential and shortages#
#          of occupations in the report.   #
# Date:    05.06.2020                      #
# Authors: Matthias Niggli, CIEB/Uni Basel #
#          Christian Rutzer, CIEB/Uni Basel# 
############################################


############# load packages ############# 
library("dplyr")
library("stringr")

############# set directories #############
# (1) make sure current directory is the repository directory ("green potential")
getwd()


############# load and process the data #############
## Read green potential and shortage of isco occupations
isco_list <- read.csv2("Data Creation/sec1&3_green_potential_shortages_isco/isco_list.csv")

## save a temporary data.frame with green potential only
isco_green <- select(isco_list, -index1)
colnames(isco_green) <- c("ISCO", "Occupation", "Estimated Green Potential")

## Clean the data frame
isco_list <- isco_list[complete.cases(isco_list), ]
isco_list <- mutate(isco_list, Title = ifelse(str_detect(index1, "\\(") == T, paste0(Title, "*"), as.character(Title))) %>% mutate(index1 = gsub("\\(|\\)", "", index1)) %>% mutate(index1 = gsub(",", ".", index1))
colnames(isco_list) <- c("ISCO", "Occupation", "Estimated Green Potential", "Shortage Indicator")
isco_list$ISCO <- as.character(isco_list$ISCO)

## save the list of data.frames in a list
isco_list <- list(isco_green = isco_green, isco_shortage = isco_list)
#isco_list %>% saveRDS(paste0(getwd(), "/Report/data_section1&3.RDS"))

