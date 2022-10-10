#############################################
# Purpose: Defines necessary functions      #      
#          to run the graph in section 4    #
#          of the shiny application         #
# Date:    02.06.2020                       #
# Authors: Matthias Niggli, CIEB/Uni Basel  #
#          Christian Rutzer, CIEB/Uni Basel #
#############################################

weighting_fun <- function(thres){
        
        # discard NOGAs that do not have at least 30 observations in the SAKE data
        NOGA_discard <- Greenness_Shortage_ISCO_NOGA %>% 
                group_by(NOGA2digit) %>% 
                summarise(n_obs = n()) %>% 
                filter(n_obs < 30) %>%
                select(NOGA2digit)
        NOGA_discard <- NOGA_discard$NOGA2digit
        if(length(NOGA_discard) > 0){
                Greenness_Shortage_ISCO_NOGA <- subset(Greenness_Shortage_ISCO_NOGA, !NOGA2digit %in% NOGA_discard)
        }

        # subset the data according to threshold & convert grouping variables
        Greenness_Shortage_ISCO_NOGA <- subset(Greenness_Shortage_ISCO_NOGA, green >= thres)
        Greenness_Shortage_ISCO_NOGA[,c("isco","NOGA2digit", "Region")] <- sapply(Greenness_Shortage_ISCO_NOGA[,c("isco","NOGA2digit", "Region")],
                                                                                  as.character)
        
        # calculate isco weights and employment shares at the national level
        weight_national <- Greenness_Shortage_ISCO_NOGA%>%
                group_by(NOGA2digit, isco)%>%
                summarise(ISCO_count = sum(Gewicht))
        tmp <- Greenness_Shortage_ISCO_NOGA %>%
                group_by(NOGA2digit)%>%
                summarise(NOGA_count = sum(Gewicht))
        
        weight_national <- merge(weight_national, tmp, by = "NOGA2digit")
        weight_national <- mutate(weight_national, weight = ISCO_count / NOGA_count, Region = "Schweiz")
        weight_national <- weight_national[, c(6,1:5)]
        
        # calculate isco weights at the cantonal level
        weight_cantonal <- Greenness_Shortage_ISCO_NOGA%>%
                group_by(Region, NOGA2digit, isco)%>%
                summarise(ISCO_count = sum(Gewicht))
        tmp <- Greenness_Shortage_ISCO_NOGA %>%
                group_by(Region, NOGA2digit)%>%
                summarise(NOGA_count = sum(Gewicht))
        
        weight_cantonal <- merge(weight_cantonal, tmp, by = c("Region", "NOGA2digit"))
        weight_cantonal <- mutate(weight_cantonal, weight = ISCO_count / NOGA_count)
        
        # return all calculated isco weights in one dataframe:
        SAKE_weights <- rbind(weight_national, weight_cantonal)
        
        return(SAKE_weights)
}

# Now define a second function that calculates the following:
#       1) employment-weighted industry-level shortages of green jobs (accordings to some threshold for green jobs)
#       2) the combined employment share of these green jobs at the industry level (accordings to some threshold for green jobs)
#       3) add trade and CO2-equivalents emission data

plot_data_fun <- function(thres, Regionen = "Schweiz", NOGAs){
        
        # define green jobs according to threshold 
        green_jobs <- subset(Greenness_Shortage_ISCO_NOGA, green >= thres)
        green_jobs <- green_jobs[!duplicated(green_jobs$isco), ]
        
        # TEST: print a warning message if not all isco weights sum to 1 -----------------
        test <- weighting_fun(thres = thres)
        test <- test %>% group_by(Region, NOGA2digit) %>% summarise_at(.vars = "weight", .funs = sum)
        if(unique(round(test$weight, 1)) != 1){warning("Sectoral ISCO weights do not sum to 1!")}
        
        # calculate combined employment share of green_jobs at the industry-level per Region:
        emp_shares <- weighting_fun(thres = 0)
        emp_shares$green_job <- ifelse(emp_shares$isco %in% unique(green_jobs$isco), 1, 0)
        emp_shares <- emp_shares %>% 
                group_by(Region, NOGA2digit) %>% 
                mutate(green_shares = green_job * weight) %>% 
                summarise(green_emp_share = sum(green_shares))
        
        # calculate weights for aggregating the shortage of green_jobs at the industry-level per Region according to some threshold
        SAKE_weights <- weighting_fun(thres = thres)
        
        # combine these weights with shortage_index, green employment shares and 'Handelbarkeit' in one data.frame for plotting:
        SAKE_weights <- merge(SAKE_weights, 
                              green_jobs[, c("isco", "green", "shortage_index", "shortage_index_norm")],
                              by = "isco", all.x = TRUE)
        plot_data <- merge(SAKE_weights, emp_shares, by = c("Region", "NOGA2digit"), all.x = TRUE)
        plot_data <- merge(plot_data, Handelbarkeit, by = "NOGA2digit")
        
        # subset to the Region and industries to plot:
        plot_data <- filter(plot_data, Region %in% Regionen & NOGA2digit %in% NOGAs )
        
        # calculate weighted shortage at the isco-level
        plot_data$WeightedShortageGreen <- plot_data$weight*plot_data$shortage_index
        plot_data$WeightedShortageGreen_norm <- plot_data$weight*plot_data$shortage_index_norm
        
        # aggregate shortage_index at the industry-level (i.e. a weighted sum of isco-level shortages):
        plot_data <- plot_data %>%
                group_by(Region, NOGA2digit) %>%
                summarise(WeightedShortageGreen = sum(WeightedShortageGreen, na.rm = TRUE),
                          WeightedShortageGreen_norm = sum(WeightedShortageGreen_norm, na.rm = TRUE),
                          Handelbarkeit = mean(Handelbarkeit, na.rm=TRUE),
                          green_emp_share = mean(green_emp_share, na.rm = TRUE),
                          GHG_per_ValueAdded = mean(GHG_per_ValueAdded, na.rm = TRUE))
        
        return(plot_data)
}
