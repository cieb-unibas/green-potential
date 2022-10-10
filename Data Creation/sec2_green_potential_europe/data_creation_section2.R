############################################
# Purpose: Use this script to generate the #
#          map in the web report.          #
# Date:    07.06.2020                      #
# Authors: Christian Rutzer, CIEB/Uni Basel#
#          Matthias Niggli, CIEB/Uni Basel #
############################################

############# load packages ############# 
require("dplyr")
require("ggplot2")
require("data.table")
require("plyr")
library("viridis")
library('scales')
library("rworldmap")
library("maps")
library("countrycode")


############# set directories #############
# (1) make sure current directory is the repository directory ("green potential")
getwd()

# (2) ELFS data is confidential. If you have access to it, set "ELFS_path" to where the data is stored
ELFS_path <- "xxxxxxxxxxxxxxxxxxxxxxxx"

############# load and process the data #############

## Load data of green potential estimates at 3-digit ISCO level:
lasso.pred.table <- read.csv2("Data Creation/sec2_green_potential_europe/green_three_isco.csv")

## Following data has to saved on the wwz-shared driver due to privacy and security contract with the BFS 
dat.2011 <- readRDS(ELFS_path)

dat.2011$num.new <- 1
setDT(dat.2011)[, ges := sum(num.new), list(isco, iso, year, emp)]
dat.2011 <- mutate(dat.2011, y.2012 = case_when(emp == 1 & year == 2012 & ges >= 20 ~ 1, TRUE ~ 0), y.2016 = case_when(emp == 1 & year == 2016 & ges >= 20 ~ 1, TRUE ~ 0))
setDT(dat.2011)[, test.20 := y.2012[year == 2012]*y.2016, list(isco, iso)]
setDT(dat.2011)[, test.2012.2016 := sum(test.20), list(isco, iso)]
dat.2011 <- filter(dat.2011, test.2012.2016 >= 1) %>% dplyr::select(-ges, -y.2012, -y.2016,-test.20,-test.2012.2016)

dat.2011 <- filter(dat.2011, !(isco %in% c("-5", "00", "010", "011", "021", "031")))
dat.2011 <- mutate(dat.2011, isco = as.character(isco))
lasso.pred.table <- mutate(lasso.pred.table, three = as.character(three), lasso.ohne.weight = as.numeric(as.character(lasso.ohne.weight)), lasso.econ.weight = as.numeric(as.character(lasso.econ.weight)))

dat_agg <- aggregate(num ~ isco + iso + year + sector, data = filter(dat.2011, age < 68 & year == 2016), FUN = sum) 
dat_agg <- left_join(dat_agg, lasso.pred.table, by = c("isco" = "three"))
dat_agg <- filter(dat_agg, is.na(lasso.econ.weight) != T)

share_green <- function(cut_off){
dat_agg <- mutate(dat_agg, green = ifelse(lasso.econ.weight >= cut_off, "green", "non.green"))
dat_agg_tot <- aggregate(num ~ iso + year + green, FUN = sum, data = dat_agg)
dat_agg_tot <- dcast(dat_agg_tot, iso + year ~ green, value.var = c("num"))
dat_agg_tot <- mutate(dat_agg_tot, share_green = green / (green + non.green), cut_off = cut_off)
return(dat_agg_tot)
}

dat_fin <- do.call(rbind, lapply(seq(0.4, 0.8, .05), function(x) share_green(x)))
dat_fin <- mutate(dat_fin, iso = countrycode(iso, "iso2c", "iso3c"))
dat_fin$iso[is.na(dat_fin$iso) == T] <- "GBR" 
dat_fin <- filter(dat_fin, iso != "ROU")

## Add countries shown in the map but having no green potential value 
c_added <- as.data.frame(cbind(do.call(rbind, replicate(length(unique(dat_fin$cut_off)), as.matrix(data.frame(iso = c("KOV", countrycode(c("PL","BY", "LU","UA","RS","BA","HR","AL","BG","RO","SI","ME","MD","MK"), "iso2c", "iso3c")), share_green = NA)), simplify = FALSE)), cut_off = rep(seq(0.4, 0.8, .05), each = 15))) %>% mutate(cut_off = as.numeric(as.character(cut_off)), share_green = as.numeric(as.character(share_green)))

dat_fin <- rbind.fill(dat_fin, c_added)

### Create data for EU map of green potential 
world_map <- map_data("world")
world_map <- mutate(world_map, iso = countrycode(region, "country.name.en", "iso3c"))
world_map[world_map$region=="Kosovo","iso"]<-"KOV"
world_map <- filter(world_map, !(region %in% c("Antarctica", "Greenland",
                                               "French Southern and Antarctic Lands")) &
                            !subregion %in% c("Ile d'Oleron","Svalbard","Jan Mayen"))
countries<-as.character(unique(dat_fin$iso))
countries<-c(countries,"KOV")
eu_map <- filter(world_map, iso %in% countries)

## Add map and green potential together
plot.data <- left_join(eu_map, dat_fin, by = "iso", all.y = T)
plot.data <- filter(plot.data, is.na(iso) != T) 
# saveRDS(plot.data, paste0(getwd(), "/Report/data_section2.rds"))


