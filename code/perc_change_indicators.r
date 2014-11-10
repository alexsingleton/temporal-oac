##################################################################################
# Change over time (2002 - 2010)
# We are interested in chnage from 2002 annually
# Data: House frequency of transactions, Tax Bands ABCD, Total Pop
##################################################################################
source(file = "/media/mick/1BAE-C95B/liverpool/r_script/functions.r")

# First Council Tax Bands
dir <- "/media/mick/My Passport/data/tax_bands_oa_11/"
for (year in 2001:2010){
  df_name <- paste("bands", year, sep = "_")
  Df <- read.csv(paste(dir, df_name, ".csv", sep = ""))
  col_name <- paste("abcd", year, sep = "_")
  Df[[col_name]] <- rowSums(Df[,c("band_a", "band_b", "band_c", "band_d")])
  if (year == 2001){
    bands <- data.frame(OA11CD = Df[,1], abcd_2001 = Df[,"abcd_2001"])
  } else {
    bands <- merge(bands, Df[,c("OA11CD", col_name)], all.x = T)
  }
}
# 677 NAs for 2005 and 2 NAs for 2010
# Fill the NAs using previous years records
bands$abcd_2005 <- with(bands, ifelse(is.na(abcd_2005), abcd_2004, abcd_2005))
bands$abcd_2010 <- with(bands, ifelse(is.na(abcd_2010), abcd_2009, abcd_2010))

bands_perc <- data.frame(OA11CD = bands$OA11CD)
bands_perc <- cbind(bands_perc, as.data.frame(sapply(bands[,3:ncol(bands)], perc_change_from_ref_year, bands[,2])))

# For 2001 use census population data
pop_01 <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/Census_2001_2011OA.csv")[,1:2]
names(pop_01)[2] <- "total_2001"
# Then Mid-year pop estimates
setwd("/media/mick/My Passport/data/proxies/pop_zip/raw")
pops <- list.files(".", "*oa11.csv")
year <- 2002
for (csv_file in pops){
  Df <- read.csv(csv_file)
  col_name <- paste("total", year, sep = "_")
  Df[[col_name]] <- rowSums(Df[,2:ncol(Df)])
  if (year == 2002){
    pop <- data.frame(OA11CD = Df[,1], total_2002 = Df[[col_name]])
  } else {
    pop <- merge(pop, Df[,c("OA11CD", col_name)], by = "OA11CD", all.x = T)
  }
  year <- year + 1
}
# There is 5 NAs for each year we will use the 2001 entry to replace them
pop <- merge(pop_01, pop, by = "OA11CD", all.x = T)
for (i in 3:11){
  pop[,i] <- ifelse(is.na(pop[,i]), pop$total_2001, pop[,i])
}

pop_perc <- data.frame(OA11CD = pop[,1])
pop_perc <- cbind(pop_perc, as.data.frame(sapply(pop[,3:ncol(pop)], perc_change_from_ref_year, pop[,2])))

# And finally frequency of house transactions
setwd("/media/mick/My Passport/data/price_paid")
sales_freq <- read.csv("pp_stats.csv")[,c(1,seq(2,41,4))]
for (i in 2:ncol(sales_freq)){
  sales_freq[,i] <- ifelse(is.na(sales_freq[,i]), 0, sales_freq[,i])
}
sales_freq_perc <- data.frame(OA11CD = sales_freq[,1])
sales_freq_perc <- cbind(sales_freq_perc, as.data.frame(sapply(sales_freq[,3:ncol(sales_freq)], perc_change_from_ref_year, sales_freq[,2])))

all_data <- merge(bands_perc, pop_perc, by = "OA11CD")
all_data <- merge(all_data, sales_freq_perc, by = "OA11CD")
write.csv(all_data, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change/raw_indicators_perc.csv", row.names = F)

all_data_std <- as.data.frame(sapply(all_data[,2:ncol(all_data)], scale))
all_data_std2 <- as.data.frame(sapply(all_data[,2:ncol(all_data)], fun_std2))

all_data_comp <- data.frame(OA11CD = all_data[,1])

year <- 2002
for (i in 1:9){
  
  all_data_comp[[paste("indicator", year, sep = "_")]] <- rowSums(all_data_std[,c(i,i+9,i+18)])
  
  year <- year + 1
}
write.csv(all_data_comp, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change/comp_indicator.csv", row.names = F)

##################### Classify indicators to 3 classes (High, Moderate, Low) using the quartiles ##################

# comp_class <- data.frame(OA11CD = all_data_comp[,1])
# comp_class <- cbind(comp_class, as.data.frame(sapply(all_data_comp[,2:ncol(all_data_comp)], recl_with_quant, c(0,0.25,0.75,1))))
# 
# write.csv(comp_class, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change/indicators_class.csv", row.names = F)

##################### Put everything in a shapefile ################################
# library(rgdal)
# 
# comp_class <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change/indicators_class.csv")
# 
# OA_2011 <- readOGR("/media/mick/My Passport/backup/GIS_Data/OAs_2011","OA_2011_England")
# 
# OA_2011@data <- data.frame(OA_2011@data, comp_class[match(OA_2011@data[,"OA11CD"], comp_class[,"OA11CD"]), ])
# OA_2011@data$OA11CD.1 <- NULL
# 
# writeOGR(OA_2011, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change", "indicators_change", driver="ESRI Shapefile")
###########################################################################################################

OA_2011 <- readOGR("/media/mick/My Passport/backup/GIS_Data/OAs_2011","OA_2011_England")

comp_values <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change/comp_indicator.csv")

OA_2011@data <- data.frame(OA_2011@data, comp_values[match(OA_2011@data[,"OA11CD"], comp_values[,"OA11CD"]), ])
OA_2011@data$OA11CD.1 <- NULL

writeOGR(OA_2011, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change", "comp_indicator", driver="ESRI Shapefile")

###############################################################################################################

OA_2011 <- readOGR("/media/mick/My Passport/backup/GIS_Data/OAs_2011","OA_2011_England")

ind_values <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change/raw_indicators_perc.csv")

OA_2011@data <- data.frame(OA_2011@data, ind_values[match(OA_2011@data[,"OA11CD"], ind_values[,"OA11CD"]), ])
OA_2011@data$OA11CD.1 <- NULL

writeOGR(OA_2011, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/indicators_change", "indicators_perc", driver="ESRI Shapefile")
