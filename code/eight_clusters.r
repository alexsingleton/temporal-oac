
########################################################################################################
# Estimate new values for the period 2002 - 2010                                                       #
# Perform cluster analysis using the estimated values                                                  #
# Assign clusters to OAs and check temporal drift                                                      #
# Develop indicators of uncertainty                                                                    #
########################################################################################################

# Load libraries
library(rgdal)
library(RColorBrewer)
library(maptools)
library(ggplot2)
library(gridExtra)

#################################################################################################################
# Function to calculate the centroids when the clusters are known
get_centroids <- function(df, cluster){
  centroids <- as.data.frame(aggregate(df, list(cluster = cluster), mean))
  return(centroids)
}
##################################################################################################################

# Import data
all_data <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/all_data.csv")

data_01 <- all_data[all_data$year == 2001,]
data_01 <- data_01[,1:56]
data_01 <- data_01[order(data_01$OA),]

data_11 <- all_data[all_data$year == 2011,]
data_11 <- data_11[,1:56]
data_11 <- data_11[order(data_11$OA),]

#################################################################################################################
# Create new data for the intercensal period
# We assume that the difference  between 2001 - 2011 values increments annually over equal intervals
# We only need to divide the difference between 2011 and 2001 values by 10 to calculate the annual difference
increment <- (data_11[,2:56] - data_01[,2:56]) / 10

start_values <- data_01[,2:56]

OA <- data_01[,1]

for (year in 2002:2010){
  new_df_name <- paste("data",year,sep="_")
  start_values <- start_values + increment
  write.csv(cbind(OA, start_values), paste("/media/mick/My Passport/backup/cluster_analysis/2001_2011/input_data/", new_df_name,".csv",sep=""), row.names=F)
#  assign(new_df_name, start_values)
}

write.csv(data_01, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/input_data/data_2001.csv", row.names = F)
write.csv(data_11, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/input_data/data_2011.csv", row.names = F)
#########################################################################################################
# Perform Cluster Analysis for the period 2001 - 2011                                                   #
#########################################################################################################

setwd("/media/mick/My Passport/backup/cluster_analysis/2001_2011/input_data")

csv_files <- list.files(".")

std_range <- function(x){(x-min(x))/(max(x)-min(x))}

year = 2001
for (i in csv_files){
  Df <- read.csv(i)
  OA <- Df[,1]
  Df <- log(Df[,2:56]+sqrt(Df[,2:56]^2+1))
  Df <- apply(Df, 2, std_range)
  fit <- NA
  for (i in 1:100){
    print(paste("starting run", i, sep=" "))
    class_8 <- kmeans(Df, centers=8, iter.max=100000, nstart=1)
    fit[i] <- class_8$tot.withinss
    if (fit[i] < min(fit[1:(i-1)])){
      SuperGroup_Final <- class_8}
  }
  if (year == 2001){
    out <- as.data.frame(OA)
    out[[paste("cluster", year, sep = "_")]] <- SuperGroup_Final$cluster
  } else {
    temp <- as.data.frame(OA)
    temp[[paste("cluster", year, sep = "_")]] <- SuperGroup_Final$cluster
    out <- merge(out, temp, by = "OA")
  }
  year <- year + 1
}

########################################################################################################
# Match Clusters                                                                                       #
########################################################################################################
match_clusters <- function(array1, array2){
  
  table_of_arrays <- table(array1, array2)
  
  max_in_table <- function(x){
    return(which(x == max(x)))
  }
  max_col <- as.integer(apply(table_of_arrays, 2, max_in_table))
  max_row <- as.integer(apply(table_of_arrays, 1, max_in_table))
  
  # Create a named list
  list_clusters <- NA
  for (i in 1:(length(max_row) - 1)){
    list_clusters <- c(list_clusters, NA)
  }
  # The names are the 2001 clusters
  names(list_clusters) <- row.names(table_of_arrays)
  
  row_index = 1
  for (i in max_row){
    if (row_index == max_col[i]) {
      list_clusters[row.names(table_of_arrays)[row_index]] <- colnames(table_of_arrays)[i]
    }
    row_index <- row_index + 1
  }
  
  # If only one was not assigned, match unassigned pair
  if (sum(is.na(list_clusters)) == 1){
    not_found <- colnames(table_of_arrays)[which(!colnames(table_of_arrays) %in% list_clusters)]
    print(paste("Warning!. The 2002 cluster", not_found, "was one not matched based on crosstabulation"))
    list_clusters[which(is.na(list_clusters))] <- not_found
  } else if (sum(is.na(list_clusters)) >= 2){
    print("Problem! Two or more clusters were not matched")
  }
  
  return(list_clusters)
}


for (year in 2001:2010){
  this_year <- paste("cluster", year, sep = "_")
  next_year <- paste("cluster", year+1, sep = "_")
  matchedClusters <- match_clusters(out[[this_year]], out[[next_year]])
  if (year == 2001){
    matchedClustersDF <- data.frame(cluster_2001 = names(matchedClusters), cluster_2002 = matchedClusters)
  } else {
    temp <- data.frame(names(matchedClusters), matchedClusters)
    names(temp) <- c(this_year, next_year)
    
    matchedClustersDF <- merge(matchedClustersDF, temp, by = this_year)
  }
}

matchedClustersDF <- matchedClustersDF[,paste("cluster", 2001:2011, sep="_")]

# cluster_2001 cluster_2002 cluster_2003 cluster_2004 cluster_2005 cluster_2006 cluster_2007 cluster_2008 cluster_2009 cluster_2010 cluster_2011
# 1            8            8            6            4            2            2            7            2            5            1            6
# 2            3            5            3            8            3            3            5            8            8            2            5
# 3            5            7            2            5            5            8            1            4            2            3            7
# 4            4            6            5            6            1            4            4            7            1            4            1
# 5            2            4            1            7            6            1            8            6            4            5            3
# 6            6            2            4            3            7            7            3            3            6            6            8
# 7            7            3            8            1            4            5            6            1            7            7            2
# 8            1            1            7            2            8            6            2            5            3            8            4

# Now reclassify the clusters data frame

recl_clusters <- function(Df_col, matchedClusters, clusters_col_nr){
  if (Df_col == matchedClusters[1, clusters_col_nr]) return(matchedClusters[1,1])
  else if (Df_col == matchedClusters[2, clusters_col_nr]) return(matchedClusters[2,1])
  else if (Df_col == matchedClusters[3, clusters_col_nr]) return(matchedClusters[3,1])
  else if (Df_col == matchedClusters[4, clusters_col_nr]) return(matchedClusters[4,1])
  else if (Df_col == matchedClusters[5, clusters_col_nr]) return(matchedClusters[5,1])
  else if (Df_col == matchedClusters[6, clusters_col_nr]) return(matchedClusters[6,1])
  else if (Df_col == matchedClusters[7, clusters_col_nr]) return(matchedClusters[7,1])
  else if (Df_col == matchedClusters[8, clusters_col_nr]) return(matchedClusters[8,1])
}

clustersRecl <- out
for (i in 3:12){
  clustersRecl[,i] <- as.integer(sapply(clustersRecl[,i], recl_clusters, matchedClustersDF, i - 1))
}
write.csv(clustersRecl, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/clusters.csv", row.names = F)

# cor(clustersRecl[,2:12])
#              cluster_2001 cluster_2002 cluster_2003 cluster_2004 cluster_2005 cluster_2006 cluster_2007 cluster_2008 cluster_2009 cluster_2010 cluster_2011
# cluster_2001    1.0000000    0.9474246    0.9121303    0.8820075    0.8554353    0.8311019    0.8083397    0.7845501    0.7610456    0.7388046    0.7178283
# cluster_2002    0.9474246    1.0000000    0.9621479    0.9303578    0.9020509    0.8760958    0.8513675    0.8262007    0.8012542    0.7776589    0.7551587
# cluster_2003    0.9121303    0.9621479    1.0000000    0.9669276    0.9373864    0.9103992    0.8847342    0.8585895    0.8326974    0.8078526    0.7838319
# cluster_2004    0.8820075    0.9303578    0.9669276    1.0000000    0.9692680    0.9416363    0.9151505    0.8885190    0.8617715    0.8362820    0.8111142
# cluster_2005    0.8554353    0.9020509    0.9373864    0.9692680    1.0000000    0.9717119    0.9443356    0.9172486    0.8899302    0.8635932    0.8373338
# cluster_2006    0.8311019    0.8760958    0.9103992    0.9416363    0.9717119    1.0000000    0.9718942    0.9444547    0.9166319    0.8894883    0.8619896
# cluster_2007    0.8083397    0.8513675    0.8847342    0.9151505    0.9443356    0.9718942    1.0000000    0.9718509    0.9430427    0.9151873    0.8866510
# cluster_2008    0.7845501    0.8262007    0.8585895    0.8885190    0.9172486    0.9444547    0.9718509    1.0000000    0.9701010    0.9411372    0.9110896
# cluster_2009    0.7610456    0.8012542    0.8326974    0.8617715    0.8899302    0.9166319    0.9430427    0.9701010    1.0000000    0.9703811    0.9388358
# cluster_2010    0.7388046    0.7776589    0.8078526    0.8362820    0.8635932    0.8894883    0.9151873    0.9411372    0.9703811    1.0000000    0.9670820
# cluster_2011    0.7178283    0.7551587    0.7838319    0.8111142    0.8373338    0.8619896    0.8866510    0.9110896    0.9388358    0.9670820    1.0000000

#########################################################################################################
# Crosstab 2002-2011 clusters with 2001 clusters                                                        #
#########################################################################################################
for (year in 2002:2011){
  crosstab <- table(clustersRecl$cluster_2001, clustersRecl[[paste("cluster", year, sep = "_")]])
  write.csv(crosstab, paste("crosstab_2001_", year,".csv",sep=""))
}

#########################################################################################################
# Put data in a shapefile                                                                               #
#########################################################################################################
OA_2011 <- readOGR("/media/mick/My Passport/backup/GIS_Data/OAs_2011","OA_2011_England")

OA_2011@data <- data.frame(OA_2011@data, clustersRecl[match(OA_2011@data[,"OA11CD"], clustersRecl[,"OA"]), ])
OA_2011@data$OA <- NULL

writeOGR(OA_2011, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/GIS", "all_clusters", driver="ESRI Shapefile")

#########################################################################################################
# Indicators of uncertainty                                                                             #
# first we need to calculate the euclidean distances and the cluster centroids                          #
# which will form the basis of the uncertainty analysis                                                 #
# three indicators:                                                                                     #
# a) minimum euclidean distance,                                                                        #
# b) difference between the minimum distance and the distance to the next closest centroid              #
# c) difference of minimum distance between two consecutive years                                       #
#########################################################################################################

calc_dist_diff <- function(x){
  sort_dist <- sort(x)
  return(sort_dist[2] - sort_dist[1])
}

all_min_dist <- data.frame(OA = clustersRecl[,"OA"])
for (year in 2001:2011){
  
  cluster <- paste("cluster", year, sep = "_")
  Df <- read.csv(paste("data_", year, ".csv", sep = ""))
  Df[,2:56] <- log(Df[,2:56]+sqrt(Df[,2:56]^2+1))
  Df[,2:56] <- apply(Df[,2:56], 2, std_range)
  Df <- merge(Df, clustersRecl[,c("OA", cluster)], by = "OA")
  centroids <- get_centroids(Df[,2:56], Df[[cluster]])
  euclidean_distances <- data.frame(OA = Df[,"OA"])
  
  for (cluster in 1:8){
    centroids_cluster <- centroids[cluster, 2:ncol(centroids)]
    euclidean_distances[[paste("dist_cl",cluster,sep="_")]] <- as.numeric(apply(Df[,2:56], 1, function(x) dist(rbind(x, centroids_cluster))))
  }
  
  euclidean_distances[[paste("min_dist", year, sep="_")]] <- as.numeric(apply(euclidean_distances[,2:9], 1, min))
  euclidean_distances[[paste("diff_dist", year, sep="_")]] <- as.numeric(apply(euclidean_distances[,2:9], 1, calc_dist_diff))
  all_min_dist <- merge(all_min_dist, euclidean_distances[,c("OA", paste("min_dist", year, sep="_"))], by = "OA")
  
  write.csv(centroids, paste("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/centroids/centroids_", year, ".csv", sep = "_"), row.names = F)
  write.csv(euclidean_distances, paste("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/euclidean_dist_", year, ".csv", sep = ""), row.names = F)
}

diff_between_years <- data.frame(OA = all_min_dist[,"OA"])
year <- 2001
for (i in 2:11){
  col_name <- paste("diff_dist",year,year+1,sep="_")
  diff_between_years[[col_name]] <- abs(all_min_dist[,i] - all_min_dist[,i+1])
  year <- year + 1
}

write.csv(diff_between_years, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/diff_between_years.csv", row.names = F)
write.csv(all_min_dist, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/all_min_dist.csv", row.names = F)

dist_files <- list.files("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist", "euclid*")

setwd("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist")
year <- 2001
for (i in dist_files){
  Df <- read.csv(i)
  col_name <- paste("diff_dist", year, sep = "_")
  if (year == 2001){
    diff_between_clusters <- data.frame(OA = Df[,"OA"], diff_dist_2001 = Df[[col_name]])
  } else {
    diff_between_clusters <- merge(diff_between_clusters, Df[,c("OA", col_name)], by = "OA")
  }
  year <- year + 1
}
write.csv(diff_between_clusters, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/diff_between_clusters.csv", row.names = F)

summary(diff_between_clusters[,2:12])
# diff_dist_2001      diff_dist_2002      diff_dist_2003      diff_dist_2004      diff_dist_2005      diff_dist_2006      diff_dist_2007      diff_dist_2008      diff_dist_2009      diff_dist_2010      diff_dist_2011     
# Min.   :0.0000026   Min.   :0.0000266   Min.   :0.0000333   Min.   :0.0000381   Min.   :0.0000298   Min.   :0.0000329   Min.   :0.0000289   Min.   :0.0000457   Min.   :0.0000342   Min.   :0.0000424   Min.   :0.0000301  
# 1st Qu.:0.0660227   1st Qu.:0.0711521   1st Qu.:0.0717556   1st Qu.:0.0722661   1st Qu.:0.0724534   1st Qu.:0.0724030   1st Qu.:0.0720315   1st Qu.:0.0712351   1st Qu.:0.0705778   1st Qu.:0.0691749   1st Qu.:0.0671833  
# Median :0.1501247   Median :0.1578305   Median :0.1589945   Median :0.1604244   Median :0.1610892   Median :0.1609067   Median :0.1601395   Median :0.1588370   Median :0.1573287   Median :0.1543527   Median :0.1496222  
# Mean   :0.1783340   Mean   :0.1823032   Mean   :0.1834872   Mean   :0.1844090   Mean   :0.1845599   Mean   :0.1842412   Mean   :0.1832736   Mean   :0.1816321   Mean   :0.1799834   Mean   :0.1769475   Mean   :0.1726854  
# 3rd Qu.:0.2640042   3rd Qu.:0.2726040   3rd Qu.:0.2745440   3rd Qu.:0.2758769   3rd Qu.:0.2764352   3rd Qu.:0.2760634   3rd Qu.:0.2746636   3rd Qu.:0.2719541   3rd Qu.:0.2692742   3rd Qu.:0.2639839   3rd Qu.:0.2569434  
# Max.   :0.7084321   Max.   :0.7160818   Max.   :0.7332807   Max.   :0.7368526   Max.   :0.7087937   Max.   :0.6982605   Max.   :0.7113674   Max.   :0.7219392   Max.   :0.7280751   Max.   :0.7275554   Max.   :0.7249338

summary(diff_between_years[,2:11])
# diff_dist_2001_2002 diff_dist_2002_2003 diff_dist_2003_2004 diff_dist_2004_2005 diff_dist_2005_2006 diff_dist_2006_2007 diff_dist_2007_2008 diff_dist_2008_2009 diff_dist_2009_2010 diff_dist_2010_2011
# Min.   :0.000002    Min.   :0.0000008   Min.   :0.0000005   Min.   :0.00000     Min.   :8.000e-08   Min.   :4.000e-08   Min.   :1.000e-08   Min.   :1.200e-07   Min.   :0.0000003   Min.   :0.0000002  
# 1st Qu.:0.138670    1st Qu.:0.0164751   1st Qu.:0.0112438   1st Qu.:0.00762     1st Qu.:5.356e-03   1st Qu.:4.606e-03   1st Qu.:5.198e-03   1st Qu.:5.361e-03   1st Qu.:0.0070622   1st Qu.:0.0101187  
# Median :0.216636    Median :0.0290955   Median :0.0215026   Median :0.01559     Median :1.143e-02   Median :9.872e-03   Median :1.108e-02   Median :1.152e-02   Median :0.0148197   Median :0.0203401  
# Mean   :0.225770    Mean   :0.0343417   Mean   :0.0251791   Mean   :0.01879     Mean   :1.434e-02   Mean   :1.246e-02   Mean   :1.373e-02   Mean   :1.428e-02   Mean   :0.0181717   Mean   :0.0257099  
# 3rd Qu.:0.293708    3rd Qu.:0.0445910   3rd Qu.:0.0338248   3rd Qu.:0.02584     3rd Qu.:1.983e-02   3rd Qu.:1.729e-02   3rd Qu.:1.931e-02   3rd Qu.:2.008e-02   3rd Qu.:0.0253284   3rd Qu.:0.0340670  
# Max.   :2.472725    Max.   :0.6608292   Max.   :0.4250574   Max.   :0.30592     Max.   :2.268e-01   Max.   :1.667e-01   Max.   :1.874e-01   Max.   :2.440e-01   Max.   :0.3648114   Max.   :0.6257388  

summary(all_min_dist[,2:12])
# min_dist_2001    min_dist_2002    min_dist_2003    min_dist_2004    min_dist_2005    min_dist_2006    min_dist_2007    min_dist_2008    min_dist_2009    min_dist_2010    min_dist_2011   
# Min.   :0.4682   Min.   :0.3346   Min.   :0.3325   Min.   :0.3340   Min.   :0.3411   Min.   :0.3293   Min.   :0.3214   Min.   :0.3250   Min.   :0.3237   Min.   :0.3120   Min.   :0.3141  
# 1st Qu.:0.8862   1st Qu.:0.7092   1st Qu.:0.6821   1st Qu.:0.6628   1st Qu.:0.6498   1st Qu.:0.6431   1st Qu.:0.6432   1st Qu.:0.6498   1st Qu.:0.6581   1st Qu.:0.6715   1st Qu.:0.6913  
# Median :1.0277   Median :0.8126   Median :0.7828   Median :0.7623   Median :0.7487   Median :0.7418   Median :0.7421   Median :0.7496   Median :0.7583   Median :0.7722   Median :0.7942  
# Mean   :1.0757   Mean   :0.8527   Mean   :0.8198   Mean   :0.7970   Mean   :0.7819   Mean   :0.7743   Mean   :0.7738   Mean   :0.7807   Mean   :0.7893   Mean   :0.8040   Mean   :0.8273  
# 3rd Qu.:1.2009   3rd Qu.:0.9487   3rd Qu.:0.9138   3rd Qu.:0.8905   3rd Qu.:0.8754   3rd Qu.:0.8680   3rd Qu.:0.8679   3rd Qu.:0.8755   3rd Qu.:0.8843   3rd Qu.:0.9001   3rd Qu.:0.9258  
# Max.   :5.1115   Max.   :3.4406   Max.   :3.1761   Max.   :2.9961   Max.   :2.8584   Max.   :2.7541   Max.   :2.7520   Max.   :2.7714   Max.   :2.7714   Max.   :2.7745   Max.   :2.9481  

###############################################################################################################
# Maps of Liverpool
###############################################################################################################
oa_shp <- readOGR("/media/mick/My Passport/backup/GIS_Data/OAs_2011", "OA_2011_England")
wards_shp <- readOGR("/media/mick/My Passport/backup/GIS_Data/Wards_11", "WD_DEC_2011_EW_BGC")
higher_geog <- read.csv("/media/mick/My Passport/backup/Census_11/lookups/oa_wards_lad/OA11_WD11_LAD11_EW_LU.csv")[,c(1:2,8)]
# oa_msoa_lookup <- read.csv("/home/mick/Census_01/lookup/oa_lsoa_msoa/OA01_LSOA01_MSOA01_EW_LU.csv")

oa_shp@data <- data.frame(oa_shp@data, higher_geog[match(OA_2011@data[,"OA11CD"], higher_geog[,"OA11CD"]), ])
oa_shp@data[,2] <- NULL
oa_shp@data <- data.frame(oa_shp@data, clustersRecl[match(OA_2011@data[,"OA11CD"], clustersRecl[,"OA"]), ])
oa_shp@data[,4] <- NULL 

liverpool <- subset(oa_shp, grepl("^Liverpool.*", LAD11NM))

liverpool_wards <- wards_shp[wards_shp@data$WD11CD %in% liverpool@data$WD11CD, ]

my_colours <- brewer.pal(7, "Accent")

col_names <- names(leeds@data)
setwd("/media/mick/My Passport/backup/membership_analysis/output/cluster_analysis/2001_2011/results/plots")
for (year in 2001:2011){
  
  jpeg(paste("Liverpool", year, ".jpeg", sep = ""), width = 1000, height = 1200,quality=100)
  
  cluster <- as.factor(liverpool@data[[paste("cluster", year, sep = "_")]])
  
  plot(liverpool, col = my_colours[cluster], axes = FALSE, border = NA)
  
  plot(liverpool_wards, border = "#707070", add = TRUE)
  
  # Add on text labels for the wards
  pointLabel(coordinates(liverpool_wards)[,1], coordinates(liverpool_wards)[,2], labels = liverpool_wards@data$WD11NM, cex = 0.7)
  
  legend("bottomleft", legend = levels(cluster), fill = my_colours, bty = "n", cex = 1.2)
  
  title(paste("Liverpool Supergroup Census OA Classification,", year, sep = " "))
  
  dev.off()
  
}

##################################################################################################
# Sankey graph
##################################################################################################
require(rCharts)
require(igraph)

Df <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/clusters.csv")

createSankeyData <- function(df_input, cluster_type = "Supergroup"){
  out <- NULL
  year <- 2001
  for (i in 2:(ncol(df_input) - 1)){
    cross_tab <- table(df_input[,i], df_input[,i + 1])
    for (row in 1:nrow(cross_tab)){
      for (col in 1:ncol(cross_tab)){
        source_cluster <- paste(cluster_type, row.names(cross_tab)[row], year, sep = "_")
        target_cluster <- paste(cluster_type, colnames(cross_tab)[col], year + 1, sep = "_")
        value <- cross_tab[row, col]
        create_row <- c(source_cluster, target_cluster, value)
        out <- rbind(out, create_row)
      }
    }
    year <- year + 1
  }
  out <- as.data.frame(out)
  row.names(out) <- as.character(seq(1:nrow(out)))
  colnames(out) <- c("source", "target", "value")
  out[,1] <- as.character(out[,1])
  out[,2] <- as.character(out[,2])
  out[,3] <- as.character(out[,3])
  out[,3] <- as.numeric(out[,3])
  
  return(out)
}

sankeyData <- createSankeyData(Df)

sankeyData2 <- sankeyData[1:448,]
sankeyData3 <- sankeyData[449:640,]

sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')

sankeyPlot$set(
  data = sankeyData2,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 1750,
  height = 500,
  labelFormat = ".1%"
)

sankeyPlot

sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey')

sankeyPlot$set(
  data = sankeyData3,
  nodeWidth = 10,
  nodePadding = 5,
  layout = 32,
  width = 250,
  height = 500,
  labelFormat = ".1%"
)

sankeyPlot

###################################################################################
# Efficiency of Uncertainty Predictors
###################################################################################

clusters <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/clusters.csv")

diff_years <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/diff_between_years.csv")
min_dist <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/all_min_dist.csv")
diff_clusters <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/results/dist/diff_between_clusters.csv")

# Function to identify the OAs which drifted between two consecutive years
identify_changed <- function(this_year, next_year){
  if (this_year != next_year){
    return("Changed")
  } else {
    return("Unchanged")
  }
}

# Run the function
changed <- data.frame(OA = clusters[,"OA"])
for (year in 2001:2010){
  new_col <- paste("changed", year, year + 1, sep = "_")
  this_year_col <- paste("cluster", year, sep = "_")
  next_year_col <- paste("cluster", year + 1, sep = "_")
  changed[[new_col]] <- as.factor(mapply(identify_changed, clusters[[this_year_col]], clusters[[next_year_col]]))
}

# Use boxplots to visualise the difference in minimum euclidean distance
# between OAs that drifted and OAs that did not
min_dist <- merge(min_dist, changed, by = "OA")
par(mfrow = c(2,5))
year <- 2001
for (i in 2:11){
  boxplot(min_dist[,i] ~ min_dist[,i+11], main = paste("Year:", year), xlab = "Changed", ylab = "Minimum Euclidean Distance")
  year <- year + 1
}

# Do the same for the difference in minimum Euclidean distance between years
diff_years <- merge(diff_years, changed, by = "OA")
par(mfrow = c(2,5))
year <- 2001
for (i in 2:11){
  boxplot(diff_years[,i] ~ diff_years[,i+10], main = paste("Period:", year,"-", year+1), xlab = "Changed", ylab = "Absolute Minimum Euclidean Distance Difference")
  year <- year + 1
}

# Do the same for the difference in minimum Euclidean distance between clusters (the one assigned and the next closest)
diff_clusters <- merge(diff_clusters, changed, by = "OA")
par(mfrow = c(2,5))
year <- 2001
for (i in 2:11){
  boxplot(diff_clusters[,i] ~ diff_clusters[,i+11], main = paste("Year:", year), xlab = "Changed", ylab = "Distance Difference Between Assigned and Next Closest Cluster")
  year <- year + 1
}

#####################################################################################################
# Perhaps 2001 is a specific case, check if 8 clusters is appropriate using scree plot
#####################################################################################################
setwd("/media/mick/My Passport/backup/cluster_analysis/2001_2011/input_data")
csv_files <- list.files(".", "data*")
par(mfrow = c(3,4))
year <- 2001
for (i in csv_files){
  Df <- read.csv(i)[,2:56]
  wss <- (nrow(Df)-1)*sum(apply(Df,2,var))
  for (j in 2:12) wss[j] <- sum(kmeans(Df, centers=j,nstart=1)$withinss)
  plot(1:12, wss, type="b", main = paste("Year:", year), xlab="Number of Clusters", ylab="Within groups sum of squares")
  year <- year + 1
}

#####################################################################################################
# Compare Minimum Euclidean distances conditional on clusters
#####################################################################################################
min_dist_clusters <- merge(min_dist[,1:12], clusters, by = "OA")
for (i in 13:23) min_dist_clusters[,i] <- as.factor(min_dist_clusters[,i])

par(mfrow = c(2,6))
year <- 2001
for (i in 2:12){
  dist_col <- "Minimum_Euclidean_Distance"
  assign(dist_col, min_dist_clusters[,i])
  cluster_col <- "Clusters"
  assign(cluster_col, min_dist_clusters[,i+11])
  plot.design(Clusters ~ Minimum_Euclidean_Distance, main = paste("Year:", year))
  year <- year + 1
}

