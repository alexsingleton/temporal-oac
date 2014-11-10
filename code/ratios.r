#########################################################################################
# Produce ratios
# Divide the sum of each variable for both years conditional on each cluster
# by the sum of the appropriate denominator conditional on each cluster
# and then divide this by the ratio of the total sum of each variable
# and the total sum of the appropriate denominator
# e.g. ratio1 = ((People above 65, Year = 2001, Cluster = 1) + (People above 65, Year = 2011, Cluster = 1))/((Total Pop, Year = 2001, Cluster = 1) + (Total Pop, Year = 2011, Cluster = 1))
# ratio 2 = ((People above 65, Year = 2001, Cluster = All) + (People above 65, Year = 2011, Cluster = All)/((Total Pop, Year = 2001, Cluster = All) + (Total Pop, Year = 2011, Cluster = All)))
# index = ratio1 / ratio2
#########################################################################################

# Import data
data_01 <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/Census_2001_2011OA.csv")
data_11 <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/Census_2011.csv")

area <- read.csv("/media/mick/My Passport/backup/Census_11/pop/density_2011.csv")[,c(1,3)]
data_01 <- merge(data_01, area, by="OA11CD")
data_01$density <- (data_01$total_pop / data_01$area)

# import clusters
clusters <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/all_data_clusters.csv")[,1:3]

# lookup
lookup <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/lookup_vars_01_11.csv")
lookup2 <- na.omit(lookup)

data_01 <- merge(data_01, clusters[clusters$year == 2001,], by.x = "OA11CD", by.y = "OA")
data_11 <- merge(data_11, clusters[clusters$year == 2011,], by = "OA")

nominators_exclude_01 <- c("density", "llti_count_01")
nominators_01 <- as.character(lookup2[, 1])
denominators_01 <- unique(as.character(lookup2[, 2]))

nominators_exclude_11 <- c("k035", "k007")
nominators_11 <- as.character(lookup2[, 3])
denominators_11 <- unique(as.character(lookup2[, 4]))

sum_cond_nominators_01 <- aggregate(data_01[,nominators_01], list(data_01$cluster), sum) 
sum_cond_nominators_11 <- aggregate(data_11[,nominators_11], list(data_11$cluster), sum)
sum_cond_denominators_01 <- aggregate(data_01[,denominators_01], list(data_01$cluster), sum) 
sum_cond_denominators_11 <- aggregate(data_11[,denominators_11], list(data_11$cluster), sum) 

# Function to calculate ratio of sum of nominators over sum of denomintors
fun_ratio <- function(nominators_01, nominators_11, denominators_01, denominators_11){
  return(sum(nominators_01, nominators_11) / (sum(denominators_01, denominators_11)))
}

out <- data.frame(cluster = 1:8)

for (i in 1:nrow(lookup2)){
  
  row <- lookup2[i,] # Use the lookup table to match nominators with denominators
  col_name <- as.character(lookup2[i,3]) # Output variable name
  col_data <- NULL # Variable data
  
  # Ratio of the sum of nominators (2001&2011) over the sum of denominators (2001&2011) for all clusters
  total_ratio <- fun_ratio(sum_cond_nominators_01[,as.character(row[,1])], sum_cond_nominators_11[,as.character(row[,3])],
                           sum_cond_denominators_01[,as.character(row[,2])], sum_cond_denominators_11[,as.character(row[,4])])
  
  for (cluster in 1:8){
    
    # Ratio of the sum of nominators (2001&2011) over the sum of denominators (2001&2011) conditional on clusters
    cluster_level_ratio <- fun_ratio(sum_cond_nominators_01[cluster, as.character(row[,1])], sum_cond_nominators_11[cluster, as.character(row[,3])],
                                      sum_cond_denominators_01[cluster, as.character(row[,2])], sum_cond_denominators_11[cluster, as.character(row[,4])])
      
    col_data <- c(col_data, cluster_level_ratio)
  }
  
  col_data <- col_data / total_ratio
  
  out[[col_name]] <- col_data

}
write.csv(out, "/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/index_scores.csv", row.names = F)


