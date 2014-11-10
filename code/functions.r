########################################################################################
fun_std <- function(x){
  out <- log(x + 1)
  out <- (out - min(out)) / (max(out) - min(out))
  return(out)
}

fun_std2 <- function(x){
  out <- (x - min(x)) / (max(x) - min(x))
  return(out)
}

get_centroids <- function(Df, cluster){
  centroids <- as.data.frame(aggregate(Df, list(cluster = cluster), mean))
  return(centroids)
}


# Function to identify the OAs which drifted between two consecutive years
identify_changed <- function(this_year, next_year){
  if (this_year != next_year){
    return("Changed")
  } else {
    return("Unchanged")
  }
}

max_percent_change <- function(x){
  max_val <- max(x)
  return(sum(sapply(x, function(x) abs(x - max_val)))/length(x))
}

# Calculate the difference between the euclidean distance to assigned and next nearest centroid
calc_dist_diff <- function(x){
  sort_dist <- sort(x)
  return(sort_dist[2] - sort_dist[1])
}

# Calculate ratio of Euclidean distances to closest and furthest centroid
# It is an indicator of dimensionality problem and Euclidean distance suitability
# The greater from 1 it is the ratio the better
closest_furthest_ratio <- function(x){
  sort_dist <- sort(x)
  return(sort_dist[length(sort_dist)] / sort_dist[1])
}

# Three measures of standard distance dispersion
# 1. Quadratic average of distance from the center of gravity
# d = sqrt((sum((dic^2)/n)))
# 2. The square root of the sum of variances of x and y
# d = sqrt(var(x) + var(y))
# 3. The mean quadratic distance with repetition between any pair of cases
# d = sqrt(sum(sum((dij^2)/n)))

# 1. Quadratic Average
std_dist1 <- function(min_dist){
  return(sqrt(sum((min_dist^2)/length(min_dist))))
}

# 2. Square Root of the Sum of Variances
std_dist2 <- function(variables_values){
  return(sqrt(sum(var(variables_values))))
}





# Calculate % change from reference year
# The first column is the reference year
# ref_year <- a column in a dataframe
# other_years <- one or more columns
# Use it as sapply(Df[,2:ncol(Df)], perc_change_from_ref_year, Df[,1])
perc_change_from_ref_year <- function(other_years, ref_year){
  return(ifelse(ref_year == 0, 0, abs(100*(other_years - ref_year)/ref_year)))
}

# Classify a numeric array using the quantiles (provided by the user)
recl_with_quant <- function(x, qnt_breaks){
  my_qnt <- quantile(x, qnt_breaks)
  out <- cut(x, my_qnt, include.lowest = T)
  levels(out) <- c("Low", "Moderate", "High")
  return(out)
}

# euclidian_minimum_dist <- function(x){
#   euclidian_dist <- sqrt(rowSums(sweep(centroids[,2:42], 2, x) ^ 2))
#   min_euclidian_dist <- min(euclidian_dist)
#   index <- which(euclidian_dist == min_euclidian_dist)
#   cluster_name <- as.character(centroids[index, 1])
#   return(cluster_name)
# }
# 
# euclidian_all_dist <- function(x,y,centroids){
#   euclidian_dist <- sqrt(rowSums(sweep(centroids[x, 2:42], 2, y) ^ 2))
# }

all_euclid_dist <- function(x, cluster_centroid){
  return(dist(rbind(x, cluster_centroid)))
}

#########################################################################################

# library(foreign)
# library(plyr)
# library(rgdal)
# library(rgeos)
# library(sqldf)
# 
# 
# OA_2001 <- readOGR("/media/mick/My Passport/backup/GIS_Data/Boundaries_01", "England_oa_2001_clipped_area")
# OA_2011 <- readOGR("/media/mick/My Passport/backup/GIS_Data/OAs_2011", "OA_2011_England")
# lookup = read.csv("/media/mick/My Passport/data/dwelling/OA01_OA11_LAD11_EW_LU.csv")
# nspl = read.dbf("/media/mick/My Passport/data/dwelling/NSPL_FEB_2013_UK.dbf")

convert_oa01_to_oa11 <- function(input_data, output_data, OA_2001, OA_2011, orig_lookup, NSPL){
  
  # Input data should be a data frame with a column named oa_01
  # Output data the path and name of the output csv file
  
  # Select lookup only for england
  orig_lookup <- orig_lookup[orig_lookup$OA11CD %in% OA_2011@data$OA11CD,]
  
  # Unchanged
  unchanged <- subset(orig_lookup, CHGIND == "U")
  # Merged
  merged <- subset(orig_lookup, CHGIND == "M")
  # Split
  split <- subset(orig_lookup, CHGIND == "S")
  # Other
  other <- subset(orig_lookup, CHGIND == "X")
  
  # Remove non current (DOTERM !is.na), non E & W (OA11), large user (USERTYPE = 1) from NSPL
  NSPL_Keep <- subset(NSPL, USERTYPE == 0 & is.na(DOTERM) & OA11 != "" & !is.na(OA11), select = c("PCD", "DOTERM", "USERTYPE", "OA11"))
  # Create postcode frequency for each 2011 OA
  Postcodes_2011_OA <- as.data.frame(table(as.character(NSPL_Keep$OA11)))
  # The 2011 OAs from the postcodes file is 26 OAs short from the OA_2011 shapefile
  
  # Create proportions for split postcodes
  split <- data.frame(split, Postcodes_2011_OA[match(split[, "OA11CD"], Postcodes_2011_OA[,"Var1"]), ])
  postcode_sums <- ddply(split, .(OA01CDO), summarise, sum = sum(Freq))
  
  split <- data.frame(split, postcode_sums[match(split[, "OA01CDO"], postcode_sums[,"OA01CDO"]), ])
  split$proportion <- split$Freq/split$sum
  split$Var1 <- NULL
  split$OA01CDO.1 <- NULL
  
  # Subset split
  split_NA <- split[is.na(split$proportion), ]
  split_no_NA <- split[!is.na(split$proportion), ]
  out <- NULL
  
  # Run correction
  for (x in 1:nrow(split_NA)) {
    
    old <- as.character(split_NA[x, "OA01CDO"])
    new <- as.character(split_NA[x, "OA11CD"])
    
    tmp_01 <- OA_2001[OA_2001@data$LABEL == old, ]
    tmp_11 <- OA_2011[OA_2011@data$OA11CD == new, ]
    tmp_intersect <- gIntersection(tmp_01, tmp_11)
    tmp_overlap <- gArea(tmp_intersect)
    
    tmp_area_01 <- gArea(tmp_01)
    tmp_01_overlap_pct <- tmp_overlap/tmp_area_01
    out <- c(out, tmp_01_overlap_pct)
    
    remove(tmp_01, tmp_11, tmp_overlap, tmp_area_01, tmp_01_overlap_pct)
  }
  
  # Create a unified split object
  
  split_NA <- subset(split_NA, select = c("OA01CDO", "OA11CD"))
  split_NA <- cbind(split_NA, out)
  colnames(split_NA) <- c("OA01CDO", "OA11CD", "proportion")
  split_no_NA <- subset(split_no_NA, select = c("OA01CDO", "OA11CD", "proportion"))
  split <- rbind(split_no_NA, split_NA)
  
  # List of old OA freq
  old_freq <- as.data.frame(table(as.character(other$OA01CDO)))
  colnames(old_freq) <- c("OA01CDO", "old_N")
  # List of new OA freq
  new_freq <- as.data.frame(table(as.character(other$OA11CD)))
  colnames(new_freq) <- c("OA11CD", "new_N")
  # Merge onto other
  other <- data.frame(other, old_freq[match(other[, "OA01CDO"], old_freq[, "OA01CDO"]),])
  other$OA01CDO.1 <- NULL
  other <- data.frame(other, new_freq[match(other[, "OA11CD"], new_freq[, "OA11CD"]),])
  other$OA11CD.1 <- NULL  
  
  out <- NULL
  for (x in 1:nrow(other)) {
    
    old <- as.character(other[x, "OA01CDO"])
    new <- as.character(other[x, "OA11CD"])
    
    
    tmp_01 <- OA_2001[OA_2001@data$LABEL == old, ]
    tmp_11 <- OA_2011[OA_2011@data$OA11CD == new, ]
    tmp_intersect <- gIntersection(tmp_01, tmp_11)
    
    if (is.null(tmp_intersect)) {
      #checks if tmp_intersect is null - if so, it assigns it the area of the 01 OA
      tmp_intersect <- gArea(tmp_01)
      tmp_overlap <- 0
      plot(tmp_01, col = "#6E7B8B", border = "#CAE1FF")
      plot(tmp_11, border = "red", add = TRUE)
      title(c(old, new))}
    else {
      tmp_overlap <- gArea(tmp_intersect)
    }
    
    # Get sum of overlap polygons
    tmp_area_01 <- gArea(tmp_01)
    tmp_01_overlap_pct <- tmp_overlap/tmp_area_01
    out <- c(out, tmp_01_overlap_pct)
    
    remove(tmp_01, tmp_11, tmp_overlap, tmp_area_01, tmp_01_overlap_pct)
  }
  
  other <- cbind(other, out)
  
  ####################################
  # Creating Smaller Objects
  ####################################
  
  unchanged <- subset(unchanged, select = c("OA01CDO", "OA11CD"))
  merged <- subset(merged, select = c("OA01CDO", "OA11CD"))
  # split <- subset(split, select=c('OA01CDO','OA11CD','proportion'))
  # ####Split was trimmed ealier!
  other <- subset(other, select = c("OA01CDO", "OA11CD", "out"))
  colnames(other) <- c("OA01CDO", "OA11CD", "proportion")
  ######################################
  
  #########################################################################
  # Appending 2001 data and creating aggregation into the 2011 boundaries
  #########################################################################
  
  unchanged <- merge(unchanged, input_data, by.x = "OA01CDO", by.y = "oa_01", in.x = TRUE)
  merged <- merge(merged, input_data, by.x = "OA01CDO", by.y = "oa_01", in.x = TRUE)
  split <- merge(split, input_data, by.x = "OA01CDO", by.y = "oa_01", in.x = TRUE)
  other <- merge(other, input_data, by.x = "OA01CDO", by.y = "oa_01", in.x = TRUE)
  
  
  # Unchanged output areas are direct matches, so no further manipulation is necessary, beyond removing the 2001 code.
  unchanged$OA01CDO <- NULL
  
  # However, for merged, the aggregate of the 2001 data is required. This is achieved using the aggregate function,
  # with a list variable generated from the column names.
  ###############################
  # Merged
  ###############################
  n <- colnames(other)
  n <- n[4:length(n)]
  merged_out <- as.data.frame(unique(merged$OA11CD))
  colnames(merged_out) <- "OA11CD"
  # Loop to merge all variables
  for (x in 1:length(n)) {
    Cen_Var <- n[x]
    temp <- sqldf(paste("select OA11CD,sum(", Cen_Var, ") as ", Cen_Var, " from merged group by OA11CD", 
                        sep = ""))
    merged_out <- merge(merged_out, temp, by = "OA11CD", in.x = TRUE)
    remove(temp)
  }
  
  # The split and the other lookup objects are both handled by multiplying the 2001 attributes by the proportion scores;
  # and then in the case of other, aggregated (summed) by the new 2011 Output Areas.
  ###########################
  # Split
  ###########################
  n <- colnames(split)
  n <- n[4:length(n)]
  
  split_out <- as.data.frame(split$OA11CD)
  colnames(split_out) <- "OA11CD"
  
  for (x in 1:length(n)) {
    Cen_Var <- n[x]
    temp <- sqldf(paste("select OA11CD,", Cen_Var, " * proportion as ", Cen_Var, 
                        " from split", sep = ""))
    split_out <- merge(split_out, temp, by = "OA11CD", in.x = TRUE)
  }
  
  #####################################
  # Other - create proportional splits
  #####################################
  n <- colnames(other)
  n <- n[4:length(n)]
  
  #other <- cbind(other, 1:nrow(other))
  
  other_out <- as.data.frame(other$OA11CD)
  colnames(other_out) <- c("OA11CD")
  
  for (x in 1:length(n)) {
    Cen_Var <- n[x]
    temp <- as.data.frame(round(get("other")[, Cen_Var] * get("other")[, "proportion"], 3))
    colnames(temp) <- paste(Cen_Var)
    other_out <- cbind(other_out, temp)
    remove(temp)
  }
  
  
  # Other - create summed values for 2011 output areas
  n <- colnames(other_out)
  n <- n[2:length(n)]
  
  other_out_merged <- as.data.frame(unique(other_out$OA11CD))
  colnames(other_out_merged) <- "OA11CD"
  
  # Loop to merge all variables
  for (x in 1:length(n)) {
    Cen_Var <- n[x]
    temp <- sqldf(paste("select OA11CD,sum(", Cen_Var, ") as ", Cen_Var, " from other_out group by OA11CD", 
                        sep = ""))
    other_out_merged <- data.frame(other_out_merged, temp[match(other_out_merged[, 
                                                                                 "OA11CD"], temp[, "OA11CD"]), ])
    other_out_merged$OA11CD.1 <- NULL
    remove(temp)
  }
  
  print("printing total number of rows")
  print(nrow(unchanged) + nrow(merged_out) + nrow(split_out) + nrow(other_out_merged))
  
  ##########################################################################
  #  Create a single unified 2001 Census Table for the 2011 boundaries
  ##########################################################################
  
  
  out <- rbind(unchanged, merged_out, split_out, other_out_merged)
  out <- out[order(out$OA11CD), ]
  
  write.csv(out, output_data, row.names = F)
  
}


########################################################################################################
# A function to Match and Reclassify Clusters                                                          #
########################################################################################################

# Input is a data frame with two or more columns of cluster numbers produced from
# two or more separate cluster analysis which have to be matched

recl_clusters <- function(Df){
  
  # max_in_table function is used after crosstabulation in the function match_clusters
  # to get the index of the entry with the maximum number of clusters in a row or column
  max_in_table <- function(x){
    out <- which(x == max(x))
    # check if we have a tie, i.e. two or more entries in a row or column with the same number of clusters
    if (length(out) == 1){
      return(out)
    } else {
      return(-99) # If there is a tie return a number that we can identify
    }
  }
  
  # If there is a tie check if it only occurred for one column or row
  # If so replace -99 with the missing row or column
  deal_with_ties <- function(x){
    if (-99 %in% x){
      ties <- which(x == -99)
      if (length(ties) == 1){
        x[ties] <- which(! 1:length(x) %in% x)
        return(x)
      } else {
        stop(paste("Problem, multiple ties for pair of columns", pair_of_cols[1], "and", pair_of_cols[2]))
      }
    } else {
      return(x)
    }
  }
  
  match_clusters <- function(pair_of_cols){
    
    crosstab <- table(Df[,pair_of_cols])
    
    max_col <- as.integer(apply(crosstab, 2, max_in_table))
    max_col <- deal_with_ties(max_col)
    max_row <- as.integer(apply(crosstab, 1, max_in_table))
    max_row <- deal_with_ties(max_row)
    
    # Create a named list
    list_clusters <- NA
    for (i in 1:(length(max_row) - 1)){
      list_clusters <- c(list_clusters, NA)
    }
    # The names are the first column clusters
    names(list_clusters) <- row.names(crosstab)
    
    row_index = 1
    for (i in max_row){
      if (row_index == max_col[i]) {
        list_clusters[row.names(crosstab)[row_index]] <- colnames(crosstab)[i]
      }
      row_index <- row_index + 1
    }
    
    # If only one was not assigned, match unassigned pair
    if (sum(is.na(list_clusters)) == 1){
      not_matched <- colnames(crosstab)[which(!colnames(crosstab) %in% list_clusters)]
      print(paste("Warning! Cluster", not_matched, "was not matched after crosstabulation between columns", pair_of_cols[1], "and", pair_of_cols[2]))
      list_clusters[which(is.na(list_clusters))] <- not_matched
    } else if (sum(is.na(list_clusters)) >= 2){
      print("Problem! Two or more clusters were not matched")
    }
    
    return(list_clusters)
  }
  
  
  for (col in 1:(ncol(Df) - 1)){
    
    print(paste("Crosstabulating", col, "and", col+1))
    
    current_col <- paste("col", col, sep = "_")
    next_col <- paste("col", col+1, sep = "_")
    matchedClusters <- match_clusters(c(col, col+1))
    if (col == 1){
      matchedClustersDF <- data.frame(col_1 = names(matchedClusters), col_2 = matchedClusters)
    } else {
      temp <- data.frame(names(matchedClusters), matchedClusters)
      names(temp) <- c(current_col, next_col)
      matchedClustersDF <- merge(matchedClustersDF, temp, by = current_col)
    }
  }
  
  matchedClustersDF <- matchedClustersDF[,paste("col", 1:ncol(Df), sep="_")]
  
  # Now reclassify the clusters data frame
  fetch_first_value <- function(col_data, matchedClustersDF, clusters_col_nr){
    return(as.character(matchedClustersDF[which(matchedClustersDF[,clusters_col_nr] == col_data), ][,1]))
  }
  
  for (i in 2:ncol(matchedClustersDF)){
    print(paste("Reclassifying", i))
    Df[,i] <- as.integer(sapply(Df[,i], fetch_first_value, matchedClustersDF, i))
  }
  
  return(Df)
}

# Df2 <- recl_clusters(Df)
