library(ggplot2)
library(gridExtra)

source(file = "/media/mick/1BAE-C95B/liverpool/r_script/functions.r")

clusters <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/dist/clusters.csv")
min_dist <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/dist/min_dist.csv")
diff_clusters <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/dist/diff_between_clusters.csv")
eucl_dist_01 <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/dist/euclidean_dist_2001.csv")
eucl_dist_11 <- read.csv("/media/mick/My Passport/backup/cluster_analysis/2001_2011/analysis2/results/dist/euclidean_dist_2011.csv")

changed<-data.frame(OA = clusters[,1])
changed$changed_01_11 <- as.factor(mapply(identify_changed, clusters[,2], clusters[,3]))

all_data <- merge(changed, min_dist[,1:3],by = "OA")
all_data <- merge(all_data, diff_clusters[,1:3],by = "OA")

all_data2 <- merge(eucl_dist_01[,c("OA", "cluster_2001", "min_dist_2001")], 
              eucl_dist_11[,c("OA", paste("dist_cl", 1:8, sep = "_"))],
              by = "OA")

out <- NULL
for (i in 1:nrow(all_data2)){
  new_dist_col <- paste("dist_cl", all_data2[i, "cluster_2001"], sep = "_")
  out <- c(out, abs(all_data2[i, "min_dist_2001"] - all_data2[i, new_dist_col]))
}
all_data2$diff_01_11 <- out

all_data <- merge(all_data, all_data2[,c("OA", "diff_01_11")], by = "OA")


# par(mfrow = c(1,5))
# boxplot(all_data$diff_01_11~all_data$changed_01_11, main = "A",
#         xlab = "Classification", ylab = "Euclidean Distance Difference '01 - '11")
# boxplot(all_data$min_dist_2001~all_data$changed_01_11, main = "B",
#         xlab = "Classification", ylab = "Euclidean Distance '01")
# boxplot(all_data$min_dist_2011~all_data$changed_01_11, main = "C",
#         xlab = "Classification", ylab = "Euclidean Distance '11")
# boxplot(all_data$diff_dist_2001~all_data$changed_01_11, main = "D",
#         xlab = "Classification", ylab = "Euclidean Distance Difference Between Assigned and Next Closest Cluster '01")
# boxplot(all_data$diff_dist_2011~all_data$changed_01_11, main = "E",
#         xlab = "Classification", ylab = "Euclidean Distance Difference Between Assigned and Next Closest Cluster '11")

# b1 <- ggplot(all_data, aes(changed_01_11, diff_01_11, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance Difference '01-'11") +
#   ggtitle("A") + geom_boxplot() + scale_fill_discrete(name = "Classification") + theme(legend.position="bottom")
# b2 <- ggplot(all_data, aes(changed_01_11, min_dist_2001, fill = changed_01_11)) + xlab("Cluster Classification") +
#   ylab("Euclidean Distance '01") + ggtitle("B") + geom_boxplot() + scale_fill_discrete(name = "Classification") + theme(legend.position="bottom")
# b3 <- ggplot(all_data, aes(changed_01_11, min_dist_2011, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance '11") +
#   ggtitle("C") + geom_boxplot() + scale_fill_discrete(name = "Classification") + theme(legend.position="bottom")
# b4 <- ggplot(all_data, aes(changed_01_11, diff_dist_2001, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance Difference Between Assigned and Next Closest Cluster '01") +
#   ggtitle("D") + geom_boxplot() + scale_fill_discrete(name = "Classification") + theme(legend.position="bottom")
# b5 <- ggplot(all_data, aes(changed_01_11, diff_dist_2011, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance Difference Between Assigned and Next Closest Cluster '11") +
#   ggtitle("E") + geom_boxplot() + scale_fill_discrete(name = "Classification") + theme(legend.position="bottom")

b1 <- ggplot(all_data[all_data$diff_01_11 <= 1.5,], aes(changed_01_11, diff_01_11, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance Difference '01-'11") +
  ggtitle("A") + geom_boxplot() + scale_fill_discrete(name = "Classification") + guides(fill=F)
b2 <- ggplot(all_data[all_data$min_dist_2001 <= 2.5,], aes(changed_01_11, min_dist_2001, fill = changed_01_11)) + xlab("Cluster Classification") +
  ylab("Euclidean Distance '01") + ggtitle("B") + geom_boxplot() + scale_fill_discrete(name = "Classification") + guides(fill=F)
b3 <- ggplot(all_data[all_data$min_dist_2011 <= 3,], aes(changed_01_11, min_dist_2011, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance '11") +
  ggtitle("C") + geom_boxplot() + scale_fill_discrete(name = "Classification") + guides(fill=F)
b4 <- ggplot(all_data[all_data$diff_dist_2001 <= 0.6,], aes(changed_01_11, diff_dist_2001, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance Difference Between Assigned and Next Closest Cluster '01") +
  ggtitle("D") + geom_boxplot() + scale_fill_discrete(name = "Classification") + guides(fill=F)
b5 <- ggplot(all_data[all_data$diff_dist_2011 <= 0.6,], aes(changed_01_11, diff_dist_2011, fill = changed_01_11)) + xlab("Cluster Classification") + ylab("Euclidean Distance Difference Between Assigned and Next Closest Cluster '11") +
  ggtitle("E") + geom_boxplot() + scale_fill_discrete(name = "Classification") + guides(fill=F)

jpeg("/media/mick/1BAE-C95B/liverpool/paper2/plots/figure6_b.jpg", width = 1200, height = 1000, quality = 100)
grid.arrange(b1,b2,b3,b4,b5,ncol=5)
dev.off()