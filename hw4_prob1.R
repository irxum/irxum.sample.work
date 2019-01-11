#
#
# hw4_prob1.R
#

# Problem 1
# 
# You can find a dataset dealing with European employment in 1979 at 
# http://lib.stat.cmu.edu/DASL/Stories/EuropeanJobs.html. This dataset gives the 
# percentage of people employed in each of a set of areas in 1979 for each of a 
# set of European countries. Notice this dataset contains only 26 data points. 
# That's fine; it's intended to give you some practice in visualization of 
# clustering.
# 
# 1.  Use an agglomerative clusterer to cluster this data. Produce a dendrogram 
# of this data for each of single link, complete link, and group average 
# clustering. You should label the countries on the axis. What structure in the 
# data does each method expose? it's fine to look for code, rather than writing 
# your own. Hint: I made plots I liked a lot using R's hclust clustering 
# function, and then turning the result into a phylogenetic tree and using a fan 
# plot, a trick I found on the web; try plot(as.phylo(hclustresult), 
# type='fan'). You should see dendrograms that "make sense" (at least if you 
# remember some European history), and have interesting differences.
# 
# 2.  Using k-means, cluster this dataset. What is a good choice of k for this 
# data and why?

library(ape)
library(cluster)
library(tidyverse)
library(ggplot2)

wk_dir <- "C:/Users/irxumtenk/Google Drive/Education/MCS-DS_UIUC/CS 498 Applied Machine Learning/homework4/"

fname <- "hw4prob1data.txt"

f_path <- paste(wk_dir, fname, sep = "")


emp_dt <- read.table(f_path, header = TRUE, sep = "\t", row.names = 1)


# ------------ #
# Start Part 1 #
#
# obtained guidance from https://rpubs.com/gaston/dendrograms on plotting
# these dendrograms and using the ape package

# try for single link
emp_clust_sngl <- hclust(dist(emp_dt), method = "single")

plot(as.phylo(emp_clust_sngl), type = 'fan', 
     tip.color = hsv(runif(15, 0.5, 0.95), 0.9, 0.8, 0.7), 
     edge.color = hsv(runif(10, 0.5, 0.95), 0.1, 0.2, 0.3), 
     edge.width = runif(20, 0.5, 3), use.edge.length = TRUE, col = "gray80")

# try for complete link
emp_clust_cmplt <- hclust(dist(emp_dt), method = "complete")

plot(as.phylo(emp_clust_cmplt), type = 'fan', 
     tip.color = hsv(runif(15, 0.5, 0.95), 0.9, 0.8, 0.7), 
     edge.color = hsv(runif(10, 0.5, 0.95), 0.1, 0.2, 0.3), 
     edge.width = runif(20, 0.5, 3), use.edge.length = TRUE, col = "gray80")

# try for group average clustering
emp_clust_ave <- hclust(dist(emp_dt), method = "average")

plot(as.phylo(emp_clust_ave), type = 'fan', 
     tip.color = hsv(runif(15, 0.5, 0.95), 0.9, 0.8, 0.7), 
     edge.color = hsv(runif(10, 0.5, 0.95), 0.1, 0.2, 0.3), 
     edge.width = runif(20, 0.5, 3), use.edge.length = TRUE, col = "gray80")

# End of Part 1 #
# ------------- #


# ------------ #
# Start Part 2 #
#
# obtained guidance from https://uc-r.github.io/kmeans_clustering

set.seed(5)

# function to compute total within-cluster sum of square 
wcss <- function(k) {
  kmeans(emp_dt, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10

# extract wss for 2-15 clusters
wcss_vals <- map_dbl(k.values, wcss)

kw_df <- data.frame(k.values, wcss_vals)

ggplot(data=kw_df, aes(x=k.values, y=wcss_vals)) +
  geom_point(colour="steelblue", shape = 5, size = 2, stroke = 2)+
  geom_line(colour="steelblue") +
  ylab("Total within-clusters sum of squares") + 
  xlab("Number of clusters K") + 
  scale_x_continuous(breaks = c(1:10), minor_breaks = NULL) +
  ylim(0,10000) +
  theme_grey()


plot(k.values, wcss_vals,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(emp_dt, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(emp_dt))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
ks.values <- 2:10

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(ks.values, avg_sil)

kws_df <- data.frame(ks.values, avg_sil_values)

ggplot(data=kws_df, aes(x=ks.values, y=avg_sil_values)) +
  geom_point(colour="orange", shape = 5, size = 2, stroke = 2)+
  geom_line(colour="orange") +
  ylab("Average Silhouettes") + 
  xlab("Number of clusters K") + 
  scale_x_continuous(breaks = c(1:10), minor_breaks = NULL) +
  ylim(0,0.6) +
  theme_grey()


plot(ks.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

# End of Part 2 #
# ------------- #



