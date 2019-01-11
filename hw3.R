#
# hw3.R
#
#
#
#
# Problem 1
#
# CIFAR-10 is a dataset of 32x32 images in 10 categories, collected by Alex 
# Krizhevsky, Vinod Nair, and Geoffrey Hinton. It is often used to evaluate 
# machine learning algorithms. You can download this dataset from 
# https:// www.cs.toronto.edu/~kriz/cifar.html.
#
# 1.  For each category, compute the mean image and the first 20 principal 
# components. Plot the error resulting from representing the images of each 
# category using the first 20 principal components against the category.
#
# 2.  Compute the distances between mean images for each pair of classes. Use 
# principal coordinate analysis to make a 2D map of the means of each 
# categories. For this exercise, compute distances by thinking of the images as 
# vectors.
#
# 3.  Here is another measure of the similarity of two classes. For class A and 
# class B, define E(A | B) to be the average error obtained by representing all 
# the images of class A using the mean of class A and the first 20 principal 
# components of class B. Now define the similarity between classes to be 
# (1/2)(E(A | B) + E(B | A)). If A and B are very similar, then this error 
# should be small, because A's principal components should be good at 
# representing B. But if they are very different, then A's principal components 
# should represent B poorly. In turn, the similarity measure should be big. Use 
# principal coordinate analysis to make a 2D map of the classes. Compare this 
# map to the map in the previous exercise? are they different? why?
#

library(png)
library(grid)
library(ggplot2)


wk_dir <- "C:/Users/irxumtenk/Google Drive/Education/MCS-DS_UIUC/CS 498 Applied Machine Learning/homework3/cifar-10-batches-bin/"
results_dir <- "C:/Users/irxumtenk/Google Drive/Education/MCS-DS_UIUC/CS 498 Applied Machine Learning/homework3/results/"

meta_pf <- paste(wk_dir, "batches.meta.txt", sep="")

data_fnames <- c("data_batch_1.bin",
                 "data_batch_2.bin",
                 "data_batch_3.bin",
                 "data_batch_4.bin",
                 "data_batch_5.bin",
                 "test_batch.bin")

#*************************#
# Part 0 Start
#*************************#
# Code in this section inspired by contents of posting below
# https://stackoverflow.com/questions/32113942/importing-cifar-10-data-set-to-r
#
# Read binary file and convert to integer vectors
# [Necessary because reading directly as integer() 
# reads first bit as signed otherwise]
#
# File format is 10000 records following the pattern:
# [label x 1][red x 1024][green x 1024][blue x 1024]
# NOT broken into rows, so need to be careful with "size" and "n"
#
# (See http://www.cs.toronto.edu/~kriz/cifar.html)
labels <- read.table(meta_pf)
images.rgb <- list()
images.lab <- list()
num.images = 10000 # Set to 10000 to retrieve all images per file to memory

# Cycle through all 5 binary files
for (f in 1:6) {
  to.read <- file(paste(wk_dir, data_fnames[f], sep=""), "rb")
  for(i in 1:num.images) {
    l <- readBin(to.read, integer(), size=1, n=1, endian="big")
    r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
    index <- num.images * (f-1) + i
    images.rgb[[index]] = data.frame(r, g, b)
    images.lab[[index]] = l+1
  }
  close(to.read)
  remove(l,r,g,b,f,i,index, to.read)
}

# function to run sanity check on photos & labels import
drawImage <- function(index) {
  # Testing the parsing: Convert each color layer into a matrix,
  # combine into an rgb object, and display as a plot
  img <- images.rgb[[index]]
  img.r.mat <- matrix(img$r, ncol=32, byrow = TRUE)
  img.g.mat <- matrix(img$g, ncol=32, byrow = TRUE)
  img.b.mat <- matrix(img$b, ncol=32, byrow = TRUE)
  img.col.mat <- rgb(img.r.mat, img.g.mat, img.b.mat, maxColorValue = 255)
  dim(img.col.mat) <- dim(img.r.mat)
  
  # Plot and output label
  
  grid.raster(img.col.mat, interpolate=FALSE)
  
  # clean up
  remove(img, img.r.mat, img.g.mat, img.b.mat, img.col.mat)
  
  labels[[1]][images.lab[[index]]]
}


disp_img <- function(img) {
  # hint from Ryan Yusko
  # and "baptiste" on StackOverflow https://stackoverflow.com/a/11306342
  r <- img[1:1024]
  g <- img[1025:2048]
  b <- img[2049:3072]
  img_matrix = rgb(r,g,b,maxColorValue=255)
  dim(img_matrix) = c(32,32)
  img_matrix = t(img_matrix) # fix to fill by columns
  grid.raster(img_matrix, interpolate=FALSE)
}

torow_rgb <- function(img_rgb) {
  temp_flat <- c(img_rgb$r, img_rgb$g, img_rgb$b)
  return(temp_flat)
}


drawImage(sample(1:(num.images*5), size=1))
drawImage(5)

itd <- images.rgb[[1]]
itdc <- c(itd$r,itd$g,itd$b)
itx <- images.rgb[[50]]
itxf <- torow_rgb(itx)

disp_img(itxf)

#*************************#
# Part 0 End
#*************************#


for (f in 1:6) {
  print(paste(wk_dir, data_fnames[f], ".bin", sep=""))
}


images_lab_v <- unlist(images.lab)

# si <- split(images.rgb, images.lab)

table(images_lab_v)


#*************************#
# Part 1 Start
#*************************#


pic_cf <- matrix(NA, ncol = 3*1024, nrow = 6000)
pc_m <- matrix(NA, ncol = 20, nrow = 6000)
pc_rot <- matrix(NA, ncol = 20, nrow = 3*1024)

key_lab <- 10

cat_err <- rep(1.0, length(labels[,1]))


for (kl in 3:10) {

  rk <- 1
  
  for (i in 1:length(images.rgb)) {
    
    if (images.lab[[i]] == kl) {
      tr <- torow_rgb(images.rgb[[i]])
      pic_cf[rk, ] <- tr
      rk <- rk + 1
    }
    
  }
  
  mean_pic_cf1 <- colMeans(pic_cf, 1)
  disp_img(as.integer(mean_pic_cf1))
  pic_name <- paste(toString(labels[kl,1]), ".png", sep="")
  pic_fname <- paste(results_dir, pic_name, sep="")
  #dev.copy(png, pic_fname)
  #dev.off()
  mean_name <- paste(toString(labels[kl,1]), ".csv", sep="")
  mean_fname <- paste(results_dir, mean_name, sep="")
  write.csv(mean_pic_cf1, mean_fname)
  print("finished saving mean image pic and data\n")
  
  pca_m <- prcomp(pic_cf)
  print("finished PCA\n")
  pc_use <- 20
  
  
  pc_m <- pca_m$x[,1:pc_use]
  pc_rot <- pca_m$rotation[,1:pc_use]
  
  pc_m_fname <- paste(results_dir,"pc_m",toString(kl),".csv",sep="")
  pc_rot_fname <- paste(results_dir,"pc_rot",toString(kl),".csv",sep="")
  write.csv(pc_m, pc_m_fname)
  write.csv(pc_rot, pc_rot_fname)
  print("finished writing 1st 20 PCA components to file\n")
  
  trunc_m <- pc_m %*% t(pc_rot)
  trunc_m <- scale(trunc_m, center = -1 * mean_pic_cf1, scale=FALSE)
  
  vlen <- 6000
  diff_err <- rep(0,vlen)
  for (j in 1:vlen) {
    imgids <- j
    trunc_ids <- trunc_m[imgids,]
    diff_ids <- trunc_m[imgids,] - pic_cf[imgids,]
    diff_ids_sq <- sapply(diff_ids, function(x) x^2)
    diff_ids_sq_sum <- sum(diff_ids_sq)
    diff_err[j] <- diff_ids_sq_sum
  }
  
  
  
  cat_err[kl] <- mean(diff_err)
  print("finished ")
  print(kl)
}  

# plot barchart for category error
err_df <- data.frame(labels, cat_err)
err_df$lab <- sapply(err_df$V1, toString)
ggplot(data=err_df, aes(x=V1, y=cat_err)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=sprintf("%0.0f", round(cat_err, digits = 0))), vjust=-0.3, size=3) +
  ylab(label="Average Sum of Squared Differences") + 
  xlab("") + 
  theme_minimal()

# diff_ids_sumsq <- diff_ids %*% diff_ids
# disp_img(as.integer(trunc_m[imgids,]))


#*************************#
# Part 1 End
#*************************#






#*************************#
# Part 2 Start
#*************************#

mean_name <- paste(toString(labels[kl,1]), ".csv", sep="")
mean_fname <- paste(results_dir, mean_name, sep="")

num_cat <- 10

mean_mat <- matrix(NA, ncol = 3*1024, nrow = num_cat)

labels_str <- sapply(labels$V1, toString)

for (fk in 1:num_cat) {
  
  mean_name <- paste(toString(labels[fk,1]), ".csv", sep="")
  mean_fname <- paste(results_dir, mean_name, sep="")
  
  mean_data <- read.csv(mean_fname)

  mean_mat[fk,] <- mean_data$x
}

dmean <- dist(mean_mat) # euclidean distances between the rows
dmean_m <- as.matrix(dmean)
dmean_fname <- paste(results_dir, "dmean.csv", sep="")
write.csv(dmean_m,dmean_fname)

fit <- cmdscale(dmean,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
# plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
#      main="Metric MDS", type="n")
# text(x, y, labels = labels_str, cex=.7) 

mds_df <- data.frame(x,y,labels_str)

ggplot(data=mds_df, aes(x=x, y=y)) +
  geom_point(colour="steelblue", shape = 5, size = 2, stroke = 2)+
  geom_text(aes(label= labels_str), nudge_y = -50) +
  ylab(label="") + 
  xlab("") + 
  ylim(-1500,1500) +
  xlim(-1500,1500) +
  theme_grey()


#*************************#
# Part 2 End
#*************************#



#*************************#
# Part 3 Start
#*************************#

# first save all pca components to disk to make things easier.

pc_m <- matrix(NA, ncol = 20, nrow = 6000)
pc_rot <- matrix(NA, ncol = 20, nrow = 3*1024)
set_A <- matrix(NA, ncol = 3*1024, nrow = 6000)
set_B <- matrix(NA, ncol = 3*1024, nrow = 6000)

num_cat <- 10

sim_m <- matrix(NA, ncol = num_cat, nrow = num_cat)


for (ii in 1:num_cat) {
  
  pcm_A_f <- paste(results_dir, "pc_m", toString(ii), ".csv", sep="")
  pcrot_A_f <- paste(results_dir, "pc_rot", toString(ii), ".csv", sep="")
  
  pcm_A <- as.matrix(read.csv(pcm_A_f)[,-1])
  pcrot_A <- as.matrix(read.csv(pcrot_A_f)[,-1])
  
  trunc_mA <- pcm_A %*% t(pcrot_A)
  # these next few lines is to get set of rgb's of A
  rk <- 1
  for (i in 1:length(images.rgb)) {
    if (images.lab[[i]] == ii) {
      tr <- torow_rgb(images.rgb[[i]])
      set_A[rk, ] <- tr
      rk <- rk + 1
    }
  }
  
  meanA_f <- paste(results_dir, toString(labels[ii,1]), ".csv", sep="")
  meanA <- as.vector(read.csv(meanA_f)[,-1])
  
  for (jj in 1:num_cat) {
    
    
    pcm_B_f <- paste(results_dir, "pc_m", toString(jj), ".csv", sep="")
    pcrot_B_f <- paste(results_dir, "pc_rot", toString(jj), ".csv", sep="")
    
    pcm_B <- as.matrix(read.csv(pcm_B_f)[,-1])
    pcrot_B <- as.matrix(read.csv(pcrot_B_f)[,-1])
    
    
    # these next few lines is to get set of rgb's of B
    rk <- 1
    for (i in 1:length(images.rgb)) {
      if (images.lab[[i]] == ii) {
        tr <- torow_rgb(images.rgb[[i]])
        set_B[rk, ] <- tr
        rk <- rk + 1
      }
    }
    
    meanB_f <- paste(results_dir, toString(labels[jj,1]), ".csv", sep="")
    meanB <- as.vector(read.csv(meanB_f)[,-1])
    
    trunc_mB <- pcm_B %*% t(pcrot_B)
    trunc_mAB <- scale(trunc_mB, center = -1 * meanA, scale=FALSE)
    
    trunc_mBA <- scale(trunc_mA, center = -1 * meanB, scale=FALSE) 
    
    diff_AB <- trunc_mAB - set_A
    diffsq_AB <- diff_AB^2
    sumdiffsq_AB <- rowSums(diffsq_AB)
    eAB <- mean(sumdiffsq_AB)
    
    diff_BA <- trunc_mBA - set_B
    diffsq_BA <- diff_BA^2
    sumdiffsq_BA <- rowSums(diffsq_BA)
    eBA <- mean(sumdiffsq_BA)
    
    sim_AnB <- 0.5*(eAB + eBA)
    
    sim_m[ii,jj] <- sim_AnB
    print(ii)
    print(jj)
    print(sim_AnB)

  }
  
}

# ut <- upper.tri(sim_m)
# 
# fm <- matrix(NA, ncol = num_cat, nrow = num_cat)
# fm <- as.matrix(Matrix::forceSymmetric(sim_m,uplo="U"))
# fm_df <- data.frame(fm)
# p3_fname <- paste(results_dir, "part3.csv", sep="")
# write.csv(fm_df,p3_fname)


fm_df <- data.frame(sim_m)
p3_fname <- paste(results_dir, "part3_B.csv", sep="")
write.csv(fm_df,p3_fname)



fitp3 <- cmdscale(sim_m,eig=TRUE, k=2) # k is the number of dim
fitp3 # view results

# plot solution
xp3 <- fitp3$points[,1]
yp3 <- fitp3$points[,2]
# plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
#      main="Metric MDS", type="n")
# text(x, y, labels = labels_str, cex=.7) 

simp3_df <- data.frame(xp3,yp3,labels_str)

ggplot(data=simp3_df, aes(x=xp3, y=yp3)) +
  geom_point(colour="orange", shape = 5, size = 2, stroke = 2)+
  geom_text(aes(label= labels_str), nudge_x = 1e6) +
  ylab(label="") + 
  xlab("") + 
  ylim(-1e7,1e7) +
  xlim(-1e7,1e7) +
  theme_grey()




#*************************#
# Part 3 End
#*************************#


#####

# im <- matrix(NA, ncol = 3*1024, nrow = num.images)
# 
# pic_array <- array(NA, c(10, 6*num.images, 3*1024))
# 
# ri <- rep(1,10)
# 
# for (i in 1:(6*num.images)){
#   idx <- images.lab[[i]]
#   print(idx)
#   tr <- torow_rgb(images.rgb[[i]])
#   pic_array[idx, ri[idx],] <- tr
#   ri[idx] <- ri[idx] + 1
# }


# images.rgb <- matrix(NA, ncol = 3*1024, nrow = num.images)
# images.lab <- vector(1, num.images)
# 
# for (f in 1:1) {
#   to.read <- file(paste(wk_dir, data_fnames[f], sep=""), "rb")
#   for(i in 1:2) {
#     l <- readBin(to.read, integer(), size=1, n=1, endian="big")
#     r <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
#     g <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
#     b <- as.integer(readBin(to.read, raw(), size=1, n=1024, endian="big"))
#     index <- num.images * (f-1) + i
#     images.rgb[index,] = c(r, g, b)
#     images.lab[index] = l+1
#   }
#   close(to.read)
#   remove(l,r,g,b,f,i,index, to.read)
# }
