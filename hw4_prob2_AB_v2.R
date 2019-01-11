#
#
# hw4_prob2.R
#
#
# Do exercise 6.2 in the Jan 15 version of the course text 
#
# 6.2. Obtain the activities of daily life dataset from the UC Irvine machine 
# learning website:
# (https://archive.ics.uci.edu/ml/datasets/Dataset+for+ADL+Recognition+with+Wrist-worn+Acceleromete
# data provided by Barbara Bruno, Fulvio Mastrogiovanni and Antonio Sgor-bissa).
# (a) Build a classiffier that classifies sequences into one of the 14 activities 
# provided. To make features, you should vector quantize, then use a histogram
# of cluster centers (as described in the subsection; this gives a pretty 
# explicit set of steps to follow). You will find it helpful to use hierarchical
# k-means to vector quantize. You may use whatever multi-class classifier
# you wish, though I'd start with R's decision forest, because it's easy to
# use and effective. You should report (a) the total error rate and (b) the
# class confusion matrix of your classifier.
# (b) Now see if you can improve your classifier by (a) modifying the number
# of cluster centers in your hierarchical k-means and (b) modifying the size
# of the fixed length samples that you use.

library(ggplot2)
library(randomForest)


# get folder names which become the classes

#wk_dir <- "C:/Users/irxumtenk/Google Drive/Education/MCS-DS_UIUC/CS 498 Applied Machine Learning/homework4/"
wk_dir <- "C:/Users/akwan/Google Drive/Education/MCS-DS_UIUC/CS 498 Applied Machine Learning/homework4/"

dt_dir <- "HMP_Dataset/"

wd_path <- paste(wk_dir, dt_dir, sep="")


cls <- list.dirs(wd_path, full.names = FALSE)
cls <- cls[!grepl("_MODEL",cls)] # avoid folders with "_MODEL" in the name
cls <- cls[cls != ""] # avoid empty string folders



# Cycle through files to count them

fcount <- 0

for (cs in cls) {
  cls_path <- paste(wk_dir, dt_dir, "/",cs,sep="")
  cls_files <- list.files(cls_path)
  fcount <- fcount + length(cls_files)
}



test_frxn <- 0.2
set.seed(5)

test_lbl <- rep("", fcount)
train_lbl <- rep("", fcount)
test_fp <- rep("", fcount)
train_fp <- rep("", fcount)

train_st <- 1
test_st <- 1

for (cs in cls) {
  
  cls_path <- paste(wk_dir, dt_dir, "/",cs,"/",sep="")
  cls_files <- list.files(cls_path) 
  #print(length(acc_files))
  
  num_tot <- length(cls_files)
  num_test <- ceiling(num_tot * test_frxn) # use ceiling to ensure at least one test
  num_train <- num_tot - num_test 

  full_idx <- seq(num_tot)
  train_idx <- sort(sample(full_idx, num_train))
  test_idx <- subset(full_idx, !(full_idx %in% train_idx))
  
  train_nd <- train_st + num_train - 1
  test_nd <- test_st + num_test - 1
  
  fp_temp1 <- paste(cs, "/", cls_files[train_idx], sep = "")
  train_fp[train_st:train_nd] <- fp_temp1
  train_lbl[train_st:train_nd] <- rep(cs, num_train)
  
  fp_temp2 <- paste(cs, "/", cls_files[test_idx], sep="")
  test_fp[test_st:test_nd] <- fp_temp2
  test_lbl[test_st:test_nd] <- rep(cs, num_test)
  
  train_st <- train_nd + 1
  test_st <- test_nd + 1
  
}

train_fp <- train_fp[train_fp != ""]
train_lbl <- train_lbl[train_lbl != ""]

test_fp <- test_fp[test_fp != ""]
test_lbl <- test_lbl[test_lbl != ""]

toflat <- function(threecoldf) {
  temp_flat <- c(threecoldf[,1], threecoldf[,2], threecoldf[,3])
  return(temp_flat)
}

# function definition
cutChunks <- function(fpv, vl, vc) {
  
  chunk_vctr <- list()
  chunk_count <- 0
  chunk_fn <- list()
  fn_idx <- 1
  
  for (af in fpv) {
    
    af_path <- paste(wd_path, "/", af, sep="")
    af_df <- read.table(af_path)
    dim_af_df <- dim(af_df)
    
    num_chunks <- ceiling(dim_af_df[1] / vl)
    rst <- 1 # restart index for chunking
    
    for (nc in (1:num_chunks)) {
      
      if (nc == num_chunks) {
        rst <- dim_af_df[1] - vl + 1
        rend <- dim_af_df[1] # must take care of chunks at end of file
        #print(rst)
        #print(rend)
      } else {
        rend <- nc * vl
      }
      
      chunk_temp <- af_df[rst:rend,]
      chunk_temp <- toflat(chunk_temp)
      chunk_count <- chunk_count + 1
      
      chunk_vctr[[chunk_count]] <- chunk_temp
      chunk_fn[[chunk_count]] <- fn_idx
      rst <- rst + vl
    }
    px <- sprintf("chunk_count = %d\n", chunk_count)
    print(px)
    fn_idx <- fn_idx + 1
  }
  
  ucv <- unlist(chunk_vctr)
  ucf <- unlist(chunk_fn)
  chunk_mat <- matrix(ucv, ncol = (vl * vc), byrow = TRUE)
  chunk_df <- data.frame(ucf,chunk_mat)
  return(chunk_df)
}

vlen <- 32
vcols <- 3

full_fp <- append(train_fp, test_fp)
full_lbl <- append(train_lbl, test_lbl)
train_cv <- cutChunks(train_fp, vlen, vcols)
test_cv <- cutChunks(test_fp, vlen, vcols)
full_cv <- rbind(train_cv, test_cv)
  
k <- 480
nd <- vlen * vcols + 1
kclus <- kmeans(full_cv[,2:nd], k, nstart = 10)

sgl_clus_df <- data.frame(full_cv$ucf, kclus$cluster)  
st <- 1
nd <- dim(train_cv)[1]
train_clus_df <- sgl_clus_df[st:nd,]
st <- nd + 1
nd <- dim(full_cv)[1]
test_clus_df <- sgl_clus_df[st:nd,]

train_clus_ls <- split(train_clus_df, train_clus_df$full_cv.ucf)
test_clus_ls <- split(test_clus_df, test_clus_df$full_cv.ucf)

train_mat <- matrix(NA, ncol = k, nrow = length(train_fp))
test_mat <- matrix(NA, ncol = k, nrow = length(test_fp))
kbins <- 1:k


# This function takes the result of a table call (tbl) and 
# number of clusters and returns a vector of frequencies
# the index corresponds to the cluster identifier #
# the value indicates the frequency.  Unlike the result of 
# table, this function fills in zeros 
toFreqVctr <- function(tb, nk) {
  tbdf <- as.data.frame(tb)
  names(tbdf) <- c("ch","freq")
  vctr <- rep.int(0, nk)
  ch <- as.integer(paste(tbdf$ch))
  vctr[ch] <- tbdf$freq
  return(vctr)
}

cmi <- 1
for (tc in train_clus_ls) {
  sghist <- table(tc[,2])
  sgfreq <- toFreqVctr(sghist,k)
  train_mat[cmi,] <- sgfreq
  cmi <- cmi + 1
  if (cmi %% 100) {print (cmi)}
}

cmi <- 1
for (tc in test_clus_ls) {
  sghist <- table(tc[,2])
  sgfreq <- toFreqVctr(sghist,k)
  test_mat[cmi,] <- sgfreq
  cmi <- cmi + 1
  if (cmi %% 100) {print (cmi)}
}

full_mat <- rbind(train_mat,test_mat)

rf_sl_df <- data.frame(full_lbl, full_mat)

train_idx <- 1:length(train_fp)
acc.rf <- randomForest(full_lbl ~ ., data = rf_sl_df, subset = train_idx)

cfres <- acc.rf$confusion
res_fname <- "cfres.csv"
write.csv(cfres, file = paste(wk_dir,"/",res_fname,sep=""))


# PART B of Problem 2
# Now try to tune k and vlen to get better 
vlen_v <- c(12, 24, 36, 48, 60, 72)
k_v <- c(120)

vk_mat <- matrix(NA, ncol=length(k_v), nrow = length(vlen_v))

rowi <- 1

for (vl in vlen_v) {
  
  train_cv <- cutChunks(train_fp, vl, vcols)
  test_cv <- cutChunks(test_fp, vl, vcols)
  full_cv <- rbind(train_cv, test_cv)
  
  px <- sprintf("vlen = %d\n", vl)
  print(px)
  
  colj <- 1
  
  for (ki in k_v) {
    
    nd <- vl * vcols + 1
    kclus <- kmeans(full_cv[,2:nd], ki, nstart = 10)
    px <- sprintf("finished kmeans with k = %d", ki)
    print(px)
    
    sgl_clus_df <- data.frame(full_cv$ucf, kclus$cluster)  
    st <- 1
    nd <- dim(train_cv)[1]
    train_clus_df <- sgl_clus_df[st:nd,]
    st <- nd + 1
    nd <- dim(full_cv)[1]
    test_clus_df <- sgl_clus_df[st:nd,]
    
    train_clus_ls <- split(train_clus_df, train_clus_df$full_cv.ucf)
    test_clus_ls <- split(test_clus_df, test_clus_df$full_cv.ucf)
    
    train_mat <- matrix(NA, ncol = ki, nrow = length(train_fp))
    test_mat <- matrix(NA, ncol = ki, nrow = length(test_fp))

    cmi <- 1
    for (tc in train_clus_ls) {
      sghist <- table(tc[,2])
      sgfreq <- toFreqVctr(sghist,ki)
      train_mat[cmi,] <- sgfreq
      cmi <- cmi + 1
      if (cmi %% 10 == 0) {
        px <- sprintf("train cmi = %d", cmi)
        print(px)
      }
    }
    
    cmi <- 1
    for (tc in test_clus_ls) {
      sghist <- table(tc[,2])
      sgfreq <- toFreqVctr(sghist,ki)
      test_mat[cmi,] <- sgfreq
      cmi <- cmi + 1
      if (cmi %% 10 == 0) {
        px <-sprintf("test cmi = %d", cmi)
        print(px)
      }
    }
    
    full_mat <- rbind(train_mat,test_mat)
    
    rf_sl_df <- data.frame(full_lbl, full_mat)
    
    train_idx <- 1:length(train_fp)
    acc.rf <- randomForest(full_lbl ~ ., data = rf_sl_df, subset = train_idx)
    px <- sprintf("finished random forest with k = %d", ki)
    print(px)
    
    arnt <- acc.rf$ntree
    aroob <- acc.rf$err.rate[arnt,1]
    accu <- 1 - as.numeric(aroob)
    
    vk_mat[rowi, colj] <- accu
    px <- sprintf("vlen = %d, k = %d, accuracy = %f", vl, ki, accu)
    print(px)
  
    colj <- colj + 1
  }
  rowi <- rowi + 1
}

accu_fname <- "accu.csv"
write.csv(vk_mat, file = paste(wk_dir,"/",accu_fname,sep=""))

