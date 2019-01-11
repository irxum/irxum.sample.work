#
#
# hw6_prob2_v1.R
#
#
# Logistic regression The UCI Machine Learning dataset repository hosts a 
# dataset giving whether a Taiwanese credit card user defaults against a 
# variety of features here. Use logistic regression to predict whether the user 
# defaults. You should ignore outliers, but you should try the various 
# regularization schemes we have discussed. 


library(ggplot2)
library(glmnet)
library(readxl)
library(ResourceSelection)
library(data.table)

#base_dir <- "C:/Users/irxumtenk/Google Drive/Education/MCS-DS_UIUC/"
base_dir <- "C:/Users/akwan/Google Drive/Education/MCS-DS_UIUC/"

hw_dir <- "CS 498 Applied Machine Learning/homework6/"

fname <- "default of credit card clients.xls"

d_path <- paste(base_dir, hw_dir, fname, sep="")

tccd_df <- read_excel(d_path, skip = 1)

st_ncols <- dim(tccd_df)[2]

colnames(tccd_df)[st_ncols] <- "default"

tccd_df <- tccd_df[,2:st_ncols]

tccd_dt <- data.table(tccd_df)

tccd2_df <- tccd_df

names(tccd_dt)

tep <- unique(tccd_dt[,'EDUCATION'])

oneHotEncode <- function(dtt, colencode) {
  #print(colencode)
  lvls <- unique(dtt[colencode])
  print(class(lvls))
  print(lvls)

  for (lv in lvls) {
    cl = paste(colencode, lv, sep="")
    print(cl)
    dtt[cl] <- as.integer(dtt[colencode] == lv)
  }
  dtt[,ncol(dtt)] <- NULL # remove last column
  dtt[,colencode] <- NULL # remove original column
  return(dtt)
}

tccd_dt <- oneHotEncode(tccd2_df, 'SEX')
tccd_dt <- oneHotEncode(tccd_dt, 'EDUCATION')
tccd_dt <- oneHotEncode(tccd_dt, 'MARRIAGE')


# <<<<

#tccd_m = as.matrix(tccd_dt)

logit_tccd <- glm(formula = default ~., family = binomial(link = "logit"),
                  data = tccd_dt)

summary(logit_tccd)

logit_pred <- predict(logit_tccd, type="response")
logit_resid <- residuals(logit_tccd, type="deviance")

logit_tccdB_df <- data.frame(logit_resid, logit_pred)

ggplot(data=logit_tccdB_df, aes(x=logit_pred, y=logit_resid)) +
  geom_point(colour="brown", shape = 5, size = 0.5, stroke = 1) +
  ylab("residuals") + 
  xlab("fitted values of default") +
  #xlim(50, 175) +
  #ylim(-5, 5) +
  theme_grey()


hoslem.test(tccd_dt$default, fitted(logit_tccd))

logit_tccd_df <- data.frame(logit_tccd$residuals, logit_tccd$fitted.values)

names(logit_tccd_df) <- c('resid', 'fitval')

ggplot(data=logit_tccd_df, aes(x=fitval, y=resid)) +
  geom_point(colour="brown", shape = 5, size = 0.5, stroke = 1) +
  ylab("residuals") + 
  xlab("fitted values of default") +
  #xlim(50, 175) +
  #ylim(-5, 5) +
  theme_grey()


# try ridge

y_logit <- as.matrix(tccd_dt$default)
x_cc <- as.matrix(within(tccd_dt, rm('default')))

cvfit_logit <- cv.glmnet(x_cc, y_logit, alpha=0,type.measure="class")
plot(cvfit_logit)
cvfit_logit$cvm
idx <- which.min(cvfit_logit$lambda)
erratmin <- cvfit_logit$cvm[idx]
accatmin <- 1 - erratmin
accatmin

# try lasso

cvfit_logit <- cv.glmnet(x_cc, y_logit, alpha=1,type.measure="class")
plot(cvfit_logit)
cvfit_logit(type.measure="class")

# try elastic

cvfit_logit <- cv.glmnet(x_cc, y_logit, alpha=0.3,type.measure="class")
plot(cvfit_logit)

cvfit_logit <- cv.glmnet(x_cc, y_logit, alpha=0.5,type.measure="class")
plot(cvfit_logit)

cvfit_logit <- cv.glmnet(x_cc, y_logit, alpha=0.7,type.measure="class")
par(mar=c(5.1,4.1,5.1,2.1))
plot(cvfit_logit, main = "alpha =")

iterations = 11
variables = 2

output <- matrix(ncol=variables, nrow=iterations)

for (ii in 1:iterations) {
  ialpha = (ii - 1) * 0.1
  print(ialpha)
  cv_logit <- cv.glmnet(x_cc, y_logit, alpha = ialpha, type.measure = "class")
  cialpha = toString(ii - 1)
  pl_fn <- paste("cvfitplotalpha", cialpha, ".png", sep = "")
  pl_fp <- paste(base_dir, hw_dir, pl_fn, sep = "")
  png(pl_fp, width = 800, height = 600, units = "px")
  mtitle <- paste("alpha = ", toString(ialpha), sep ="")
  par(mar=c(5.1,4.1,5.1,2.1))
  plot(cv_logit, main = mtitle)
  dev.off()
  idx <- which(cv_logit$lambda == cv_logit$lambda.min)
  erratmin <- cv_logit$cvm[idx]
  accatmin <- 1 - erratmin
  output[ii,] <- c(ialpha, accatmin)
}


mm <- max(output[,2])
mi <- which.max(output[,2])

cv_logitm <- cv.glmnet(x_cc, y_logit, alpha = output[mi,1], 
                       type.measure = "class")
plot(cv_logitm, main = "alpha = 0.3")

tmp_coeffs <- coef(cv_logitm, s = "lambda.min")