library(readr)
wcgs_df <- read_csv("data/wcgs.csv")


yX_mat <- as.matrix(wcgs_df[, c("chol", "age", "weight", "sbp")])

y <- yX_mat[, 1]
X <- yX_mat[, c(2, 3, 4)] # or [, -1], or [, 2:4]

XT <- t(X)
XTX <- XT %*% X
# inv_XTX <- 1 / XTX # WRONG
inv_XTX <- solve(XTX)

# Check
inv_XTX %*% XTX
XTX %*% inv_XTX

beta_num <- inv_XTX %*% XT %*% y
anyNA(inv_XTX)
anyNA(XT)
anyNA(y)


###  NAs Removed  ###
yX2_mat <- as.matrix(wcgs_df[, c("chol", "age", "weight", "sbp")])
yX2_mat <- na.omit(yX2_mat)
# We lose 12 observations

y2 <- yX2_mat[, 1]
X2 <- yX2_mat[, c(2, 3, 4)] # or [, -1], or [, 2:4]

X2T <- t(X2)
X2TX2 <- X2T %*% X2
inv_X2TX2 <- solve(X2TX2)

beta_num <- inv_X2TX2 %*% X2T %*% y2


###  Add Intercept  ###
yX_mat <- as.matrix(wcgs_df[, c("chol", "age", "weight", "sbp")])
yX_mat <- na.omit(yX_mat)

y <- yX_mat[, 1]
X <- yX_mat[, c(2, 3, 4)] # or [, -1], or [, 2:4]
X <- cbind(1, X)
head(X)
X
XT <- t(X)
XTX <- XT %*% X  # matrix multiplication
inv_XTX <- solve(XTX)  # second argument is missing, then returns an inverce of XTX

beta_num <- inv_XTX %*% XT %*% y
beta_num

###  Check  ###
lm(chol ~ age + weight + sbp, data = wcgs_df)

###  Writing Function  ###

coef_lm <- function(data=data, responce, x1, x2, x3){
  yX_mat <- as.matrix(data[, c(responce, x1, x2, x3)])
  yX_mat <- na.omit(yX_mat)  # ommiting missing data
  y <- yX_mat[, 1]  # chosing responce vector
  X <- yX_mat[, c(2, 3, 4)]  # chosing predictors matrix
  X <- cbind(1, X)  # adding a design matrix
  XT <- t(X)  # transpose of matrix X
  XTX <- XT %*% X  # matrix multiplication
  inv_XTX <- solve(XTX)  # second argument is missing, then returns an inverce of XTX
  beta_num <- inv_XTX %*% XT %*% y
  beta_num
}
 # return(list(intercept=beta_num[1], beta_num[2]))                                             
# beta_num[3], beta_num[4])))
 # return(cbind(c(intercept, beta1, beta2, beta3, beta4), c(beta_num[1], beta_num[2], 
 #              beta_num[3], beta_num[4])))


new <- coef_lm(wcgs_df, "chol", "age", "weight", "sbp")
new

