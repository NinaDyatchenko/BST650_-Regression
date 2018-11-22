wcgs_df <- read_csv("C:/Users/Nina/Documents/GitHub/BST650_Regression/wcgs.csv")


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

getcoeff <- function(y_num, X_mat){
  # browser() black magic
  
  y_mat <- as.matrix(y_num)
  namesX <- colnames(X_mat)
  X_mat <- as.matrix(cbind(1, X_mat))
  
  # Remove NAs.
  #puts matrix together to cut same rows from 
  #both matrices, then devide the yX_mat back to y_mat and X_mat
  
  yX_mat <- cbind(y_mat, X_mat)  
  yX_mat <- na.omit(yX_mat)
  y_mat <- yX_mat[, 1]
  X_mat <- yX_mat[, -1]
  
  # Names
  if (is.null(namesX)){
    slopenames <- paste0("beta", 1:(ncol(X_mat)-1))
    colnames(X_mat) <- c("Intercept", slopenames)
  } else{
    colnames(X_mat)[1] <- "Intercept"
  }
  
  # Slopes
  XT <- t(X_mat)  # transpose of matrix X
  XTX <- XT %*% X_mat  # matrix multiplication
  inv_XTX <- solve(XTX)
  beta_num <- inv_XTX %*% XT %*% y_mat
  beta_num
  
}

# Test
testX <- wcgs_df[, c("age", "weight", "sbp")]
getcoeff(
  y_num = wcgs_df$chol,
  X_mat = testX
)
lm(chol ~ age + weight + sbp, data = wcgs_df)

colnames(testX) <- NULL
getcoeff(
  y_num = wcgs_df$chol,
  X_mat = testX
)
