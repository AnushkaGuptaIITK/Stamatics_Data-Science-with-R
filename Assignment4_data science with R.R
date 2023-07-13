#############Q1
A <- matrix(rnorm(1000*1000), nrow = 1000, ncol = 1000)
##########loops
column_norms_loop <- numeric(ncol(A))
for (i in 1:ncol(A)) {
  column_norms_loop[i] <- sqrt(sum(A[, i]^2))
}
##############sapply

column_norms_sapply <- sapply(1:ncol(A), function(i) sqrt(sum(A[, i]^2)))
#########sapply is faster in execution as compared to loop method because sapply uses vectorization which is faster than for loop in loops method




#############Q2
n <- 1000
m <- 1000
A <- matrix(runif(n * m), nrow = n, ncol = m)
B <- matrix(rnorm(n * m), nrow = n, ncol = m)
x <- runif(m)

ABtx <- crossprod(A, B) %*% x


############Q3
# Load the ques3.Rdata file
load("ques3.Rdata")

# Calculate the determinant of mat using logarithmic transformation
det_A <- sum(log(abs(eigen(mat, only.values = TRUE)$values)))

# Calculate p!
p <- dim(mat)[1]
fact_p <- sum(log(1:p))
trace_A <- sum(eigen(mat, only.values = TRUE)$values)
answer <- det_A / p + fact_p + p * log(2.7) - log(p) + log(trace_A)
ans <- exp(answer)



################Q4
n <- 50
m <- 1e3
A <- matrix(runif(n * m), nrow = n, ncol = m)
p_vec <- sqrt(colSums(A^2))
p_vec <- p_vec / sum(p_vec)
chosen <- sample.int(m, size = 1, prob = p_vec)



##############Q5
autoreg_fast <- function(n, rho) {
  error <- rnorm(n - 1)
  out <- numeric(n)
  out[1] <- 0
  for (t in 2:n) {
    out[t] <- rho * out[t - 1] + error[t - 1]
  }
  return(out)
}

############Q6
library(Rcpp)
sel_sums <- function(mat) {
  p <- ncol(mat)
  s <- sample(1:p, size = 1)
  
  cppFunc('NumericVector sumsC(NumericMatrix mat, int s) {
    int p = mat.ncol();
    NumericVector sums(p);
    
    for (int i = 0; i < s; i++) {
      for (int j = 0; j < p; j++) {
        sums[j] += mat(j, i);
      }
    }
    
    return sums;
  }')
  
  sumsC(mat, s)
}


