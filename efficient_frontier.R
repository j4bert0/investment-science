# Takes a N-dimensional covariance matrix as parameter (by def. square matrix)
# Solves Lagrangian system of equations. The system is written as block matrix.
# Returns vector of asset weights corresponding to the minimum variance portfolio
solve_min_variance_p <- function(covm) {
  N <- dim(covm)[1]
  top.mat <- cbind(2*covm,rep(1,N))
  bot.vec <- c(rep(1,N),0)
  mat <-rbind(top.mat,bot.vec)
  b <- c(rep(0,N),1)
  z <- solve(mat)%*%b
  return(z[1:N,1])
}

# Takes 3 parameters:
#   - Target return for the portfolio (mu_t)
#   - Vector of assets expected returns (mu)
#   - N-dimensional covariance matrix as parameter (by def. square matrix)
# Solves Lagrangian system of equations. The system is written as block matrix.
# Returns vector of asset weights corresponding to the efficient portfolio
solve_eff_p <- function(mu_t,mu,covm) {
  N <- dim(covm)[1]
  top.mat <- cbind(2*covm,mu,rep(1,N))
  mid.vec <- c(mu,0,0)
  bot.vec <- c(rep(1,N),0,0)
  mat <-rbind(top.mat,mid.vec,bot.vec)
  b <- c(rep(0,N),mu_t,1)
  z <- solve(mat)%*%b
  return(z[1:N,1])
}

# Solving the efficient portfolio for returns 0.08 ... 0.3
# i.e. forming the portfolio curve and efficient frontier
min_r <- 0.08; max_r <- 0.3
tmp <- numeric((max_r-min_r)*1000)
eff_frontier <- data.frame(tmp,tmp)
colnames(eff_frontier) <- c("Return", "Std")
i <- 1
for (r in seq(min_r,max_r,0.001)) {
  w <- solve_eff_p(r,mu,covm)
  std <- sqrt(t(w)%*%covm%*%w)
  eff_frontier[i,1] <- r
  eff_frontier[i,2] <- std
  i <- i + 1
}

# Efficient frontier plot
plot(eff_frontier$Std,eff_frontier$Return,type="l",main="Mean-variance diagram",
     xlab="Standard deviation",ylab="Return",xlim=c(0,0.4),ylim=c(0,0.4))

lines(std_minv,r_minv,type="p",pch=20,col="red",cex=2) # Minimum variance
lines(std_18,r_18,type="p",pch=20,col="blue",cex=2) # Efficient portfolio for 18% target return

# Assets
lines(0.25,0.2,type="p",pch=20,col="green",cex=2) # A
lines(0.2,0.15,type="p",pch=20,col="green",cex=2) # B
lines(0.15,0.1,type="p",pch=20,col="green",cex=2) # C

# One-fund optimal portfolio
lines(std_onef,r_onef,type="p",pch=20,col="green",cex=2) # C

text(c(0.25,0.2,0.15,std_onef),c(0.2,0.15,0.1,r_onef),labels=c("A","B","C","OPT"),cex=0.7,pos=3)

abline(a=r_f,b=(r_onef-r_f)/std_onef,col="green")