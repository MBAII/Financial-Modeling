rm(list=ls())
# install.packages("quantmod")
library("quantmod")
library("alabama")
library("ggplot2")

mv_model <- function(s,rf,beginDate,endDate,ret){
  #message(ret)
  print(beginDate)
  print(endDate)
  rf <- as.numeric(rf)
  ##############################
  tics <- unlist(strsplit(s, ","))
  P.list <- lapply(tics, function(tic)
    get(getSymbols(tic, from = beginDate, to = endDate)))
  sapply(P.list,nrow)

  # Get the adjusted prices into a single object
  P.adj <- lapply(P.list, function(p) p[,6])
  # Merge the elements of the list
  P <- Reduce(merge,P.adj)
  

  
  
  
  names(P) <- tics

  # Returns:
  Rets <- P/lag(P) - 1
  # Define parameters for the optimization:
  # 1. The returns: retain the mean returns for all non-missing observations
  R <- apply(Rets,2,function(x) mean(x,na.rm = TRUE))
  # 2. The covariance matrix: run pairwise correlations
  Sigma <- var(Rets, use="pairwise")
  # Annualize data:
  R <- (1 + R)^252 - 1
  Sigma <- Sigma * 252
  
  #######################change R and SIgma#################
  R <- c(R,rf)
  tics <- c(tics,"RF") 
  n <- length(R)
  # names(R) <- tics
  Sigma <- rbind(Sigma,rep(0,length=n))
  Sigma <- cbind(Sigma,rep(0,length=(n+1)))
  ###########################################################

  # Generate many random portfolios to plot:
  R_pf <- function(w,R=R){
    r <- t(w) %*% R
    return(r)
  }

  S_pf <- function(w,Sigma=Sigma){
    s <- t(w) %*% Sigma %*% w
    return(s)
  }

  randPfs <- function(R,Sigma){
    w <- rep(NA,length=n)
    w[1] <- runif(1)
    for (i in 2:n){
      w[i] <- runif(1,min=0,max=1 - sum(w,na.rm=TRUE))
    }
    w <- w / sum(w)
    w <- sample(w)
    R0 <- R_pf(w,R)
    S0 <- S_pf(w,Sigma)
    results <- list("R"=R0,"S"=S0,"w"=w)
    return(results)
  }


  N <- n*1000
  pfs <- as.data.frame(matrix(NA,ncol=2+n,nrow=N))
  for (i in 1:N){
    tmp <- randPfs(R,Sigma)
    pfs[i,] <- matrix(c(tmp$R,sqrt(tmp$S),t(tmp$w)))
  }

  # The following block of code adds a single row to the pfs data frame # that will be used later to generate a random starting value
  # in the portfolio optimization routine.
  x0 <- rep(0,length=n)
  x0[which.max(R)] <- 1
  pfs <- rbind(pfs,c(max(R),sqrt(S_pf(x0,Sigma)),x0))

  # Plot the results:
  p <- ggplot(data=pfs,aes(x=V2,y=V1)) +
    geom_point(color = "yellow") +
    ylab(label="R_pf") +
    xlab(label="sigma_pf") +
    ggtitle("Random Portfolios") +
    expand_limits(x=0, y=0)

  # Plot the 100% portfolios:
  for (i in 1:n){
    wtmp <- rep(0,length=n)
    wtmp[i]=1
    p <- p + geom_point(x=c(sqrt(S_pf(wtmp,Sigma))),y=c(R_pf(wtmp,R)),color = "green")
  }


  hin <- function(x){
    h <- x
    return(x)
  }

  # Equality
  heq <- function(x){
    h <- sum(x) - 1
    return(h)
  }

  # ------Objective Function: Minimize Variance-----
  eval_f <- function(x){
    s <- t(x) %*% Sigma %*% x
    return(s)
  }

  # Set the starting valae to a feasible portfolio:
  x0 <- rep(1/n,length=n)
  # Solve the optimization problem:
  tmp <- constrOptim.nl(par=x0, fn=eval_f, heq=heq, hin=hin)

  # Print the return and risk of the estimated minimum variance portfolio:
  Rmin <- rf
  #Rmin <- ret
  Smin <- 0
  message(sprintf("Minimum variance portfolio:\n\tR = %5.4f\n\tSigma=%5.4f",Rmin,Smin))

  # Plot the minimum variance portfolio (orange):
  p <- p + geom_point(x=c(Smin),y=c(Rmin),color="orange") +
    annotate("segment", x = 0.25, xend = Smin,y = 0.1,  yend = Rmin, colour = "orange",size=1, alpha=0.6, arrow=arrow()) +
    annotate("text", x = c(0.25), y = c(0.1),  label = c("Min Variance") , color="black", size=4 , angle=0)

  #The plot the maximim return portfolio:
  wmax <- rep(0,length=n)
  wmax[which.max(R)] <- 1
  Rmax <- R_pf(wmax,R)
  Smax <- sqrt(S_pf(wmax,Sigma))
  p <- p + geom_point(x=c(Smax),y=c(Rmax),color="orange") +
    annotate("segment", x = 0.5, xend = Smax,y = 0.35, yend = Rmax, colour = "orange",size=1, alpha=0.6, arrow=arrow()) +
    annotate("text", x = c(0.5), y = c(0.35),label = c("Max Return") , color="black", size=4 , angle=0)


  # --------------Efficient Frontier----------------
  # Constraints wi >= 0
  # Set this in the loop because it changes every time
  # sum(w) = 1
  heq <- function(x){
    h <- sum(x) - 1 
    return(h)
  }
  # Set the number of portoflios along [Rmin,Rmax]
  npf <- 50
  soln <- as.data.frame(matrix(NA,ncol=2+n,nrow=npf))
  Rconst <- seq(max(R),ret,length=npf)

  # Set a starting value to a feasible portfolio
  # (Note that if the feasible region changes throughout the # loop, then you will need to change x0 in the loop)
  x0 <- rep(0,length=n)
  x0[which.max(R)] <- 1
  for (i in 1:npf){
    hin <- function(x){
      eps <- 1e-6
      h <- rep(NA,1)
      # Define no shorting constraints (the eps is to # mitigate the impact of rounding erros)
      for (j in 1:length(x)){
        h[j] <- x[j] + eps
      }
      # Define the return constraint (Note - this is # the only element that changes in the loop)

      h[length(x)+1] <- t(x)%*%R  - Rconst[i] + eps
      return(h)
    }
    # Solve the problem (the tryCatch() assures the # code will run if a single iteration returns an # error)
    tryCatch({
      tmp <- constrOptim.nl(par=x0, fn=eval_f,
                            heq=heq, hin=hin, "control.outer"=list("trace"=FALSE), "control.optim"=list("reltol"=1e-12,
                                                                                                        "trace"=0)) },error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    # Save the resulting weight vector
    wtmp <- tmp$par
    # Save the results
    soln[i,2] <- sqrt(S_pf(wtmp,Sigma))
    soln[i,1] <- R_pf(wtmp,R)
    soln[i,c(3:(n+2))] <- wtmp
    message(sprintf("%d. %6.5f\t%6.5f\t%6.5f\t%d\t%6.5f\t%8.7f\t%d\t%d",
                    i,Rconst[i],soln[i,1],soln[i,2], tmp$outer.iterations,sum(wtmp), Rconst[i]-soln[i,1], tmp$convergence,
                    tmp$counts[1]))

    weight <- as.data.frame(matrix(c(tmp$par),nrow = 1))
    
  }

  # Add results to the plot:
  p <- p + geom_point(data=soln, aes(x=soln[,2],y=soln[,1]),color="blue")
  
  
  # weight <- as.data.frame(matrix(c(tmp$par),nrow = 1))
  message(weight)
  names(weight) <- c(tics)
  message(weight)
  
  newList <- list("plot" = p, "weight" = weight)
  return(newList)
}
