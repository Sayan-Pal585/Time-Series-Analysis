##Cost function

crost.cost <- function(p0,data,cost,type,w,nop,w.opt,init,init.opt,lbound,ubound){
  # Cost functions for Croston and variants  
  
  if (w.opt == TRUE && init.opt == TRUE){
    frc.in <- crost(data=data,w=p0[1:nop],h=0,init=p0[(nop+1):(nop+2)],
                    type=type,opt.on=TRUE)$frc.in
  } else if (w.opt == TRUE && init.opt == FALSE){
    frc.in <- crost(data=data,w=p0[1:nop],h=0,init=init,
                    type=type,opt.on=TRUE)$frc.in
  } else if (w.opt == FALSE && init.opt == TRUE){
    frc.in <- crost(data=data,w=w,h=0,init=p0,
                    type=type,opt.on=TRUE)$frc.in
  }
  
  if (cost == "mse"){
    E <- data - frc.in  
    E <- E[!is.na(E)]
    E <- mean(E^2)
  } else if(cost == "mae"){
    E <- data - frc.in  
    E <- E[!is.na(E)]
    E <- mean(abs(E))
  } else if(cost == "mar"){
    n <- length(data)
    temp <- cumsum(data)/(1:n)
    n <- ceiling(0.3*n)
    temp[1:n] <- temp[n]
    E <- abs(frc.in - temp)
    E <- E[!is.na(E)]
    E <- sum(E)
  } else if(cost == "msr"){
    n <- length(data)
    temp <- cumsum(data)/(1:n)
    n <- ceiling(0.3*n)
    temp[1:n] <- temp[n]
    E <- (frc.in - temp)^2
    E <- E[!is.na(E)]
    E <- sum(E)    
  }
  
  # Constrains
  for (i in 1:(nop*w.opt+2*init.opt)){
    if (!(p0[i]>=lbound[i]) | !(p0[i]<=ubound[i])){
      E <- 9*10^99
    }
  }
  
  return(E)
  
}

##Crost.opt

crost.opt <- function(data,type=c("croston","sba","sbj"),cost=c("mar","msr","mae","mse"),
                      w=NULL,nop=c(2,1),init,init.opt=c(TRUE,FALSE)){
  # Optimisation function for Croston and variants
  
  type <- type[1]
  cost <- cost[1]
  nop <- nop[1]
  init.opt <- init.opt[1]
  
  # Croston decomposition
  nzd <- which(data != 0)               # Find location on non-zero demand
  k <- length(nzd)
  x <- c(nzd[1],nzd[2:k]-nzd[1:(k-1)])  # Intervals
  
  if (is.null(w) == TRUE && init.opt == FALSE){
    # Optimise only w
    p0 <- c(rep(0.05,nop))
    lbound <- c(rep(0,nop))
    ubound <- c(rep(1,nop))
    if (nop != 1){
      wopt <- optim(par=p0,crost.cost,method="Nelder-Mead",data=data,cost=cost,
                    type=type,w=w,nop=nop,w.opt=is.null(w),init=init,init.opt=init.opt,
                    lbound=lbound,ubound=ubound,control=list(maxit=2000))$par    
    } else {
      # Use Brent
      wopt <- optim(par=p0,crost.cost,method="Brent",data=data,cost=cost,
                    type=type,w=w,nop=nop,w.opt=is.null(w),init=init,init.opt=init.opt,
                    lbound=lbound,ubound=ubound,lower=lbound,upper=ubound,
                    control=list(maxit=2000))$par  
    }
    wopt <- c(wopt,init)
  } else if (is.null(w) == TRUE && init.opt == TRUE){
    # Optimise w and init
    p0 <- c(rep(0.05,nop),init[1],init[2])
    lbound <- c(rep(0,nop),0,1)
    ubound <- c(rep(1,nop),max(data),max(x))
    wopt <- optim(par=p0,crost.cost,method="Nelder-Mead",data=data,cost=cost,
                  type=type,w=w,nop=nop,w.opt=is.null(w),init=init,init.opt=init.opt,
                  lbound=lbound,ubound=ubound,control=list(maxit=2000))$par
  } else if (is.null(w) == FALSE && init.opt == TRUE){
    # Optimise only init
    nop <- length(w)
    p0 <- c(init[1],init[2])
    lbound <- c(0,1)
    ubound <- c(max(data),max(x))
    wopt <- optim(par=p0,crost.cost,method="Nelder-Mead",data=data,cost=cost,
                  type=type,w=w,nop=nop,w.opt=is.null(w),init=init,init.opt=init.opt,
                  lbound=lbound,ubound=ubound,control=list(maxit=2000))$par
    wopt <- c(w,wopt)
  }
  
  return(list(w=wopt[1:nop],init=wopt[(nop+1):(nop+2)]))
  
}
