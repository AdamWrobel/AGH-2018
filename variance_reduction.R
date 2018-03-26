#' @title Varaince reduction techniques in MC simulation by Renato Barros
#' @author Kamila Kazimierska

library(ggplot2)

## Antithetic sampling
# Instead of sampling 1000 (incerements - random normal samples)
# sample 500 and take 500 with changed sign + to -
# be careful, property of this method: changing the sign change speed of convergance

## Moment matching involves re-normalizing the Brownian increments at each time step 
# to force them to conform to N(0,1), normalize by substracting mean and dividing by sd
# be careful, property of this method: simulation might be depended on one path!

# Note, you can put a cup how hight the path can go the cap can

## Importance sampling dW' = dW + ??dt, S = S'/Z, where
# simulation of S' in this way will allow more paths to be excerised to 
# contribiute in the call price option (as an average - MC) (instead of 24 around 1000)
# Z(t) = exp(?? ???_0^t dW + ??^2/2 t),  where ?? arbitray
# Z = exp(theta*dW*dt + theta^2/2*t)

# dW - brownian incremnents, argument means method used for the incerments 
# default is standard, there is also AS and REN

monte_carlo <- function(n_paths, n_step, r, sigma, T, S0, dW = 'Standard', theta = 0 ) {
  n_steps <- n_step*T
  # dt = 1/n_step
  dt = 1/n_steps
  if(dW == 'AS'){
    Np = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths/2,n_steps)
    N_ = -Np
    N = rbind(Np,N_)
  } else { 
    N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)
    if(dW == "REN") {
      # Re - normalizujemy nasze przyrosty tylko gdy ta metoda jest wybrana
      # jak nie zostawiamy standardowo wygenerowane N
      N <- apply(N, 2, FUN = function(x){(x - mean(x))/sd(x)})
    } else {
      if(dW =="IS") { # Zmiana miary, to dw'
        N <- apply(N, 2, FUN = function(x){(x + theta*dt)}) # zmodyfikowany przyrost
  
      }
    }
  }
  
  paths2 = matrix(nrow=n_paths,ncol=n_steps+1,S0)	  
  # schemat Eulera 

  for (i in 1:n_paths){
    for (j in 1:n_steps) {
      paths2[i,j+1] = paths2[i,j] + r*paths2[i,j]*dt + sigma*paths2[i,j]*sqrt(dt)*N[i,j]
    }
  }
  # Martyngal przez ktory bedziemy dzielic nasza S'(T) uzyskana z metody IS musimy tu stowrzyc 
  # tutaj go tworzymy bo wymaga informacji o przyrostach 
  if(dW == "IS") {
    Z <- exp(theta*N[,n_steps]*dt+theta*theta*T/2)
    # Z = exp(theta*dW*dt + theta^2/2*t)
    # jak zwróciæ martynga³ razem z sciezkami procesu Wienera? np polacz cbind do zwracenego obiektu
    paths2 <- cbind(paths2,Z)
    
  }
  
  return(paths2)
}


##  Generated paths, see the diference 
# dW = AS, Standard, REN
paths2 <- monte_carlo(n_paths = 1000, n_step = 1, r = 0.03, sigma = 0.2, T = 10, S0 = 100,
                      dW = "REN")


n_step <-1
T = 10
n_steps <- n_step*T
dt = 1/n_steps
t = c(0,cumsum(rep(dt,n_steps)))
matplot(t(paths2[1:10,]),type='l')

# Create histogram of simulated prices 
# Step 1, run the MC simulation for stock price generation 100 times with diff METHODS!
# Step 2, each simulation step take an average of the price for last step (10th column)
# 1000 rows 10 colums 

# CHANGE THE PARAMETER dW TO a AS, b Standard c REN
 avg <- c(rep(1:100))
 for(i in 1:100) {
   set.seed(i)
   paths2 <-monte_carlo(n_paths = 1000, n_step =1, r = 0.03, sigma = 0.2, T=10, S0 =100, 
                        dW = "REN")
   avg[i] <- mean(paths2[,11])
 }

dt <- data.frame(step = c(rep(1:100)), means = avg)
ggplot(dt, aes(means, fill = 'blue')) + geom_histogram() +theme_bw() + 
ggtitle('Histogram of average of the average of the stock price')

sd(avg)

# dW = AS sd = 1.349
# dW = Standard sd = 2.418269
# dW = REN sd =  1.135772 BRAWO TY! 


# Zastosowanie metody importance sampling wykonujemy dopiero gdy chcemy wyceniæ opcjê! 
# W tej metodzie wiêcej œcie¿ek - tzn. przy stworzonej œcie¿ce cena akcji bêdzie wiêksza od strika
# st¹d opcje nie bêd¹ wygasa³y bez realizacji

# Mean(max(S'(T) – K, 0) / Z(T) ) x exp (-rT)
# K = S0 exp(µT + 2.5 ?? ???????)
# dw = IS wtedy zwracany obiekt to nie tylko sciezki procesu Wienera ale 
# martyngal Z ktorego bedziemy uzywac do dzielenia, potrzebny tylko w kroku ostatnim
# call price nie dziala bo ten strike za duzy
  
# K = S0*exp(r*T+2.5*sigma*sqrt(T)) # to ma byæ cena forward? wzor z prezentacji???
# K trzeba obliczyc tak zeby, cena call byla 0 
 K = 120
 S0 = 100
 T = 10
 r=0.03
 K = S0*exp(r*T)
  dW = 'IS'
  CP <- c(rep(1:100))
  
  for(i in 1:100) { # 100 wywo³uje cene opcji
    paths2 <- monte_carlo(n_paths = 1000,n_step = 1, r = 0.03, sigma = 0.2, T = 10, S0 = 100,
                          dW = "IS", theta = 0.2)
    if(dW == "IS") {
      Z <- paths2[,12]
      CP[i] <- mean(sapply(((paths2[,11]-K)/Z), FUN = function(x){max(x,0)}))*exp(-r*T)
    } else {
      CP[i] <- mean(sapply(paths2[,11]-K, FUN = function(x){max(x,0)}))*exp(-r*T)
    }
  }
  
  hist(CP)
  mean(CP)
  # 1.879633
  sd(CP)
  # 0.1576476



