### Ćwiczenie 1
### Wygenerować 10,000 ścieżek procesu Wienera na przedziale [0,1] ze 100 równymi krokami symulacji
### Narysować 10 pierwszych ścieżek

n_paths = 10000
n_steps = 100
dt = 1/n_steps

N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)
paths = matrix(nrow=n_paths,ncol=n_steps+1,0)

for (i in 1:n_paths){
  for (j in 1:n_steps) {
    paths[i,j+1] = paths[i,j] +sqrt(dt)*N[i,j]
  }
}

matplot(t(paths[1:10,]),type='l')
grid()


### Ćwiczenie 2
### Wygenerować 10,000 ścieżek procesu Wienera na przedziale [0,1] ze 100 równymi krokami symulacji 
### używając funkcji cumsum i apply. Narysować 10 pierwszych ścieżek.

n_paths = 10000
n_steps = 100
dt = 1/n_steps

N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)
paths = matrix(nrow=n_paths,ncol=n_steps+1,0)

N=sqrt(dt)*N
paths = cbind(rep(0,n_paths),t(apply(N,1,cumsum)))

matplot(t(paths[1:10,]),type='l')
grid()


### Ćwiczenie 3
### Wygenerować 10,000 ścieżek geometrycznego ruchu Browna (model Blacka-Scholesa) na przedziale [0,3] 
### (do trzech lat) z 12 krokami symulacji na rok trzema metodami (dokładna dyskretyzacja, 
### schemat Eulera i schemat Milsteina)  na podstawie tych samych próbek z rozkładu normalnego. 
### Przyjąć r=0.02, sigma=0.25.
### Porównać pierwsze ścieżki uzyskane każdą z metod na jednym rysunku.

n_paths = 10000
T = 3
n_steps_per_year = 12
n_steps = n_steps_per_year * T
dt = 1/n_steps_per_year
S0 = 100
r = 0.02
sigma = 0.25

N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)
paths1 = matrix(nrow=n_paths,ncol=n_steps+1,S0)   #dokładna dyskretyzacja
paths2 = matrix(nrow=n_paths,ncol=n_steps+1,S0)	  #schemat Eulera
paths3 = matrix(nrow=n_paths,ncol=n_steps+1,S0)   #schemat Milsteina

for (i in 1:n_paths){
  for (j in 1:n_steps) {
    paths1[i,j+1] = paths1[i,j]*exp((r-sigma^2/2)*dt+sigma*sqrt(dt)*N[i,j])
    paths2[i,j+1] = paths2[i,j] + r*paths2[i,j]*dt + sigma*paths2[i,j]*sqrt(dt)*N[i,j]
    paths3[i,j+1] = paths3[i,j] + r*paths3[i,j]*dt + sigma*paths3[i,j]*sqrt(dt)*N[i,j] + 1/2*sigma^2*paths3[i,j]*dt*(N[i,j]^2-1)
  }
}

t = c(0,cumsum(rep(dt,n_steps)))

matplot(t,paths1[1,],type='l',col="red")
lines(t,paths2[1,],type='l',col="blue")
lines(t,paths3[1,],type='l',col="green")
grid()


### Ćwiczenie 4
### Wygenerować 10,000 ścieżek procesu CIR (patrz notatki z zajęć) na przedziale [0,1] 
### z 252 krokami symulacji (schemat Eulera i schemat Milsteina) na podstawie 
### tych samych próbek z rozkładu normalnego. Przyjąć kappa=10.0, theta=0.03, r0=0.05, sigma=0.25.
### Porównać pierwsze ścieżki uzyskane każdą z metod na jednym rysunku.

n_paths = 10000
T = 1
n_steps_per_year = 100
n_steps = n_steps_per_year * T
dt = 1/n_steps_per_year
r0 = 0.05
kappa = 10.0
theta = 0.03
sigma = 0.25

N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)
paths1 = matrix(nrow=n_paths,ncol=n_steps+1,r0)   #schemat Eulera
paths2 = matrix(nrow=n_paths,ncol=n_steps+1,r0)	  #schemat Milsteina

for (i in 1:n_paths){
  for (j in 1:n_steps) {
    paths1[i,j+1] = paths1[i,j] + kappa*(theta-paths1[i,j])*dt + sigma*sqrt(paths1[i,j])*sqrt(dt)*N[i,j]
    paths2[i,j+1] = paths2[i,j] + kappa*(theta-paths2[i,j])*dt + sigma*sqrt(paths2[i,j])*sqrt(dt)*N[i,j]+sigma^2*1/4*dt*(N[i,j]^2-1)
  }
}

t = c(0,cumsum(rep(dt,n_steps)))

matplot(t,paths1[1,],type='l',col="red")
lines(t,paths2[1,],type='l',col="blue")
grid()


### Ćwiczenie 5
### Wygenerować 10,000 ścieżek dwóch skorelowanych procesów Wienera na przedziale [0,2] 
### z 252 krokami symulacji w roku i korelacją 0.9.
### Narysować pierwsze ścieżki obydwu procesów na jednym rysunku.

n_paths = 10000
T = 2
n_steps_per_year = 252
n_steps = n_steps_per_year * T
dt = 1/n_steps_per_year
rho = 0.9

N = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)
N2 = matrix(rnorm(n_paths*n_steps,mean=0,sd=1),n_paths,n_steps)

paths_W1 = matrix(nrow=n_paths,ncol=n_steps+1,0)
paths_W2 = matrix(nrow=n_paths,ncol=n_steps+1,0)

for (i in 1:n_paths){
  for (j in 1:n_steps) {
    paths_W1[i,j+1] = paths_W1[i,j] +sqrt(dt)*N[i,j]
    paths_W2[i,j+1] = paths_W2[i,j] +sqrt(dt)*N2[i,j]
  }
}

paths_W2 = rho*paths_W1 + sqrt(1-rho^2)*paths_W2
t = c(0,cumsum(rep(dt,n_steps)))
matplot(t, paths_W1[1,],type='l', col="red")
lines(t, paths_W2[1,],type='l', col="green")
grid()


### Ćwiczenie 6
### Wygenerować 100 ścieżek trzech skorelowanych procesów Wienera na przedziale [0,1] 
### z 252 krokami symulacji w roku i korelacjami rho(W1,W2=0.9, rho(W1,W3)=-0.7, rho(W2,W3)=-0.7.
### Narysować pierwsze ścieżki trzech procesów na jednym rysunku.

library(MASS)

n_paths = 100
T = 1
n_steps_per_year = 252
n_steps = n_steps_per_year * T
dt = 1/n_steps_per_year
rho12 = 0.9
rho13 = -0.7
rho23 = -0.7

corr_matrix = matrix(c(1,rho12,rho13,rho12,1,rho23,rho13,rho23,1),3,3)
mN = t(mvrnorm(n_paths*n_steps,rep(0,3),corr_matrix))   #generowanie próbek z wielowymiarowego rozkładu normalnego

dt = 1/n_steps

paths_W1 = matrix(nrow=n_paths,ncol=n_steps+1,0)
paths_W2 = matrix(nrow=n_paths,ncol=n_steps+1,0)
paths_W3 = matrix(nrow=n_paths,ncol=n_steps+1,0)

for (i in 1:n_paths) {
  for (j in 1:n_steps) {
    paths_W1[i,j+1] = paths_W1[i,j]+sqrt(dt)*mN[1,(i-1)*n_steps+j]
    paths_W2[i,j+1] = paths_W2[i,j]+sqrt(dt)*mN[2,(i-1)*n_steps+j]
    paths_W3[i,j+1] = paths_W3[i,j]+sqrt(dt)*mN[3,(i-1)*n_steps+j]
  }
}

t = c(0,cumsum(rep(dt,n_steps)))

matplot(t, paths_W1[1,],type='l', col="red")
lines(t, paths_W2[1,],type='l', col="green")
lines(t, paths_W3[1,],type='l', col="blue")
grid()


### Ćwiczenie 7 (dodatkowe - do domu)
### Dowolną metodą wygenerować 10,000 ścieżek cen akcji w modelu Hestona do 1 roku z 252 krokami symulacji:
### dXt = (r*St)dt + (sqrt(vt)*St)dWt_1 (proces cen akcji - geometryczny ruch Browna ze stochastyczną wariancją)
### dvt = kappa*(theta-vt)dt + (sigma*sqrt(vt))dWt_2 (proces stochastycnej wariancji - CIR)
### Wt_1 i Wt_2 są dwoma procesami Wienera skorelowanymi z korelacją -0.75
### Przyjąc r=0.05, kappa=1.2, theta = 0.25, sigma = 0.5
