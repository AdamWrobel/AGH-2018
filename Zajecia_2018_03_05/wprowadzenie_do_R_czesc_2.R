

### pakiet dplyr - przypomnienie ###
#install.packages('dplyr')
library(dplyr)

# operator %>%
# input %>% funkcja jest rownoznaczny z funkcja(input)

# funkcja select - wybieranie kolumn
iris %>% select(Sepal.Width) %>% head

# funkcja filter - filtorwanie danych spelniajacych kryteria
iris %>% filter(Sepal.Width > 3) %>% head

# funkcja mutate - tworzenie nowej zmiennej
iris_surf <- iris %>% mutate(Petal.Surface = 1/2 *Petal.Length * Petal.Width)
iris_surf %>% head

# funkcja group_by i summarize - tworzenie agregatow
iris_surf %>% group_by(Species) %>% summarize(mean_Petal.Surface = mean(Petal.Surface))

# funkcja arrange - sortowanie
iris_surf %>% head
iris_arranged <- iris_surf %>% arrange(Petal.Surface)
iris_arranged %>% head


# cwiczenie 1

# zdefiniuj katalog roboczy podajac sciezke pod ktora znajduja sie materialy 

# wczytaj plik Polskie_tablice_trwania_zycia_1990_2016.csv (https://stat.gov.pl/obszary-tematyczne/ludnosc/trwanie-zycia/trwanie-zycia-tablice,1,1.html')


# zmienne:
# sex - plec (1 - mezczyzna, 2 - kobieta)
# age - wiek
# qx - prawdopodobienstwo smierci w ciagu roku
# ex - dalsza oczekiwana dlugosc zycia (life expectancy)

# wybierz kolumny sex, age, qx, ex, year i przypisz do obiektu PTTZ_small


# sprawdz oczekiwana dalsza dlugosc zycia (life expectancy) dla kobiet i mezczyzn w wieku 25 lat (uzywajac danych z roku 2016)




### wizualizacja ###

# ggplot2
# install.packages('ggplot2')
library(ggplot2)

PTTZ_60_65_M <- PTTZ_small %>% filter(sex == 1, age %in% c(55:60))

ggplot(PTTZ_60_65_M, aes(x = year, y = qx, group = age, colour = age)) + geom_line()

# zmienna age jako zmienna kategoryczna
str(PTTZ_60_65_M)
PTTZ_60_65_M <- PTTZ_60_65_M %>% mutate(age = as.character(age))

ggplot(PTTZ_60_65_M, aes(x = year, y = qx, colour = age)) + geom_line()


# cwiczenie 2
# porownaj oczekiwana dlugosc kobiet i mezczyzn na przestrzeni lat 1990-2016 
# tworzac wykres dla osob w wieku 50,55,60 lat; uzyj PTTZ_for_plot i funkcji ggplot
# oraz parametrow colour i lty (typ lini)
PTTZ_for_plot <- PTTZ_small %>% filter(age %in% c(50,55,60)) %>% 
  mutate(age = as.character(age), sex = as.character(sex))





### symulacje i wizualizacja ###

runif # losowanie z rozkladu jednostajnego
rnorm # losowanie z rozkladu normalnego

# losowanie z standardowego rozkladu normalnego
N1 <- rnorm(n = 260, mean = 0, sd = 1)

# wizualizacja zmiennej losowej
plot(N1)
hist(N1)
N1 %>% density %>% plot

# symulacja brownian motion

N2 <- rnorm(n = 260)
path <- cumsum(N2)

plot(path, type = 'l')
grid()

# 10 paths
N3 <- rnorm(n = 260*10, sd = 3)
tabela <- data.frame(N3, scenario = rep(1:10, each= 260), step = seq(1,260))
tabela <- tabela %>% group_by(scenario) %>% mutate(path = cumsum(N3))


# cwiczenie 3
# zrob wykres realizacji sciezek, gdzie kazda ze sciezek bedzie zaznaczona innym kolerem
# uzyj tabela i funkcji ggplot



### definiowanie funkcji ###

mean_excluding_zeros <- function(input_vector){
  temp <- input_vector[input_vector!= 0]
  output <- mean(temp)
  return(output)
}
wektor <- c(0,2,3,0,4,0)
mean(wektor)
mean_excluding_zeros(wektor)

# cwiczenie 4
# zdefiniuj funkcje o postaci y = x + x^2, gdzie x jest inputem, a y outputem



## petle w symulacjach ##

number_of_steps <- 10

N <- rnorm(n = number_of_steps+1, sd = 0.5)
path <- vector(mode="numeric", length = number_of_steps)

# jedna sciezka - 10 krokow
for(i in 1:(number_of_steps+1)){
  path[i+1] <- path[i] + N[i]
}


# piec sciezek - kazda po 260 krokow
number_of_simulations <- 5
number_of_steps <- 10

path <- matrix(nrow = number_of_steps + 1, ncol = number_of_simulations, NA)
one_path <- vector(mode="numeric", length = number_of_steps)

# petla dla symulacji
for(simulation in 1:number_of_simulations){

  # petla dla krokow
  N <- rnorm(n = number_of_steps+1, sd = 0.5)
  for(i in 1:(number_of_steps)){
    one_path[i + 1] <- one_path[i] + N[i]
  }
  path[,simulation] <- one_path
}

# ploting paths in matricies
matplot(path, type = 'l')
grid()

# cwiczenie 5
# zapisz logike symulacji sciezek jako funkcje i wykonaj dla 260 krokow i 10 symulacji
# jako krok drugi dodatkowo zdefinuje srednia i odychylenie standardowe jako parametry funkcji
# jako krok trzeci wykonaj funkcje z srednia o wartosci 1 i odychleniu 3

