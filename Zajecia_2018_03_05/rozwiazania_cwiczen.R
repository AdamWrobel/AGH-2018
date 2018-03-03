
# cwiczenie 1

# zdefiniuj katalog roboczy podajac sciezke pod ktora znajduja sie materialy 

# wczytaj plik Polskie_tablice_trwania_zycia_1990_2016.csv (https://stat.gov.pl/obszary-tematyczne/ludnosc/trwanie-zycia/trwanie-zycia-tablice,1,1.html')
PTTZ <- read.csv('Polskie_tablice_trwania_zycia_1990_2016.csv')

# zmienne:
# sex - plec (1 - mezczyzna, 2 - kobieta)
# age - wiek
# qx - prawdopodobienstwo smierci w ciagu roku
# ex - dalsza oczekiwana dlugosc zycia (life expectancy)

# wybierz kolumny sex, age, qx, ex, year i przypisz do obiektu PTTZ_small
PTTZ_small <- PTTZ %>% select(sex, age, qx, ex, year)

# sprawdz oczekiwana dalsza dlugosc zycia (life expectancy) dla kobiet i mezczyzn w wieku 25 lat (uzywajac danych z roku 2016)
PTTZ_small %>% filter(age == 25, year == 2016)


# cwiczenie 2
# porownaj oczekiwana dlugosc kobiet i mezczyzn na przestrzeni lat 1990-2016 
# tworzac wykres dla osob w wieku 50,55,60 lat; uzyj PTTZ_for_plot i funkcji ggplot
# oraz parametrow colour i lty (typ lini)
PTTZ_for_plot <- PTTZ_small %>% filter(age %in% c(50,55,60)) %>% 
  mutate(age = as.character(age), sex = as.character(sex))


ggplot(PTTZ_for_plot, aes(x = year, y = qx, lty = sex, colour = age)) + 
  geom_line()

ggplot(PTTZ_for_plot, aes(x = year, y = qx, colour = age)) + 
  geom_line() + facet_wrap(~sex)


# cwiczenie 3
# zrob wykres realizacji sciezek, gdzie kazda ze sciezek bedzie zaznaczona innym kolerem
# uzyj tabela i funkcji ggplot
ggplot(tabela, aes(x = step, y = path, group = scenario, colour = factor(scenario))) + geom_line()


# cwiczenie 4
# zdefiniuj funkcje o postaci y = x + x^2, gdzie x jest inputem, a y outputem
our_function <- function(x){
  x + x^2
}


# cwiczenie 5
# zapisz logike symulacji sciezek jako funkcje i wykonaj dla 260 krokow i 10 symulacji
# jako krok drugi dodatkowo zdefinuje srednia i odychylenie standardowe jako parametry funkcji
# jako krok trzeci wykonaj funkcje z srednia o wartosci 1 i odychleniu 3

simulate_paths <- function(number_of_simulations = 5, number_of_steps = 10, 
                           mean_input = 0, sd_input = 1){
  
  path <- matrix(nrow = number_of_steps + 1, ncol = number_of_simulations, NA)
  one_path <- vector(mode="numeric", length = number_of_steps)
  
  # petla dla symulacji
  for(simulation in 1:number_of_simulations){
    
    # petla dla krokow
    N <- rnorm(n = number_of_steps+1, mean = mean_input, sd = sd_input)
    for(i in 1:(number_of_steps)){
      one_path[i + 1] <- one_path[i] + N[i]
    }
    path[,simulation] <- one_path
  }
  
  # ploting paths in matricies
  matplot(path, type = 'l')
  grid()
}
simulate_paths(number_of_simulations = 10, number_of_steps = 260, mean_input = 1, sd_input = 3)
