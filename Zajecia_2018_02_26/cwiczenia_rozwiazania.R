# cwiczenie 2 - zbior danych mtcars#
head(mtcars)
#[, 1]   mpg     Miles/(US) gallon
#[, 2]   cyl     Number of cylinders
#[, 3]   disp    Displacement (cu.in.)
#[, 4]   hp      Gross horsepower
#[, 5]   drat    Rear axle ratio
#[, 6]   wt      Weight (lb/1000)
#[, 7]   qsec    1/4 mile time
#[, 8]   vs      V/S
#[, 9]   am      Transmission (0 = automatic, 1 = manual)
#[,10]   gear    Number of forward gears
#[,11]   carb    Number of carburetors

# odpowiedz na pytania:
# ile samochodow z probki ma conajmniej 190 koni mechanicznych (zmienna hp)
A <- mtcars[mtcars$hp >= 190,]
dim(A)

# stworz histogram zmiennej mpg (licza mil jakie samochod jest w stanie przejachac na galonie paliwa)
hist(mtcars$mpg)

# zbuduj model regresji liniowej pomiedzy zmienna mpg a zmiennymi wt, qsec, am
# funkcja lm(zmienna_objasniana ~ zmienna_objasniajaca_1 + zmienna_objasniajaca_2, data = ramka_danych)
model <- lm(mpg ~ wt+qsec+am, data = mtcars)

# wykonaj funkcje summary na dopasowanym modelu
summary(model)

# wykonaj funkcje plot na dopasowanym modelu
plot(model)

model$coefficients


# cwiczenie 3 - dplyr #
# stworz nowa zmienna, ktora bedzie zawierac informacje o tym ile miesiecy zajmie splata pozyczki 
# przyjmujac 30% wartosci zmiennej income jest splacane co miesiac
klienci %>% head
klienci %>% mutate(months_to_pay = ......)

# policz default rate dla osob zarabiajacych wiecej niz 5000 (zmienna income)
# podpowiedz: funckja ifelse() wewnatrz funkcji mutate pozwala zdefiniowac zmienna w zaleznosci o spelnienia warunku
# dt %>% mutate(new_variable = ifelse(condition == TRUE, result_when_TRUE, result_when_FALSE))
# podpowiedz: funckja n() podaje liczbe obserwacji w danej grupie
# dt %>% group_by(type) %>% mutate(count_observations_for_each_type = n())
klienci %>% mutate(income_above_5000 = ifelse(income > 5000,1,0)) %>% 
  group_by(income_above_5000) %>% summarize(DR = sum(default)/n()


# cwiczenie 4 - petle #
# napisz petle, ktora bedzie :
# a) dopasowywac model jednowymiarowy regresji liniowej pomiedzy zmienna mpg i kazda ze pozostalych zmiennych
# b) przekazywac do konsoli wynik funkcji summary dla kazdego z modeli

for(i in 2:dim(mtcars)[2]){
  two_var <- mtcars[,c(1,i)]
  model <- lm(two_var)
  print(summary(model))
}
