# r-notes
## for loop

for loop wygląda trochę inaczej bo używamy wektora

```r
for(i in 1:5){
  print("Hello R")
}
```

## Prawo wielkich liczb

Przy dostatecznie wielkiej liczbie prób częstość danego zdarzenia losowego będzie się dowolnie mało różniła od jego prawdopodobieństwa

sprawdzanie Normal distribiution

```r
mean <- 1000

count <- 0
for(i in rnorm(mean)){
  number <- rnorm(1)
  if(number > -1 && number < 1)
    count <- count + 1
}

count/mean
```

## Wektor

odpowiednik array z elementami tego samego typu. Wektor to sekwencja elementów danych tego samego typu.

Index zaczyna się na 1

```r
Name = c("Amiya", "Raj", "Asish")

Language = c("R", "Python", "Java")

Age = c(22, 25, 45)
```

`sequence`

```r
seq(1,30,2) # od 1 do 30 step co 2
```

`rep`

```r
rep("Hi!", 5) # repeat Hi 5 times
```

accessing elements

```r
Name[2] # Raj
```

## Operacje na wektorach

```r
v1 <- c(1,2,3,4,5)
v2 <- c(10,20,30,40,50)

v1+v2 # 11 22 33 44 55
v1/v2 # 0.1 0.1 0.1 0.1 0.1
v1*v2 # 10 40 90 160 250
```

```r
x <- rnorm(5)

for(i in x){
  # i staje sie wartoscią w x
  # i nie jest 0 1 2 3 4 tylko 
  #  przyjmuje wartości wektora
  print(i)
}
```

```r
revenue <- c(14574.49, 7606.46, 8611.41, 9175.41, 8058.65, 8105.44, 11496.28, 9766.09, 10305.32, 14379.96, 10713.97, 15433.50)
expenses <- c(12051.82, 5695.07, 12319.20, 12089.72, 8658.57, 840.20, 3285.73, 5821.12, 6976.93, 16618.61, 10054.37, 3803.96)

profit <- revenue - expenses
profit

tax <- round(0.30 * profit, 2)
tax

profitAfterTax <- profit - tax
profitAfterTax

profitMargin <- round(profitAfterTax/revenue, 2) * 100
profitMargin

goodMonths <-  profitAfterTax > mean(profitAfterTax)
goodMonths

badMonths <- profitAfterTax < mean(profitAfterTax)
badMonths

bestMonth <- max(profitAfterTax)
bestMonth

worstMonth <- min(profitAfterTax)
worstMonth
```
