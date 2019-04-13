#chisq.test

#zmienne
{
  o <- 6                #oczka
  x <- 100000           #liczba powtorzen
  i <- 0                #licznik do petli
  wynik_hiv_5   <- c(0)
  wynik_hiv_10  <- c(0)
  wynik_hiv_20  <- c(0)
  wynik_hiv_30  <- c(0)
  wynik_hiv_40  <- c(0)
  wynik_hiv_50  <- c(0)
  wynik_hiv_60  <- c(0)
  wynik_hiv_70  <- c(0)
  wynik_hiv_80  <- c(0)
  wynik_hiv_90  <- c(0)
  wynik_hiv_100 <- c(0)
  prawd <- rep(1/6, 6)  #prawdopodobienstwa oczek
  #prawd <- c(1/6, 1/6, 1/3, 1/8, 1/9, 7/72)
}
#do losowania wartosci
losowanko <- function(liczba_losowan, liczba_kostek, dice)
{
  i <- 0
  wylosowane <- rep(0,6)
  while (i < liczba_losowan){
    #losujemy jeden raz i sprawdzamy co wylosowaismy i zwiekszamy licznik dla tej wartosci
    wynik <- sample(dice, replace = TRUE, size = liczba_kostek)
    
    if (wynik == 1){
      wylosowane[1] <- wylosowane[1] + 1
    }
    if (wynik == 2){
      wylosowane[2] <- wylosowane[2] + 1
    }
    if (wynik == 3){
      wylosowane[3] <- wylosowane[3] + 1
    }
    if (wynik == 4){
      wylosowane[4] <- wylosowane[4] + 1
    }
    if (wynik == 5){
      wylosowane[5] <- wylosowane[5] + 1
    }
    if (wynik == 6){
      wylosowane[6] <- wylosowane[6] + 1
    }
    
    i <- i + 1
  }
  return(wylosowane)
}

#chisq.test
test_na_hiv <- function(to_co_wylosowalo, prawdopodobienstwo)
{
  return(chisq.test(to_co_wylosowalo, p = prawdopodobienstwo)$p.value)
}

#bierze wyniki z chisq.test (test_na_hiv) i sprawdza czy p.value jest wieksze od 0.005
do_wykresu <- function(l_5, L_10, L_20, L_30, L_40, L_50, L_60, L_70, L_80, L_90, L_100, liczba_powtorzen)
{
  temp <- rep(0,11)           #wektor z 'gotowymi' p.values
  for (i in l_5){             #bierzemy te p.values wieksze od 0.005 i zwiekszamy dla nich licznik
    if (i > 0.05){
      temp[1] <- temp[1] + 1
    }
  }
  
  for (i in L_10){
    if (i > 0.05){
      temp[2] <- temp[2] + 1
    }
  }
  for (i in L_20){
    if (i > 0.05){
      temp[3] <- temp[3] + 1
    }
  }
  for (i in L_30){
    if (i > 0.05){
      temp[4] <- temp[4] + 1
    }
  }
  for (i in L_40){
    if (i > 0.05){
      temp[5] <- temp[5] + 1
    }
  }
  for (i in L_50){
    if (i > 0.05){
      temp[6] <- temp[6] + 1
    }
  }
  for (i in L_60){
    if (i > 0.05){
      temp[7] <- temp[7] + 1
    }
  }
  for (i in L_70){
    if (i > 0.05){
      temp[8] <- temp[8] + 1
    }
  }
  for (i in L_80){
    if (i > 0.05){
      temp[9] <- temp[9] + 1
    }
  }
  for (i in L_90){
    if (i > 0.05){
      temp[10] <- temp[10] + 1
    }
  }
  for (i in L_100){
    if (i > 0.05){
      temp[11] <- temp[11] + 1
    }
  }
  temp <- temp / liczba_powtorzen  #liczymy prawdopodobienstwo, ze p.value jest wieksze od 0.05
  
  return(temp)
}

#nasze rzuty
while (i < x)
{
  wynik_hiv_5   <- c(wynik_hiv_5,   test_na_hiv(losowanko(5,   1, seq(1, o)), p = prawd))
  wynik_hiv_10  <- c(wynik_hiv_10,  test_na_hiv(losowanko(10,  1, seq(1, o)), p = prawd))
  wynik_hiv_20  <- c(wynik_hiv_20,  test_na_hiv(losowanko(20,  1, seq(1, o)), p = prawd))
  wynik_hiv_30  <- c(wynik_hiv_30,  test_na_hiv(losowanko(30,  1, seq(1, o)), p = prawd))
  wynik_hiv_40  <- c(wynik_hiv_40,  test_na_hiv(losowanko(40,  1, seq(1, o)), p = prawd))
  wynik_hiv_50  <- c(wynik_hiv_50,  test_na_hiv(losowanko(50,  1, seq(1, o)), p = prawd))
  wynik_hiv_60  <- c(wynik_hiv_60,  test_na_hiv(losowanko(60,  1, seq(1, o)), p = prawd))
  wynik_hiv_70  <- c(wynik_hiv_70,  test_na_hiv(losowanko(70,  1, seq(1, o)), p = prawd))
  wynik_hiv_80  <- c(wynik_hiv_80,  test_na_hiv(losowanko(80,  1, seq(1, o)), p = prawd))
  wynik_hiv_90  <- c(wynik_hiv_90,  test_na_hiv(losowanko(90,  1, seq(1, o)), p = prawd))
  wynik_hiv_100 <- c(wynik_hiv_100, test_na_hiv(losowanko(100, 1, seq(1, o)), p = prawd))
  i <- i + 1 
}

wykresik <- do_wykresu(wynik_hiv_5, wynik_hiv_10, wynik_hiv_20, wynik_hiv_30, wynik_hiv_40, wynik_hiv_50,
                       wynik_hiv_60, wynik_hiv_70, wynik_hiv_80, wynik_hiv_90, wynik_hiv_100, x)

plot(c(5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), wykresik, col = 'red',
     xlab = 'liczba rzutow', ylab = 'wynik chisq.testu', pch = 3, type = 'b')