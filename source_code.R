#AHMAD SOPIAN
#G651190561

library(tidymodels)
library(ranger)
library(scales)
library(plotly)
library(tidyverse)
library(lubridate)
library(nsga2R)
options(scipen = 999)

#import data 
df_price <-  read.csv('~/R/NSGA/prices.csv')
head(df_price)
df_price_copy <- df_price %>%
  mutate(date = ymd(date)) %>%
  filter(#year(date) == 2016,
         month(date) %in% c(1:3))

df_securities <- read.csv("~/R/NSGA/securities.csv")
df_securities <- df_securities %>%
  select(`Ticker.symbol`, Security) %>%
  rename(symbol = `Ticker.symbol`, 
         name = Security)

df_securities$symbol <- as.character(df_securities$symbol)
df_price_copy <- df_price_copy %>%
  left_join(df_securities, by = c("symbol")) %>%
  select(date, symbol, name, everything())

head(df_price_copy)

#---------
set.seed(123)
# menghitung pengembalian
df_price_copy <- df_price_copy %>%
  rename(price = close) %>%
  select(date, symbol, name, price) %>%
  group_by(symbol, name) %>%
  mutate(price_prev = lag(price),
         return = (price - price_prev)/price_prev) %>%
  slice(-1) %>%
  ungroup()
head(df_price_copy)

#----menghitung rata-rata setiap stok----
mean_stok <- df_price_copy %>%
  group_by(symbol) %>%
  summarise(mean = mean(return)) %>%
  arrange(desc(mean))
max(df_price_copy$date)
head(mean_stok)
# nilai rf diperoleh berdasarkan suku bunga terbaru/
#disini menggunakan suku bunga terakhir 2016-03-31
rf <- 0.23/100

#Matriks covariance 
df_price_copy_1 <- df_price_copy %>%
  pivot_wider(id_cols = c(date, symbol), 
              names_from = symbol, values_from = return) %>%
  select(-date)

for (i in 1:n_distinct(df_price_copy$symbol)){
  df_price_copy_1[,i] <- df_price_copy_1[,i] - as.numeric(mean_stok[i,2])
}

head(df_price_copy_1)

#buat kovarian matriksnya
matrik_cov <- cov(x = df_price_copy_1)

#-------menetapkan fungsi fitnes
fitnes <- function(x){
  bobot_stok <- x
  
  #menghitung total return
  f1 <- numeric()
  for(i in 1:n_distinct(df_price_copy$symbol)){
    f1[i] <- bobot_stok[i]*mean_stok$mean[i]
  }
  mean_return <- sum(f1) - 1e9 * (round(sum(bobot_stok),10)-1)^2
  
  
  #menghitung total risk
  f2 <- numeric()
  for (i in 1:n_distinct(df_price_copy$symbol)){
    f <- numeric()
    
  for (j in 1:n_distinct(df_price_copy$symbol)){
    f[j] <- bobot_stok[i]*bobot_stok[j]*matrik_cov[i,j]
  } 
  f2[i] <- sum(f)  
  }
  risk <- sum(f2) * 1e9 *(round(sum(bobot_stok),10)-1)^2
  
  #menghitung jumlah dari assets
  card <- length(bobot_stok[bobot_stok > 0])
  
  return(c(-mean_return, risk, card))
}

#----menjalankan algoritma
set.seed(123)
n_asset <- n_distinct(df_price_copy$symbol)
finance_optim <- nsga2R(fn = fitnes, varNo = n_asset, objDim = 3, generations = 100,
                        mprob = 0.3, popSize = 200, cprob = 0.8,
                        lowerBounds = rep(0, n_asset),upperBounds = rep(1, n_asset))

finance_optim$objectives[finance_optim$paretoFrontRank == 1, ] %>% 
  matrix(ncol = 3) %>% 
  as.data.frame() %>% 
  distinct() %>% 
  mutate(V1 = round(-V1, 5),
         V2 = round(V2, 5)) %>% 
  rename(`Total Return` = V1,
         Risk = V2,
         `Number of Assets` = V3)

sum(finance_optim$parameters[1, ])

data.frame(symbol = unique(df_price_copy$symbol),
           name = unique(df_price_copy$name),
           weight = finance_optim$parameters[1, ]) %>% 
  mutate(return = round(weight * mean_stock$mean, 5),
         weight = round(weight,5))