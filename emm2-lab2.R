 install.packages("tseries")
library(tseries)

# 1. Построение графиков GARCH(1,0)
set.seed(123) 
n = 1000
a0 = 0.1 
a1 = 0.8 

# Генерация шума
e = rnorm(n) # Нормальный рапсредеоение

# Инициализация массивов
h = numeric(n)
sigma = numeric(n)

# Моделирование процесса
for (t in 2:n) {
  h[t] <- sqrt(a0 + a1 * h[t-1]^2) # Волатильность
  sigma[t] <- h[t] * e[t] # Наблюдения GARCH
}

# Построение графиков
par(mfrow = c(2, 1)) # это чтобы вывести два графика в одном окне
plot(sigma, type = "l", main = "График процесса GARCH(1,0)", ylab = "Значение", xlab = "Время")
plot(h, type = "l", main = "График волатильности GARCH(1,0)", ylab = "Волатильность", xlab = "Время")

# Задание 2: Оценка параметров для GARCH(1,0)
garch1 <- garch(sigma, order = c(1, 0))
summary(garch1)

# Задание 3: GARCH(3,0)
a0_3 = 0.1
a1_3 = 0.5
a2_3 = 0.3
a3_3 = 0.1
n=1100

epsilon = rnorm(n)

# Инициализация массивов
h = numeric(n)
sigma = numeric(n)

# Моделирование GARCH(3,0)
for (t in 4:n) {
  h[t] <- sqrt(a0_3 + a1_3 * h[t-1]^2 + a2_3 * h[t-2]^2 + a3_3 * h[t-3]^2)
  sigma_garch30[t] <- h[t] * epsilon[t]
}

