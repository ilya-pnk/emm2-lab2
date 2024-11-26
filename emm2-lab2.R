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



# Преобразование GARCH(1,0) в AR(1)
y = sigma^2 
x = c(0, y[-n]) # Лагированная последовательность

# Оценка параметров методом МНК
ar_model = lm(y ~ x) # y_t = a0 + a1 * y_(t-1) + e_t
summary(ar_model)

# Извлечение оценок параметров
a0_est = coef(ar_model)[1]
a1_est = coef(ar_model)[2]


## GARCH(3,0)
# Задание 3: Генерация GARCH(3,0)
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
  sigma[t] <- h[t] * epsilon[t]
}


# Разделение на обучающую и тестовую выборки
study_size = round(n * 0.9)
sigma_study = sigma[1:train_size]
sigma_test= sigma[(train_size+1):n]


# Оценка параметров GARCH(3,0)
garch30_model <- garch(sigma_study, order = c(3, 0))
summary(garch30_model)

# Прогноз на тестовой выборке
test_predict <- predict(garch30_model, newdata = sigma_test)
h_test <- sqrt(test_predict)
h_test;
# Построение графика
par(mfrow = c(1, 1))
plot(sigma_test, type = "l", main = "GARCH(3,0) - Тестовая выборка и прогноз", ylab = "Значение", xlab = "Итерация")
lines(h_test, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Тестовая выборка", "Прогноз волатильности"), col = c("black", "red"), lty = c(1, 2), lwd = c(1, 2))













