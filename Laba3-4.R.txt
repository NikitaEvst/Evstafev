data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", sep=",")

#  Name / Data Type / Measurement Unit / Description
#  -----------------------------
#  Sex / nominal / -- / M, F, and I (infant)
#  Length / continuous / mm / Longest shell measurement
#  Diameter / continuous / mm / perpendicular to length
#  Height / continuous / mm / with meat in shell
#  Whole weight / continuous / grams / whole abalone
#  Shucked weight / continuous / grams / weight of meat
#  Viscera weight / continuous / grams / gut weight (after bleeding)
#  Shell weight / continuous / grams / after being dried
#  Rings / integer / -- / +1.5 gives the age in years

colnames(data) <- c("sex", "length", "diameter", "height",
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")

data$sex <- as.factor(data$sex)

summary(data)


# Ищем выбросы 
par(mfrow=c(1,4))

boxplot((data$length), main = "Длина", ylab = "Длина")
boxplot((data$diameter), main = "Диаметр, мм", ylab = "Диаметр")
boxplot((data$height), main = "Высота", ylab = "Высота")
boxplot((data$whole_weight), main = "Вес", ylab = "Полный вес")


par(mfrow=c(1,3))

hist(data$diameter, main = "Диаметр, мм", ylab = "Index", xlab = "Диаметр")
hist(data$height, main = "Высота, мм", ylab = "Index", xlab = "Высота")
hist(data$whole_weight, main = "Полный вес, гр", ylab = "Index", xlab = "Вес")

#data.noout - данные без выбросов

data.noout <- data[data$length > 0.22 &
                   data$diameter > 0.15 &
                   data$height > 0.05 & data$height < 0.22 &
                   data$whole_weight < 2.1,]

summary(data.noout)


par(mfrow=c(1,4))

boxplot((data.noout$length), main = "Длина", ylab = "Длина")
boxplot((data.noout$diameter), main = "Диаметр, мм", ylab = "Диаметр")
boxplot((data.noout$height), main = "Высота", ylab = "Высота")
boxplot((data.noout$whole_weight), main = "Вес", ylab = "Полный вес")


plot (sort(data.noout$length), main = "Длина", ylab = "Длина")
plot (sort(data.noout$diameter), main = "Диаметр, мм", ylab = "Диаметр")
plot (sort(data.noout$height), main = "Высота", ylab = "Высота")
plot (sort(data.noout$whole_weight), main = "Вес", ylab = "Полный вес")


#Визуализируем возможные зависимости
par(mfrow=c(1,3))

plot(data.noout$length, data.noout$whole_weight,'p',
     main = "Зависимость веса от длины", 
     ylab = "Полный вес", xlab = "Длина")

plot(data.noout$diameter, data.noout$whole_weight,'p',
     main = "Зависимость веса от диаметра", 
     ylab = "Полный вес", xlab = "Диаметр")

plot(data.noout$height, data.noout$whole_weight,'p',
     main = "Зависимость веса от высоты", 
     ylab = "Полный вес", xlab = "Высота")


# Зависимости веса от длины и диаметра наиболее заметные - их и будем тестировать 
t.test(data.noout$whole_weight, data.noout$length)

t.test(data.noout$whole_weight, data.noout$diameter)


# Строим линейные зависимости
linear.model.1 <- lm (whole_weight ~ length, data=data.noout)
linear.model.1
summary(linear.model.1)

par(mfrow=c(1,1))

plot(data.noout$whole_weight, data.noout$length)
abline(linear.model.1)

# Делим массив данных на 2 случайные части
odds <- seq(1, nrow(data.noout), by=2)
data.in <- data.noout[odds,]
data.out <- data.noout[-odds,]

# Создаем модель для прогноза
linear.model.half <- lm (data.in$whole_weight ~ data.in$length, data=data.in)
summary (linear.model.half)

# Прогноз 
data.predict <- predict (linear.model.half)
cor (data.in$whole_weight, data.predict)
plot (data.in$whole_weight, data.predict)

data.predict.out <- predict (linear.model.half, data.out)
cor (data.out$whole_weight, data.predict.out)
plot (data.out$whole_weight, data.predict.out)

# Предикты я сделал, а что делать дальше и как их сравнивать я не знаю. Если узнаю обновлю файлик.
# Но если вы это читаете, то я не узнал.

