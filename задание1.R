library(MASS)
data(Boston) 
model1 <- lm(crim ~ indus + age + indus:chas + age:chas,
              data = Boston)
summary(model1)
model2 <- lm(crim ~ indus + age + indus:chas,
             data = Boston)
summary(model2)

#общее число наблюдений
m <- nrow(Boston)

# доля обучающей выборки
train.percent <- 0.5 

# выбрать наблюдения в обучающую выборку
inTrain <- sample(m, m * train.percent)
inTrain

# Линейная модель(a) ##############################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Boston)
# подгонка линейной модели на обучающей выборке
fit.lm.1 <- lm(crim ~ indus + age + indus:chas, 
               subset = inTrain)

# считаем MSE на тестовой выборке
mean.a.1 <- mean((crim[-inTrain] - predict(fit.lm.1,
                              Boston[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Boston)

# Квадратичная модель ##########################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Boston)
# подгонка линейной модели на обучающей выборке
fit.lm.2 <- lm(crim ~ poly(age,2) + poly(indus, 2) + indus:chas, 
               subset = inTrain)
summary(fit.lm.2)
fit.lm.2.1 <- lm(crim ~ poly(age,2) + poly(indus, 2), 
               subset = inTrain)
summary(fit.lm.2.1)
# считаем MSE на тестовой выборке
mean.a.2 <- mean((crim[-inTrain] - predict(fit.lm.2.1,
                              Boston[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Boston)

# Кубическая модель ############################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Boston)
# подгонка линейной модели на обучающей выборке
fit.lm.3 <- lm(crim ~ poly(age,3) + poly(indus, 3) + indus:chas, 
               subset = inTrain) 
summary(fit.lm.3)
# считаем MSE на тестовой выборке
mean.a.3 <- mean((crim[-inTrain] - predict(fit.lm.3,
                              Boston[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Boston)


# k-кратная перекрёстная проверка ==============================================

# оценим точность полиномиальных моделей, меняя степень
# вектор с ошибками по 10-кратной кросс-валидации
cv.err.k.fold <- rep(0, 5)
names(cv.err.k.fold) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm <- glm(crim ~ poly(age,3) + poly(indus, 3) + indus:chas, data = Boston)
  cv.err.k.fold[i] <- cv.glm(Boston, fit.glm,
                             K = 10)$delta[1]
}
# результат
cv.err.k.fold


# Линейная модель(b) ##############################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Boston)
# подгонка линейной модели на обучающей выборке
fit.lm.1b <- lm(crim ~ indus + age, 
               subset = inTrain)
summary(fit.lm.1b)
# считаем MSE на тестовой выборке
mean.b.1 <- mean((crim[-inTrain] - predict(fit.lm.1b,
                                           Boston[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Boston)

# Квадратичная модель ##########################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Boston)
# подгонка линейной модели на обучающей выборке
fit.lm.2b <- lm(crim ~ poly(age,2) + poly(indus, 2), 
               subset = inTrain)
summary(fit.lm.2b)

# считаем MSE на тестовой выборке
mean.b.2 <- mean((crim[-inTrain] - predict(fit.lm.2b,
                                           Boston[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Boston)

# Кубическая модель ############################################################

# присоединить таблицу с данными: названия стоблцов будут доступны напрямую
attach(Boston)
# подгонка линейной модели на обучающей выборке
fit.lm.3b <- lm(crim ~ poly(age,3) + poly(indus, 3), 
               subset = inTrain) 
summary(fit.lm.3b)
# считаем MSE на тестовой выборке
mean.b.3 <- mean((crim[-inTrain] - predict(fit.lm.3b,
                                           Boston[-inTrain, ]))^2)

# отсоединить таблицу с данными
detach(Boston)


# k-кратная перекрёстная проверка ==============================================

# оценим точность полиномиальных моделей, меняя степень
# вектор с ошибками по 10-кратной кросс-валидации
cv.err.k.fold.b <- rep(0, 5)
names(cv.err.k.fold.b) <- 1:5
# цикл по степеням полиномов
for (i in 1:5){
  fit.glm.b <- glm(crim ~ poly(age,3) + poly(indus, 3), data = Boston)
  cv.err.k.fold.b[i] <- cv.glm(Boston, fit.glm,
                             K = 10)$delta[1]
}
# результат
cv.err.k.fold.b
cv.err.k.fold

attach(Boston)
boot.fn <- function(data, index){
  coef(lm(crim ~ poly(age,3) + poly(indus, 3) + indus:chas, subset = index))
}
boot.fn(Boston, 1:100)
# пример применения функции к бутстреп-выборке
boot.fn(Boston, sample(m, m, replace = T))
# применяем функцию boot для вычисления стандартных ошибок параметров
#  (1000 выборок с повторами)
boot(Boston, boot.fn, 1000)


# сравним с МНК
attach(Boston)
summary(lm(crim ~ indus + age + indus:chas))$coef

detach(Auto)

# оценки отличаются из-за того, что МНК -- параметрический метод с допущениями

# вычислим оценки параметров квадратичной модели регрессии
boot.fn.2 <- function(data, index){
  coef(lm(crim ~ poly(age,3) + poly(indus, 3) + indus:chas, data = Boston, subset = index))
}
# применим функцию к 1000 бутсреп-выборкам
boot(Auto, boot.fn, 1000)