# Sun Aug 25 14:05:03 2019 ------------------------------
# Script for paper Ordinal Logistic Regression


# used packages -----

library(tidyverse)
library(broom)
library(MASS)
library(effects)


# data -----

# импорт данных
df <- foreign::read.spss("ordinal_regression.sav", to.data.frame = TRUE)
# структура данных
glimpse(df)

# определение пропущенных значений
df %>%
  inspectdf::inspect_na() %>%
  knitr::kable(caption = "Таб.1 Данные о пропущенных значениях")

# характеристика переменных, выводится самая частая категория
df %>%
  inspectdf::inspect_imb() %>%
  knitr::kable(caption = "Таб.2 Частые категории переменных")

# функция для представления всех категорий переменной
tab_apply <- function(variable) {
  janitor::tabyl(variable) %>%
    janitor::adorn_totals("row") %>%
    janitor::adorn_pct_formatting()
}
# вывод данных о категориях по всем переменным
apply(df, 2, tab_apply)


# модификация набора данных
df <- df %>%
  mutate(
    SRH = factor(SRH, levels = c("Fair or worse", "Good", "Very good"), ordered = TRUE),
    Ethnicity = relevel(Ethnicity, ref = "Kazakh"),
    MS = relevel(MS, ref = "Married"),
    Education = factor(Education, levels = c("Higher", "Basic", "Secondary", "Vocational")),
    Occupation = relevel(Occupation, ref = "Full time employed"),
    Smoking = factor(Smoking, levels = c("Non-smoker", "Daily smoker", "Occasional smoker"))
  )



# модель с 2 переменными -----

# создание модели
model <- polr(SRH ~ MS + Smoking, df, Hess = TRUE)

# вывод данных о модели
summary(model)

# данные о независимых переменных модели в первой записи набора данных
df[1, c("MS", "Smoking")]

# сумма произведений коэффициентов на значения x первой записи набора данных
(y1 <- 0.70481 * 0 + (-0.37191) * 1 + (-0.41768) *  0 +  (-0.09109) * 1)

# вероятность события SRH = Fair or worse
(p1 <- 1/(1 + exp(-(-0.6023 - y1))))

# вероятность события SRH = Fair or worse | Good
(p1or2 <- 1/(1 + exp(-(1.3657 - y1))))

# вероятность события SRH = Good
(p2 <- p1or2 - p1)

# вероятность события SRH = Very good
(p3 <- 1 - p1or2)

# сравните
round(c(p1, p2, p3),3)
predict(model, df[1,], type = "prob") %>% round(3)



# Упорядоченная логит-модель, включающая все переменные -----

# выделение тренировочного и тестового сетов
set.seed(123) # для воспроизведения
# определение номеров записей, входящих в тренировочный сет 
index <- sample(seq_len(nrow(df)), size = .7 * nrow(df))
# создание тренировочного сета
df_train <- df[index, ]
# создание тестового сета
df_test <- df[-index, ]


# создание модели
model <- polr(SRH ~ ., data = df_train, Hess = TRUE)

# параметры модели
glance(model) %>% knitr::kable(digits = 3)

# коэффициенты модели
tidy(model, conf.int = TRUE) %>% knitr::kable(digits = 3, caption = "Таб.3 Коэффициенты модели")

# значения коэффициентов модели 
tidy(model, conf.int = TRUE) %>% 
  mutate(ci_estimation = ifelse(conf.low < 0 & conf.high > 0, FALSE, TRUE)) %>%   
  filter(ci_estimation == TRUE) %>% 
  dplyr::select(-coefficient_type, -ci_estimation) %>% 
  knitr::kable(digits = 3, caption = "Таб.3 Статистически значимые коэффициенты")

# экспоненцированные значения коэффициентов
tidy(model, conf.int = TRUE, exponentiate = TRUE) %>% 
  mutate(ci_estimation = ifelse(conf.low < 1 & conf.high > 1, FALSE, TRUE)) %>%   
  filter(ci_estimation == TRUE) %>% 
  dplyr::select(-coefficient_type, -ci_estimation) %>% 
  knitr::kable(digits = 3, caption = "Таб.4 Статистически значимые коэффициенты")

# экспоненцированные значения intercept
tidy(model, conf.int = TRUE, exponentiate = TRUE) %>% 
  filter(coefficient_type == "zeta") %>% 
  dplyr::select(-coefficient_type) %>% 
  knitr::kable(digits = 3, caption = "Таб.5 Экспоненцированные значения intercept")

# оценка модели
car::Anova(model)

# модель с использованием всех предикторов кроме Gender
model1 <- polr(SRH ~ . - Gender, data = df_train, Hess = TRUE)
glance(model1)

# сравнение моделей
anova(model, model1)



# Оценка прогностических возможностей модели -----

# предсказанные значения для тренировочного сета
pred <- predict(model, df_train, type = "class")

# сравнение реальных и предсказанных значений (первые 6 позиций)
df_train %>% dplyr::select(SRH) %>% cbind(pred) %>% head()

# матрица несоответствий для тренировочного сета
(tab <- table(pred, df_train$SRH))
# определение доли ошибок для тренировочного сета
(m1 <- round(1 - sum(diag(tab)) / sum(tab), 3))

# оценка точности
(accur1 <- caret::confusionMatrix(tab)$overall[c(1, 3, 4)] %>% round(3))

# предсказанные значения для тестового сета
pred <- predict(model, df_test, type = "class")

# матрица несоответствий для тестового сета
(tab <- table(pred, df_test$SRH))
# определение доли ошибок для тестового сета
(m2 <- round(1 - sum(diag(tab)) / sum(tab), 3))

# оценка точности
(accur2 <- caret::confusionMatrix(tab)$overall[c(1, 3, 4)] %>% round(3))


# Графическое представление -----

# Графическая оценка эффекта фактора образования
plot(Effect(focal.predictors = "Education", model))

# Графическая оценка эффекта фактора семейного положения
plot(Effect(focal.predictors = "MS", model))

# Графическая оценка эффекта этнического фактора
plot(Effect(focal.predictors = "Ethnicity", model))

# Графическая оценка эффекта фактора курения
plot(Effect(focal.predictors = "Smoking", model))

# Графическая оценка эффекта фактора трудовой деятельности
plot(Effect(focal.predictors = "Occupation", model))

# Графическая оценка эффекта факторов семейного положения и курения
plot(Effect(focal.predictors = c("MS", "Gender"), model))




