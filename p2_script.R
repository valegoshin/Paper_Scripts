# Wed Apr 24 11:56:02 2019 ------------------------------
# Script for paper Logistic Regression

# used packages

library(tidyverse)
library(broom)
library(pROC)
library(caret)

# Data 

raw_data <- read_csv("risk_factors_cervical_cancer.csv")
raw_data <- janitor::clean_names(raw_data)

cervical <- raw_data %>%
  mutate_if(is.character, list(~ as.numeric(na_if(., "?")))) %>%
  select(c(1:6, 8:13, 36)) %>%
  mutate(
    smokes = factor(smokes, levels = c(0, 1)),
    hormonal_contraceptives = factor(hormonal_contraceptives, levels = c(0, 1)),
    iud = factor(iud, levels = c(0, 1)),
    st_ds = factor(st_ds, levels = c(0, 1)),
    biopsy = factor(biopsy, levels = c(0, 1))
  ) %>%
  na.omit()

glimpse(cervical)

cervical %>%
  select_if(is.numeric) %>%
  skimr::skim_to_wide() %>%
  select(-c(1, 3:5)) %>%
  knitr::kable(caption = "Таб.4 Непрерывные переменные")

cervical %>%
  select_if(is.factor) %>%
  skimr::skim_to_wide() %>%
  select(-c(1, 3:5)) %>%
  knitr::kable(caption = "Таб.5 Категориальные переменные")

# исходная модель
fit <- glm(biopsy ~ ., data = cervical, family = "binomial")

fit %>%
  glance() %>%
  knitr::kable(caption = "Таб.6 Параметры исходной модели")

fit %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  knitr::kable(caption = "Таб.7 Коэффициенты исходной модели", digits = 4)


fit %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, conf.low, min)) %>%
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = .5) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  expand_limits(y = 0) +
  coord_flip()

# подбор более подходящей модели
MASS::stepAIC(fit, direction = "both")

# измененная модель 
fit1 <- glm(
  formula = biopsy ~ hormonal_contraceptives_years + st_ds,
  family = "binomial", data = cervical
)

fit1 %>%
  glance() %>%
  knitr::kable(caption = "Таб.5 Параметры улучшенной модели")

fit1 %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  knitr::kable(caption = "Таб.6 Коэффициенты улучшенной модели", digits = 4)

# Классификационный анализ с использованием новой модели, используется пакет pROC

# dataframe с предсказанными значениями вероятности признака
fit_1 <- augment(fit1, type.predict = "response")

# roc объект
roc_data <- roc(biopsy ~ .fitted, data = fit_1)

# ROC кривая
yardstick::roc_curve(fit_1, biopsy, .fitted) %>% autoplot()

# roc auc
auc(roc_data)

# threshold
(thr <- coords(roc_data, "best"))

# dataframe с бинарной переменной предсказанных значений
fit_1_matrix <- fit_1 %>%
  mutate(preds = as.factor(ifelse(.fitted <= thr[1], 0, 1))) %>%
  select(preds, biopsy)

conf_matr <- with(fit_1_matrix, table(preds, biopsy)) %>%
  DescTools::Rev()

conf_matr %>% knitr::kable()

# Показатели оценки модели
confusionMatrix(conf_matr)$overall[[1]]
precision(conf_matr)
recall(conf_matr)
F_meas(conf_matr)

# Валидация модели

set.seed(123)
ind <- createDataPartition(cervical$biopsy, p = .7, list = FALSE)
train_data <- cervical[ind, ]
test_data <- cervical[-ind, ]

# модель на тренировочных данных
fit_train <- glm(
  formula = biopsy ~ hormonal_contraceptives_years + st_ds,
  family = "binomial", data = train_data
)

# наборы данных с предсказанными значениями вероятности признака
fit_train_data <- augment(fit_train, type.predict = "response")
fit_test_data <- augment(fit_train, newdata = test_data, type.predict = "response")

# roc объекты для тренировочных и тестовых данных
roc_train_data <- roc(biopsy ~ .fitted, data = fit_train_data)
roc_test_data <- roc(biopsy ~ .fitted, data = fit_test_data)

# DeLong's test for two ROC curves
roc.test(roc_train_data, roc_test_data)

# One more thing. Применение функций пакета finalfit

library(finalfit)
explanatory <- c("st_ds", "hormonal_contraceptives_years",  "smokes", "age", "number_of_sexual_partners", "first_sexual_intercourse", "num_of_pregnancies", "iud")
dependent <- "biopsy"

cervical %>%
  finalfit(dependent, explanatory) %>%
  knitr::kable()
