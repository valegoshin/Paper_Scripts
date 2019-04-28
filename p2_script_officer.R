# Wed Apr 24 14:16:43 2019 ------------------------------
# Script for paper Logistic Regression Word docx generation

#  used packages

library(tidyverse)
library(broom)
library(caret)
library(pROC)
library(flextable)
library(officer)

# data

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



###############
# числовые переменные в изучаемом наборе данных -----
t1 <- cervical %>%
  select_if(is.numeric) %>%
  skimr::skim_to_wide() %>%
  select(-c(1, 3:5))


colt1 <- names(t1)
t1 <- t1 %>%
  regulartable() %>%
  width(j = colt1, width = c(1, rep(.6, 7), 1.2)) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

t2 <- cervical %>%
  select_if(is.factor) %>%
  skimr::skim_to_wide() %>%
  select(-c(1, 3:5, 8))

colt2 <- names(t2)
t2 <- t2 %>%
  regulartable() %>%
  width(j = colt2, width = c(1.5, .7, 3)) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

# fit model

fit <- glm(biopsy ~ ., data = cervical, family = "binomial")

fit_text <- "fit <- glm(biopsy ~ ., data = cervical, family = 'binomial')"

# Оценка созданной модели fit -----
# broom::glance() result  -----

glance_fit <- fit %>%
  glance()

col_glance_fit <- names(glance_fit)

glance_fit <- glance_fit %>%
  regulartable() %>%
  width(j = col_glance_fit, width = c(.9, .7, .7, rep(.55, 2), rep(.7, 2))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

# broom::tidy() result -----
tidy_fit <- fit %>%
  tidy(conf.int = TRUE, , exponentiate = TRUE) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    one_in_ci = ifelse(conf.low < 1 & conf.high > 1, "yes", "no")
  )

col_tidy_fit <- names(tidy_fit)

tidy_fit <- tidy_fit %>%
  regulartable() %>%
  width(j = col_tidy_fit, width = c(1.8, rep(.7, 7))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

# plot w/ results of tidy -----
gg1 <- fit %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, conf.low, min)) %>%
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = .5) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  expand_limits(y = 0) +
  coord_flip() +
  theme_minimal()

# подбор более подходящей модели
MASS::stepAIC(fit, direction = "both")

#############
# new model, полученная в результате применения MASS::stepAIC(fit, direction = "both")

fit1 <- glm(
  formula = biopsy ~ hormonal_contraceptives_years + st_ds,
  family = "binomial", data = cervical
)

fit1_text <- 'fit1 <- glm(formula = biopsy ~ hormonal_contraceptives_years + st_ds,
family = "binomial", data = cervical)'

glance_fit1 <- fit1 %>%
  glance() %>%
  regulartable() %>%
  width(j = col_glance_fit, width = c(.9, .7, .7, rep(.55, 2), rep(.7, 2))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

tidy_fit1 <- fit1 %>%
  tidy(conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    one_in_ci = ifelse(conf.low < 1 & conf.high > 1, "yes", "no")
  ) %>%
  regulartable() %>%
  width(j = col_tidy_fit, width = c(1.8, rep(.7, 7))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

# Wed Apr 24 20:33:25 2019 ------------------------------
# Классификационный анализ с использованием новой модели, используется пакет pROC

# dataframe с предсказанными значениями вероятности признака
fit_1 <- augment(fit1, type.predict = "response") %>%
  select(1:4)

# roc объект
roc_data <- roc(biopsy ~ .fitted, data = fit_1)

# ROC кривая
gg2 <- yardstick::roc_curve(fit_1, biopsy, .fitted) %>% autoplot()

# threshold
(thr <- coords(roc_data, "best"))

# dataframe с бинарной переменной предсказанных значений
fit_1_matrix <- fit_1 %>%
  mutate(preds = as.factor(ifelse(.fitted <= thr[1], 0, 1))) %>%
  select(preds, biopsy)

conf_matr <- with(fit_1_matrix, table(preds, biopsy)) %>%
  DescTools::Rev()

conf_matr %>%
  as.data.frame() %>%
  regulartable()

# Показатели оценки модели

param_contab <- tibble(
  parameter = c("Accuracy", "Precision", "Recall", "Specificity", "F measure"),
  value = c(
    confusionMatrix(conf_matr)$overall[[1]],
    precision(conf_matr),
    recall(conf_matr),
    specificity(conf_matr),
    F_meas(conf_matr)
  )
) %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)


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
delong <- roc.test(roc_train_data, roc_test_data)

delong_result <- tibble(
  parameter = c("Method", "Data", "AUC of ROC1", "AUC of ROC2", "Statistic", "df", "p-value"),
  value = c(
    delong$method,
    delong$data.names,
    round(delong$estimate[1], 3),
    round(delong$estimate[2], 3),
    round(delong$statistic, 3),
    round(delong$parameter, 3),
    round(delong$p.value, 3)
  )
) %>%
  regulartable() %>%
  width(j = c("parameter", "value"), width = c(1, 2)) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)


library(finalfit)
explanatory <- c(
  "age", "hormonal_contraceptives_years", "iud",
  "num_of_pregnancies", "smokes", "st_ds"
)
dependent <- "biopsy"

final_tab <- cervical %>%
  finalfit(dependent, explanatory) %>%
  rename(range = "")

col_final <- colnames(final_tab)
final_tab <- final_tab %>%
  regulartable() %>%
  width(j = col_final, width = c(1.7, rep(.7, 3), rep(1.4, 2))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

##################################################
## Создание docx файла

docs <- read_docx() %>%
  body_add_par("Результаты анализа", style = "heading 2") %>%
  body_add_par(" ") %>%
  body_add_par("Таб.1 Количественные переменные", style = "table title") %>%
  body_add_flextable(t1, align = "left") %>%
  body_add_par(value = " ") %>%
  body_add_par(paste0(" ", "Таб.2 Категориальные переменные"), style = "table title") %>%
  body_add_flextable(t2, align = "left") %>%
  body_add_par(value = " ") %>%
  body_add_par("Модель fit", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par(value = fit_text) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.3 glance(fit)", style = "table title") %>%
  body_add_flextable(glance_fit) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.4 tidy(fit)", style = "table title") %>%
  body_add_flextable(tidy_fit) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.1 Коэффициенты модели fit", style = "graphic title") %>%
  body_add_gg(value = gg1, width = 6, height = 4) %>%
  body_add_par(value = " ") %>%
  body_add_par("Модель fit1", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par(value = fit1_text) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.5 glance(fit1)", style = "table title") %>%
  body_add_flextable(glance_fit1) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.6 tidy(fit1)", style = "table title") %>%
  body_add_flextable(tidy_fit1) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.2 ROC кривая", style = "graphic title") %>%
  body_add_gg(value = gg2, width = 3, height = 3) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.7 Показатели оценки классификационной модели", style = "table title") %>%
  body_add_flextable(param_contab) %>%
  body_add_par(value = " ") %>% 
  body_add_par("Таб.8 DeLong's test для двух ROC кривых", style = "table title") %>%
  body_add_flextable(delong_result) %>%
  body_add_par(value = " ") %>% 
  body_add_par("finalfit применение", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.9 Univariate & multivariate OR", style = "table title") %>%
  body_add_flextable(final_tab)


print(docs, target = "doc_p2.docx")
