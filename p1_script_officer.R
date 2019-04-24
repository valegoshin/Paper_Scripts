# Script Word doc generation -----
# https://davidgohel.github.io/officer/articles/word.html -----
# https://davidgohel.github.io/officer/articles/offcran/tables.html -----

# таблицы и графики, создаваемые в p1_script.R будут собираться в docx-файл -----


library(tidyverse)
library(broom)
library(car)
library(DescTools)
library(ggthemes)
library(flextable)
library(officer)


# функция преобразования выбросов в NA
Outl_NA <- function(x) {
  x[which(x %in% boxplot.stats(x)$out)] <- NA
  x
}

# функции для расчета MAPE с 95% доверительным интервалом
lowl <- function(x) {
  mean(x) - MeanSE(x) * 1.96
}
highl <- function(x) {
  mean(x) + MeanSE(x) * 1.96
}

mape_datci <- function(dat, n = 3) {
  dat %>%
    set_names("x", "y") %>%
    mutate(z = abs(x - y) / x) %>%
    summarise_at(vars(z), c(mean, lowl, highl)) %>%
    as.numeric() %>%
    round(n)
}


# Data -----
# импорт из файла
df <- foreign::read.spss("Simulated_sample.sav", to.data.frame = TRUE)

# преобразование данных
df1 <- df %>%
  # отбор записей по значениюю возраста матери
  # filter(Maternal_age >= 18) %>%
  # создание новых переменных
  mutate(
    BMI = Maternal_weight / (Maternal_height * .01)**2,
    Smoking = factor(ifelse(Smoking_before_pregnancy == "yes" | Smoking_during_pregnancy == "yes", "yes", "no")),
    BMI_group = cut(BMI,
      breaks = c(0, 18.5, 25, 30, Inf),
      labels = c("<18.5", "18.5-25", "25-30", "30+")
    ),
    age_group = cut(Maternal_age,
      breaks = c(10, 18, 25, 30, 35, Inf),
      right = FALSE,
      labels = c("<18", "18-25", "25-30", "30-35", "35+")
    )
  ) %>%
  # выбор переменных для включения в модель
  select(
    BIrthweight, Birthlength, Gestational_age, Marital_status,
    Education, BMI_group, Maternal_height, Maternal_age, age_group,
    Vitamins_before_pregnancy, Vitamis_during_pregnancy,
    Folic_acid_before_pregnancy, Folic_acid_during_pregnancy,
    Infant_sex, Smoking
  ) %>%
  na.omit() %>% # удаление строк с пустыми значениями
  # удаление записей с малым количеством значений переменной
  filter(
    Marital_status != "Other",
    !Education %in% c("None", "Unknown")
  ) %>%
  # преобразование номинальных переменных
  mutate(
    Marital_status = fct_relevel(Marital_status, "Married"),
    Infant_sex = factor(Infant_sex, levels = c("Male", "Female")),
    age_group = fct_relevel(age_group, "25-30"),
    BMI_group = fct_relevel(BMI_group, "18.5-25"),
    Education = fct_relevel(Education, "Technical School")
  )

# таблица данных после удаления выбросов
df1 <- df1 %>%
  mutate_if(is.numeric, Outl_NA) %>%
  na.omit()


###############
# числовые переменные в изучаемом наборе данных -----
t1 <- df1 %>%
  select_if(is.numeric) %>%
  skimr::skim_to_wide() %>%
  select(-c(1, 3:5)) %>%
  mutate(variable = str_replace_all(variable, "_", " "))


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


t2 <- df1 %>%
  select_if(is.factor) %>%
  skimr::skim_to_wide() %>%
  select(-c(1, 3:5, 8)) %>%
  mutate(variable = str_replace_all(variable, "_", " "))

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

## fit1 -----
fit1 <- lm(BIrthweight ~ Birthlength + Gestational_age + Marital_status +
  Education + BMI_group + age_group + Smoking + Infant_sex +
  Vitamins_before_pregnancy * Vitamis_during_pregnancy +
  Folic_acid_before_pregnancy * Folic_acid_during_pregnancy, df1)

fit1_text <- "fit1 <- lm(BIrthweight ~ Birthlength + Gestational_age + Marital_status + 
             Education + BMI_group + age_group + Smoking + Infant_sex +
Vitamins_before_pregnancy * Vitamis_during_pregnancy +
Folic_acid_before_pregnancy * Folic_acid_during_pregnancy, df1)"


# variance influence factor -----
rnames <- row.names(car::vif(fit1))
vif_fit1 <- car::vif(fit1) %>%
  as.data.frame() %>%
  select(1) %>%
  mutate(
    variable = rnames,
    variable = str_replace_all(variable, "_", " ")
  ) %>%
  select(2, 1) %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0) %>%
  autofit()


# Оценка созданной модели fit1 -----
# broom::glance() result  -----

glance_fit1 <- fit1 %>%
  glance()

col_glance_fit <- names(glance_fit1)

glance_fit1 <- glance_fit1 %>%
  regulartable() %>%
  width(j = col_glance_fit, width = c(.7, .8, rep(.55, 3), .3, rep(.7, 5))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

# colformat_num(col_keys = col_glance_fit, digits = c(3,3,1,1,3,0,1,1,1,0,0)) %>%

# broom::tidy() result -----
tidy_fit1 <- fit1 %>%
  tidy(conf.int = TRUE) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    zero_in_ci = ifelse(conf.low < 0 & conf.high > 0, "yes", "no")
  )

col_tidy_fit <- names(tidy_fit1)

tidy_fit1 <- tidy_fit1 %>%
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
gg1 <- fit1 %>%
  tidy(conf.int = TRUE) %>%
  filter(!str_detect(term, "Intercept")) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    term = str_wrap(term, 30)
  ) %>%
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_errorbar(width = .6) +
  coord_flip() +
  theme_minimal()

# broom::augment() result -----
augment_fit1 <- fit1 %>%
  augment() %>%
  select(-c(1, 3:14)) %>%
  head()

augment_fit1 <- augment_fit1 %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)


#########
# fit2 -----

fit2 <- lm(formula = BIrthweight ~ Birthlength + Gestational_age, data = df1)
fit2_text <- "lm(formula = BIrthweight ~ Birthlength + Gestational_age, data = df1)"

glance_fit2 <- fit2 %>%
  glance() %>%
  regulartable() %>%
  width(j = col_glance_fit, width = c(.7, .8, rep(.55, 3), .3, rep(.7, 5))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

tidy_fit2 <- fit2 %>%
  tidy(conf.int = TRUE) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    zero_in_ci = ifelse(conf.low < 0 & conf.high > 0, "yes", "no")
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


# Cook's distance plot -----
gg2 <- fit2 %>%
  augment() %>%
  mutate(n_row = row_number()) %>%
  ggplot(aes(n_row, .cooksd)) +
  geom_col() +
  geom_hline(
    yintercept = 4 * mean(cooks.distance(fit2)),
    color = "red", linetype = "dotted"
  ) +
  geom_text(
    data = augment(fit2) %>%
      mutate(n_row = row_number()) %>%
      arrange(desc(.cooksd)) %>%
      slice(1:12),
    aes(x = n_row, y = .cooksd, label = n_row), size = 2
  ) +
  theme(panel.background = element_rect(fill = "white", colour = "white"))

# remove influence values -----
df_d <- fit2 %>%
  augment() %>%
  filter(.cooksd < 4 * mean(.cooksd)) %>%
  select(1:4)

# fit3 model -----

fit3 <- lm(formula = BIrthweight ~ Birthlength + Gestational_age, data = df_d)

fit3_text <- "m(formula = BIrthweight ~ Birthlength + Gestational_age, data = df_d)"

glance_fit3 <- fit3 %>%
  glance() %>%
  regulartable() %>%
  width(j = col_glance_fit, width = c(.67, .8, rep(.56, 3), .3, rep(.7, 5))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)


# vif(fit3) -----
# variance influence factor -----
rnames <- car::vif(fit3) %>% as.data.frame() %>% row.names()
vif_fit3 <- car::vif(fit3) %>%
  as.data.frame() %>%
  mutate(
    variable = rnames,
    variable = str_replace_all(variable, "_", " ")
  ) %>%
  select(2, 1) %>%
  set_names("variable", "vif") %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0) %>%
  autofit()

tidy_fit3 <- fit3 %>%
  tidy(conf.int = TRUE) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    zero_in_ci = ifelse(conf.low < 0 & conf.high > 0, "yes", "no")
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


gg3 <- fit3 %>%
  tidy(conf.int = TRUE) %>%
  filter(!str_detect(term, "Intercept")) %>%
  mutate(
    term = str_replace_all(term, "_", " "),
    term = str_wrap(term, 30)
  ) %>%
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_errorbar(width = .6) +
  coord_flip() +
  theme_minimal()

# compare models
fit_glance <-
  cbind(
    fit1 %>% glance() %>% select(c(1:5, 8, 9)) %>% t(),
    fit2 %>% glance() %>% select(c(1:5, 8, 9)) %>% t(),
    fit3 %>% glance() %>% select(c(1:5, 8, 9)) %>% t()
  )

colnames(fit_glance) <- c("fit1", "fit2", "fit3")

options(scipen = 999)
rnames <- row.names(fit_glance)

fit_glance_tab <- fit_glance %>%
  as.data.frame() %>%
  mutate(patameter = rnames) %>%
  select(4, 1, 2, 3) %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0) %>%
  autofit()

# Test results -----

dt_runs <- RunsTest(residuals(fit3))
dt_runs_tab <- dt_runs %>%
  glance()

dt_runs_tab <- dt_runs_tab %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)

# тест на аутокорреляцию
dw <- durbinWatsonTest(fit3) # car package

glance_dw <- glance(dw) %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0) %>%
  autofit()

# оценка гомоскедастичности - Breusch-Pagan test -----
ncvt <- ncvTest(fit3) # car package

ncvt_df <- data.frame(
  test = ncvt$test,
  statistic = round(ncvt$ChiSquare, 3),
  df = round(ncvt$Df, 0),
  p.value = round(ncvt$p, 3)
)

nctv_tab <- ncvt_df %>%
  regulartable() %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0) %>%
  autofit()

# Графическая оценка

gg4 <- fit3 %>%
  augment() %>%
  ggplot(aes(Birthlength, BIrthweight)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

gg5 <- fit3 %>%
  augment() %>%
  ggplot(aes(Gestational_age, BIrthweight)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal()

gg6 <- fit3 %>%
  augment() %>%
  ggplot(aes(.resid)) +
  geom_density() +
  stat_function(
    fun = dnorm,
    args = list(mean(residuals(fit3)), sd(residuals(fit3))),
    linetype = 3
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal()

gg7 <- fit3 %>%
  augment() %>%
  ggplot(aes(sample = .resid)) + stat_qq() + stat_qq_line() +
  theme_minimal()

gg8 <- fit3 %>%
  augment() %>%
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = .5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()

gg9 <- fit3 %>%
  augment() %>%
  ggplot(aes(.fitted, .std.resid)) + geom_point(alpha = .5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal()


library(finalfit)

explanatory <- c("Birthlength", "Gestational_age")
dependent <- "BIrthweight"

final_tab <- df1 %>%
  finalfit(dependent, explanatory) %>%
  rename(range = "")

col_final <- colnames(final_tab)
final_tab <- final_tab %>%
  regulartable() %>%
  width(j = col_final, width = c(rep(1.2, 5))) %>%
  style(
    pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
    pr_p = fp_par(text.align = "center"), part = "all"
  ) %>%
  bold(part = "header") %>%
  theme_box() %>%
  height_all(0)


# docs <- read_docx()
# styles_info(docs)

docs <- read_docx() %>%
  body_add_par("Результаты анализа", style = "heading 2") %>%
  body_add_par(" ") %>%
  body_add_par("Таб.1 Количественные переменные", style = "table title") %>%
  body_add_flextable(t1, align = "left") %>%
  body_add_par(value = " ") %>%
  body_add_par(paste0(" ", "Таб.2 Категориальные переменные"), style = "table title") %>%
  body_add_flextable(t2, align = "left") %>%
  body_add_par(value = " ") %>%
  body_add_par("Модель fit1", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par(value = fit1_text) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.3 glance(fit1)", style = "table title") %>%
  body_add_flextable(glance_fit1) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.4 tidy(fit1)", style = "table title") %>%
  body_add_flextable(tidy_fit1) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.1 Коэффициенты модели fit1", style = "graphic title") %>%
  body_add_gg(value = gg1, width = 6, height = 5) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.5 augment(fit1)", style = "table title") %>%
  body_add_flextable(augment_fit1) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.6 vif(fit1)", style = "table title") %>%
  body_add_flextable(vif_fit1, align = "left") %>%
  body_add_par(value = " ") %>%
  body_add_par("Модель fit2", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par(value = fit2_text) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.7 glance(fit2)", style = "table title") %>%
  body_add_flextable(glance_fit2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.8 tidy(fit2)", style = "table title") %>%
  body_add_flextable(tidy_fit2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.2 Cook's distance plot", style = "graphic title") %>%
  body_add_gg(value = gg2, width = 6, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Модель fit3", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par(value = fit3_text) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.9 glance(fit3)", style = "table title") %>%
  body_add_flextable(glance_fit3) %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.10 vif(fit3)", style = "table title") %>%
  body_add_flextable(vif_fit3, align = "left") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.11 tidy(fit3)", style = "table title") %>%
  body_add_flextable(tidy_fit3) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.3 fit3 coefficients", style = "graphic title") %>%
  body_add_gg(value = gg3, width = 6, height = 1.5) %>%
  body_add_par(value = " ") %>%
  body_add_par("Сравнение моделей", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.12 Models comparison", style = "table title") %>%
  body_add_flextable(fit_glance_tab) %>%
  body_add_par(value = " ") %>%
  body_add_par("Диагностика модели - графики", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.4 Линейность связи между длиной и массой тела", style = "graphic title") %>%
  body_add_gg(value = gg4, width = 4, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.5 Линейность связи между сроком беременности и массой тела", style = "graphic title") %>%
  body_add_gg(value = gg5, width = 4, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.6 Нормальность распределения остатков", style = "graphic title") %>%
  body_add_gg(value = gg6, width = 4, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.7 Квантильная диаграмма", style = "graphic title") %>%
  body_add_gg(value = gg7, width = 4, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.8 fitted vs. residuals", style = "graphic title") %>%
  body_add_gg(value = gg8, width = 4, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Рис.9 fitted vs. standardized residuals", style = "graphic title") %>%
  body_add_gg(value = gg9, width = 4, height = 2) %>%
  body_add_par(value = " ") %>%
  body_add_par("Диагностика модели - тесты", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par("Оценка независимости остатков. runs test - Wald-Wolfowitz-Test", style = "heading 3") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.13 Результаты теста", style = "table title") %>%
  body_add_flextable(dt_runs_tab) %>%
  body_add_par(value = " ") %>%
  body_add_par("Тест на аутокорреляцию, Dublin-Watson Test", style = "heading 3") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.14 Результаты теста", style = "table title") %>%
  body_add_flextable(glance_dw) %>%
  body_add_par(value = " ") %>%
  body_add_par("Тест на гомоскедастичность - Breusch-Pagan test", style = "heading 3") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.15 Результаты теста", style = "table title") %>%
  body_add_flextable(nctv_tab) %>%
  body_add_par(value = " ") %>%
  body_add_par("finalfit применение", style = "heading 2") %>%
  body_add_par(value = " ") %>%
  body_add_par("Таб.16 model results", style = "table title") %>%
  body_add_flextable(final_tab)


print(docs, target = "docs.docx")
