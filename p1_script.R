# script for paper Multiple Linear Regression
# 27 февраля 2019 года


# used packages and functions

library(tidyverse)
library(broom)
library(car)
library(GGally)
library(DescTools)
library(ggthemes)


# функция преобразования выбросов в NA
Outl_NA <- function(x) {x[which(x %in% boxplot.stats(x)$out)] <- NA;  x}

# функции для расчета MAPE с 95% доверительным интервалом
lowl <- function(x) {mean(x) - MeanSE(x) * 1.96}
highl <- function(x) {mean(x) + MeanSE(x) * 1.96}

mape_datci <- function(dat, n=3) {
  dat %>% 
    set_names("x", "y") %>% 
    mutate(z = abs(x-y)/x) %>% 
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
  mutate(BMI = Maternal_weight/(Maternal_height * .01) ** 2,
         Smoking = factor(ifelse(Smoking_before_pregnancy == "yes" | Smoking_during_pregnancy == "yes", "yes", "no")),
         BMI_group = cut(BMI, breaks = c(0, 18.5, 25, 30, Inf),
                         labels = c("<18.5", "18.5-25", "25-30", "30+")),
         age_group = cut(Maternal_age, breaks = c(10, 18, 25, 30, 35, Inf),
                         right = FALSE,
                         labels = c("<18", "18-25", "25-30", "30-35", "35+"))) %>% 
  # выбор переменных для включения в модель
  select(BIrthweight, Birthlength, Gestational_age, Marital_status,
         Education, BMI_group, Maternal_height, Maternal_age, age_group,
         Vitamins_before_pregnancy, Vitamis_during_pregnancy, 
         Folic_acid_before_pregnancy, Folic_acid_during_pregnancy,
         Infant_sex, Smoking) %>%
  na.omit() %>% # удаление строк с пустыми значениями
  # удаление записей с малым количеством значений переменной
  filter(Marital_status != "Other",
         !Education %in% c("None", "Unknown")) %>% 
  # преобразование номинальных переменных
  mutate(Marital_status = fct_relevel(Marital_status, "Married"),
         Infant_sex = factor(Infant_sex, levels = c("Male", "Female")),
         age_group = fct_relevel(age_group, "25-30"), 
         BMI_group = fct_relevel(BMI_group, "18.5-25"),
         Education = fct_relevel(Education, "Technical School"))

# таблица данных после удаления выбросов
df1 <- df1 %>% 
  mutate_if(is.numeric, Outl_NA) %>% 
  na.omit()

# 
glimpse(df1)

df1 %>% 
  select_if(is.numeric) %>% 
  skimr::skim_to_wide() %>% 
  select(-c(1, 3 : 5)) %>% 
  knitr::kable(caption = "Таб.1 Непрерывные переменные")
#
df1 %>% 
  select_if(is.factor) %>% 
  skimr::skim_to_wide() %>% 
  select(-c(1, 3 : 5)) %>% 
  knitr::kable(caption = "Таб.2 Категориальные переменные")

### 
df1 %>% 
  select_if(is.numeric) %>% 
  ggpairs()

## fit1
fit1 <- lm(BIrthweight ~ Birthlength + Gestational_age + Marital_status + 
             Education + BMI_group + age_group + Smoking + Infant_sex +
             Vitamins_before_pregnancy * Vitamis_during_pregnancy +
             Folic_acid_before_pregnancy * Folic_acid_during_pregnancy, df1)

# variance influence factor
vif(fit1) %>% as.data.frame() %>% select(1)

# model & broom package finctions 

fit1 %>% glance()
fit1 %>% tidy(conf.int = TRUE) 
fit1 %>% augment() %>% head()


fit1 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!str_detect(term, "Intercept")) %>% 
  mutate(term = str_replace_all(term, "_", " "),
         term = str_wrap(term, 30)) %>% 
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_errorbar(width = .6) +
  coord_flip() +
  ggtitle("Параметры модели fit1") +
  theme_minimal()


fit2 <- lm(formula = BIrthweight ~ Birthlength + Gestational_age, data = df1)

glance(fit2)
vif(fit2)
tidy(fit2, conf.int = TRUE)

fit2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!str_detect(term, "Intercept")) %>% 
  mutate(term = str_replace_all(term, "_", " "),
         term = str_wrap(term, 30)) %>% 
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_errorbar(width = .6) +
  coord_flip() +
  ggtitle("Параметры модели fit2") +
  theme_minimal()


# Cook's distance plot
fit2 %>% 
  augment() %>% 
  mutate(n_row = row_number()) %>% 
  ggplot(aes(n_row, .cooksd)) + 
  geom_col() + 
  geom_hline(yintercept = 4 * mean(cooks.distance(fit2)), 
             color = "red", linetype = "dotted") +
  geom_text(data = augment(fit2) %>% 
              mutate(n_row = row_number()) %>% 
              arrange(desc(.cooksd)) %>% 
              slice(1:12), 
            aes(x = n_row, y = .cooksd, label = n_row), size = 2) +
  ggtitle("Модель fit2: Cook's distance") +
  theme_minimal()


df_d <- fit2 %>% 
  augment() %>% 
  filter(.cooksd < 4 * mean(.cooksd)) %>% 
  select(1:4)

fit3 <- lm(formula = BIrthweight ~ Birthlength + Gestational_age, data = df_d)
glance(fit3)
vif(fit3)
tidy(fit3, conf.int = TRUE)

# compare models
fit_glance <- 
  cbind(fit1 %>% glance() %>%  select(c(1:5, 8, 9)) %>% t(), 
        fit2 %>% glance() %>% select(c(1:5, 8, 9)) %>% t(),
        fit3 %>% glance() %>% select(c(1:5, 8, 9)) %>% t()) 

colnames(fit_glance) <- c("fit1", "fit2", "fit3")

options(scipen = 999)
fit_glance %>% 
  knitr::kable(digits = 3, caption = "Таб.8 Параметры моделей")

# Диагностика модели
g1 <- fit3 %>% 
  augment() %>% 
  ggplot(aes(.resid)) +
  geom_density()  +
  stat_function(fun = dnorm, 
                args = list(mean(residuals(fit3)), sd(residuals(fit3))),
                linetype = 3) + 
  geom_vline(xintercept = 0, linetype = 2) +
  ggtitle("Диаграмма плотности")

g2 <- fit3 %>% 
  augment() %>% 
  ggplot(aes(sample = .resid)) + stat_qq() + stat_qq_line() +
  ggtitle("Квантильная диаграмма")

gridExtra::grid.arrange(g1, g2, nrow = 1)


g1 <- fit3 %>% 
  augment() %>% 
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = .5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("fitted vs. residuals")

g2 <- fit3 %>% 
  augment() %>% 
  ggplot(aes(.fitted, .std.resid)) + geom_point(alpha = .5) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ggtitle("fitted vs. standardized residuals")

gridExtra::grid.arrange(g1, g2, nrow = 1)

# Диагностика модели - тесты
# оценка независимости остатков. runs test - Wald-Wolfowitz-Test
(dt_runs <- RunsTest(residuals(fit3))) # DescTools package

# тест на аутокорреляцию
(dw <- durbinWatsonTest(fit3)) # car package

# оценка гомоскедастичности - Breusch-Pagan test
(ncvt <- ncvTest(fit3))      # car package


# validation
set.seed(123) 
row_index <- sample(1:nrow(df_d), .8 * nrow(df_d))
train_data <- df_d[row_index, ] # создание тренировочного набора данных
test_data <- df_d[-row_index, ] # создание тестового набора данных


# модель, основанная на тренировочном наборе данных
lm_train <- lm(formula = BIrthweight ~ Birthlength + Gestational_age, data = train_data) 


# MAPE для тренировочного набора данных
(mape_train <- lm_train %>% 
    augment() %>% 
    select(BIrthweight, .fitted) %>% 
    mape_datci(4))

# MAPE для тестового набора данных
(mape_test <- lm_train %>% 
    augment(newdata = test_data) %>% 
    select(BIrthweight, .fitted) %>% 
    mape_datci(4))


library(finalfit)

explanatory <- c("Birthlength", "Gestational_age")
dependent <- "BIrthweight"

df1 %>% 
  finalfit(dependent, explanatory) %>% 
  knitr::kable()
