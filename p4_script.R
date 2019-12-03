# Tue Dec 03 15:56:02 2019 ------------------------------
# Quantile Regression

# used packages
library(tidyverse)
library(broom)
library(quantreg)
library(janitor)

# Data
# импорт данных
ipf_lifts <- read_csv("ipf_lifts.csv")

# преобразование данных
df <- ipf_lifts %>%
  mutate(year = lubridate::year(date)) %>%
  # выбор записе1 по условию
  filter(
    age_class %in% c("20-23", "24-34", "35-39", "40-44", "45-49"),
    year >= 2000,
    bodyweight_kg <= 150,
    equipment == "Single-ply",
    place %in% as.character(1:20)
  ) %>%
  # выбор переменных и переименование некоторых из них
  select(
    squat = best3squat_kg,
    bench = best3bench_kg,
    deadlift = best3deadlift_kg,
    age, age_class, bodyweight_kg, sex
  ) %>%
  # изменение формы таблицы
  pivot_longer(1:3, names_to = "exercise", values_to = "result")

# таблица половозрастной состав
df %>%
  tabyl(age_class, sex) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable(caption = "Таб.1 Половозрастной состав")


# график Распределение результатов
df %>% 
  ggplot(aes(result, fill = sex)) +
  geom_density(alpha = .5) +
  scale_fill_grey() +
  facet_wrap(~ exercise, nrow = 1)

## Линейная модель
# линейная модель базовыми функциями R
model <- lm(result ~ bodyweight_kg + age, data = df)
summary(model)

# коэффициенты линейной регрессии - с использованием функций пакетов dplyr и broom
df %>%
  lm(result ~ bodyweight_kg + age, data = .) %>%
  tidy(conf.int = TRUE) %>%
  knitr::kable(digits = 3, caption = "Таб.2 Коэффициенты регрессии - результат от веса спортсмена")

# коэффициенты линейной регрессии - стратификация по полу и упражнениям
df %>%
  group_by(exercise, sex) %>% # группировка данных
  nest() %>% # объединение данных в группах
  # создание объектов с результатами линейной регрессии
  mutate(lm_obj = map(data, ~ lm(result ~ bodyweight_kg + age, data = .))) %>%
  # создание объектов с коэффициентами и доверительными интервалами линейной регрессии
  mutate(tidy_lm = map(lm_obj, tidy, conf.int = TRUE)) %>%
  ungroup() %>% # удаление группировки
  transmute(exercise, sex, tidy_lm) %>% # создание новой таблицы
  unnest(cols = c(tidy_lm)) %>% # "распаковка" объектов с коэффициентами
  filter(term != "(Intercept)") %>% # отбор строк
  select(term, sex, exercise, estimate, conf.low, conf.high) %>% # отбор столбцов
  arrange(desc(term), sex) %>% # сортировка
  # упорядоченный вывод данных в таблице
  knitr::kable(digits = 3, caption = "Таб.3 Коэффициенты регрессии - результат от веса и возраста спортсмена, стратифицированные по упражнениям и полу")

# График Масса тела спортсмена и результат
df %>%
  ggplot(aes(bodyweight_kg, result)) +
  # создание точечной диаграммы
  geom_jitter(alpha = .7, color = "grey50", shape = ".", size = 6) +
  # создание графика линейной регрессии
  geom_smooth(method = lm, color = "black") +
  # стратификация данных
  facet_grid(sex ~ exercise, scales = "free_y")

# График Возраст спортсмена и результат
df %>%
  ggplot(aes(age, result)) +
  geom_jitter(alpha = .7, color = "grey50", shape = ".", size = 6) +
  geom_smooth(method = lm, color = "black") +
  facet_grid(sex ~ exercise, scales = "free_y")

## Квантильные диаграммы

df %>%
  ggplot(aes(bodyweight_kg, result)) +
  geom_jitter(alpha = .7, color = "grey50", shape = ".", size = 6) +
  geom_quantile(quantiles = c(.1, .25, .5, .75, .9), color = "black") +
  facet_grid(sex ~ exercise, scales = "free_y")

df %>%
  ggplot(aes(age, result)) +
  geom_jitter(alpha = .7, color = "grey50", shape = ".", size = 6) +
  geom_quantile(quantiles = c(.1, .25, .5, .75, .9), color = "black") +
  facet_grid(sex ~ exercise, scales = "free_y")


## Квантильная регрессия методами пакета quantreg

model <- rq(result ~ bodyweight_kg + age, data = df, tau = c(.1, .25, .5, .75, .9))

summary(model)

plot(summary(model))

## Изучение квантильной регрессии с использованием функций пакетов 
## dplyr, tidyr, purrr, broom

# Результат и вес спортсмена

frq <- df %>%
  select(exercise, sex, result, bodyweight_kg) %>%
  group_by(exercise, sex) %>%
  nest() %>%
  mutate(rq_obj = map(data, ~ rq(result ~ bodyweight_kg, data = ., tau = c(.1, .25, .5, .75, .9)))) %>%
  mutate(tidy_rq = map(rq_obj, tidy, conf.int = TRUE)) %>%
  ungroup() %>%
  transmute(exercise, sex, tidy_rq) %>%
  unnest(cols = c(tidy_rq)) %>%
  filter(term != "(Intercept)")

frq %>%
  # отбор переменных и создание новой переменной
  transmute(exercise, sex, tau, est_conf = paste0(
    as.character(round(estimate, 2)),
    " (",
    as.character(round(conf.low, 3)),
    " - ",
    as.character(round(conf.high, 3)),
    ")"
  )) %>%
  # создание "широкой" таблицы
  pivot_wider(names_from = tau, values_from = est_conf, names_prefix = "tau_") %>%
  # упорядоченный вывод данных в таблице
  knitr::kable(caption = "Таб.4 Коэффициенты квантильной регрессии результат от массыт тела, стратифицированные по упражнениям и полу")

flm <- df %>%
  select(exercise, sex, result, bodyweight_kg) %>%
  group_by(exercise, sex) %>%
  nest() %>%
  mutate(lm_obj = map(data, ~ lm(result ~ bodyweight_kg, data = .))) %>%
  mutate(tidy_lm = map(lm_obj, tidy, conf.int = TRUE)) %>%
  ungroup() %>%
  transmute(exercise, sex, tidy_lm) %>%
  unnest(cols = c(tidy_lm)) %>%
  filter(term != "(Intercept)") %>%
  select(exercise, sex, term, estimate_lm = estimate, conf.low_lm = conf.low, conf.high_lm = conf.high)

left_join(frq, flm, by = c("exercise", "sex")) %>%
  ggplot(aes(tau, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = .8) +
  geom_point(alpha = .5) +
  geom_line(alpha = .5) +
  geom_hline(aes(yintercept = estimate_lm), linetype = 2) +
  geom_hline(aes(yintercept = conf.low_lm), linetype = 3) +
  geom_hline(aes(yintercept = conf.high_lm), linetype = 3) +
  facet_grid(sex ~ exercise, scales = "free_y")


# Результат и возраст

frq <- df %>%
  select(exercise, sex, result, age) %>%
  group_by(exercise, sex) %>%
  nest() %>%
  mutate(rq_obj = map(data, ~ rq(result ~ age, data = ., tau = c(.1, .25, .5, .75, .9)))) %>%
  mutate(tidy_rq = map(rq_obj, tidy, conf.int = TRUE)) %>%
  ungroup() %>%
  transmute(exercise, sex, tidy_rq) %>%
  unnest(cols = c(tidy_rq)) %>%
  filter(term != "(Intercept)")

frq %>%
  transmute(exercise, sex, tau, est_conf = paste0(
    as.character(round(estimate, 2)),
    " (",
    as.character(round(conf.low, 3)),
    " - ",
    as.character(round(conf.high, 3)),
    ")"
  )) %>%
  pivot_wider(names_from = tau, values_from = est_conf, names_prefix = "tau_") %>%
  knitr::kable(caption = "Таб.5 Коэффициенты квантильной регрессии результат от возраста, стратифицированные по упражнениям и полу")

flm <- df %>%
  select(exercise, sex, result, age) %>%
  group_by(exercise, sex) %>%
  nest() %>%
  mutate(lm_obj = map(data, ~ lm(result ~ age, data = .))) %>%
  mutate(tidy_lm = map(lm_obj, tidy, conf.int = TRUE)) %>%
  ungroup() %>%
  transmute(exercise, sex, tidy_lm) %>%
  unnest(cols = c(tidy_lm)) %>%
  filter(term != "(Intercept)") %>%
  select(exercise, sex, term, estimate_lm = estimate, conf.low_lm = conf.low, conf.high_lm = conf.high)

left_join(frq, flm, by = c("exercise", "sex")) %>%
  ggplot(aes(tau, estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightgrey", alpha = .8) +
  geom_point(alpha = .5) +
  geom_line(alpha = .5) +
  geom_hline(aes(yintercept = estimate_lm), linetype = 2) +
  geom_hline(aes(yintercept = conf.low_lm), linetype = 3) +
  geom_hline(aes(yintercept = conf.high_lm), linetype = 3) +
  facet_grid(sex ~ exercise, scales = "free_y")

## Прогнозирование

# создание набора данных для прогнозирования
df1 <- df %>%
  filter(exercise == "squat") %>%
  select(result, age, bodyweight_kg) %>%
  drop_na()

# разделение на тренировочный и тестовый наборы
set.seed(123999)
ind <- caret::createDataPartition(df1$result, p = .7, list = FALSE)
train_data <- df1[ind, ]
test_data <- df1[-ind, ]

# модель квантильной регрессии
rq_train <- rq(result ~ bodyweight_kg + age, train_data, tau = c(.1, .25, .5, .75, .9))

# прогнозные значения для тестового набора
predictions <- predict(rq_train, newdata = test_data, interval = "prediction")

# получение оценок MAPE для тренировочного набора
train_mape <- rq_train %>%
  augment() %>%
  select(result, .tau, .fitted) %>%
  mutate(mape = abs(result - .fitted) / result) %>%
  group_by(.tau) %>%
  summarise(mape_train = mean(mape))

# получение оценок MAPE для тестового набора
test_mape <- data.frame(
  real_d = test_data$result,
  fitted_d = predictions
) %>%
  set_names("result", "tau10", "tau25", "tau50", "tau75", "tau90") %>%
  pivot_longer(-result, names_to = "tau", values_to = "preds") %>%
  mutate(mape = abs(result - preds) / result) %>%
  group_by(tau) %>%
  summarise(mape_test = mean(mape))

# объединение результатов и вывод их в таблице
bind_cols(train_mape, test_mape) %>%
  select(tau, mape_train, mape_test) %>%
  knitr::kable(digits = 3, caption = "Таб.6 МАРЕ для тренировочных и предсказанных значений")


