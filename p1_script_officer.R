# Script Word doc generation
# https://davidgohel.github.io/officer/articles/word.html
# https://davidgohel.github.io/officer/articles/offcran/tables.html

# таблицы и графики, создаваемые в p1_script.R будут собираться в docx-файл


library(tidyverse)
library(broom)
library(car)
library(GGally)
library(DescTools)
library(ggthemes)
library(flextable)
library(officer)


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

t1 <- df1 %>% 
  select_if(is.numeric) %>% 
  skimr::skim_to_wide() %>% 
  select(-c(1, 3 : 5)) %>% 
  mutate(variable = str_replace_all(variable, "_", " "))
  

colt1 <- names(t1)
# fontname <- "Arial Narrow"
t1 <- t1 %>% 
  regulartable() %>% 
  width(j = colt1, width = c(1, rep(.6, 7), 1.2)) %>%  
  style(pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
        pr_p = fp_par(text.align = "center"), part = "all") %>% 
  bold(part = "header") %>% theme_box() %>% height_all(0)
  
#  align(align = "center", part = "all") %>% 
#  font(fontname = fontname) %>% 
#  fontsize(size = 9) %>% theme_box() %>% height_all(0)
  
  
#
t2 <- df1 %>% 
  select_if(is.factor) %>% 
  skimr::skim_to_wide() %>% 
  select(-c(1, 3 : 5, 8)) %>% 
  mutate(variable = str_replace_all(variable, "_", " ")) 

colt2 <- names(t2)
t2 <- t2 %>% 
  regulartable() %>% 
  width(j = colt2, width = c(1.5, .7, 3)) %>%
  style(pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
        pr_p = fp_par(text.align = "center"), part = "all") %>% 
  bold(part = "header") %>% theme_box() %>% height_all(0) 

# gpl <- df1 %>% 
#  select_if(is.numeric) %>% 
#  GGally::ggpairs()


## fit1
fit1 <- lm(BIrthweight ~ Birthlength + Gestational_age + Marital_status + 
             Education + BMI_group + age_group + Smoking + Infant_sex +
             Vitamins_before_pregnancy * Vitamis_during_pregnancy +
             Folic_acid_before_pregnancy * Folic_acid_during_pregnancy, df1)

fit1_text <- "fit1 <- lm(BIrthweight ~ Birthlength + Gestational_age + Marital_status + 
             Education + BMI_group + age_group + Smoking + Infant_sex +
Vitamins_before_pregnancy * Vitamis_during_pregnancy +
Folic_acid_before_pregnancy * Folic_acid_during_pregnancy, df1)"


# variance influence factor
rnames <- row.names(car::vif(fit1))
vif_fit1 <- car::vif(fit1) %>% as.data.frame() %>% select(1) %>% 
  mutate(variable = rnames, 
         variable = str_replace_all(variable, "_", " ")) %>% select(2, 1) %>% 
  regulartable() %>% 
  style(pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
        pr_p = fp_par(text.align = "center"), part = "all") %>% 
  bold(part = "header") %>% theme_box() %>% height_all(0) %>% 
  autofit()


glance_fit1 <- fit1 %>% 
  glance() 

col_glance_fit <- names(glance_fit1)

glance_fit1 <- glance_fit1 %>% 
  regulartable() %>% 
  width(j = col_glance_fit, width = c(.7, .8, rep(.55, 3), .3, rep(.7, 5))) %>% 
  style(pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
        pr_p = fp_par(text.align = "center"), part = "all") %>% 
  bold(part = "header") %>% theme_box() %>% height_all(0)

# colformat_num(col_keys = col_glance_fit, digits = c(3,3,1,1,3,0,1,1,1,0,0)) %>%
  
tidy_fit1 <- fit1 %>% 
  tidy(conf.int = TRUE) %>% 
  mutate(term = str_replace_all(term, "_", " "),
         zero_in_ci = ifelse(conf.low < 0 & conf.high > 0, "yes", "no"))

col_tidy_fit <- names(tidy_fit1)

tidy_fit1 <- tidy_fit1 %>%  
  regulartable() %>% 
  width(j = col_tidy_fit, width = c(1.8, rep(.7, 7))) %>% 
  style(pr_t = fp_text(font.size = 9, font.family = "Arial Narrow"),
        pr_p = fp_par(text.align = "center"), part = "all") %>% 
  bold(part = "header") %>% theme_box() %>% height_all(0) 
  
gg1 <- fit1 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(!str_detect(term, "Intercept")) %>% 
  mutate(term = str_replace_all(term, "_", " "),
         term = str_wrap(term, 30)) %>% 
  ggplot(aes(term, estimate, ymin = conf.low, ymax = conf.high)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") +
  geom_errorbar(width = .6) +
  coord_flip() 

              
# docs <- read_docx()
# styles_info(docs)

docs <- read_docx()  %>% 
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
  body_add_par("Таб.5 vif(fit1)", style = "table title") %>% 
  body_add_flextable(vif_fit1, align = "left") %>% 
  body_add_par(value = " ") %>% 
  body_add_par("Рис.1 Коэффициенты модели fit1", style = "graphic title") %>%  
  body_add_gg(value = gg1, width = 6, height = 5)


print(docs, target = "docs.docx")
