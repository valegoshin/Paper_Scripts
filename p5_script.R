# Thu Feb  6 21:04:21 2020 ------------------------------
# Principal Component Analysis & Factor Analysis -----

# used packages -----

library(tidyverse)
library(ggrepel)
library(factoextra)


# data -----
decathlon <- read_csv("decathlon.csv")

# Top 6 tables -----
decathlon %>%
  select(1:8) %>%
  head() %>%
  knitr::kable(caption = "Таб.1 6 лучших")

decathlon %>%
  select(1, 2, 9:13) %>%
  head() %>%
  knitr::kable(caption = "Таб.2 6 лучших продолжение")

# показатели описательной статистики -----
psych::describe(decathlon[, 4:13], fast = TRUE) %>%
  knitr::kable(digits = 3, caption = "Таб.3 Некоторые показатели описательной статистики")

# корреляционная матрица -----
psych::pairs.panels(decathlon[, 4:13], gap = 0)



# Оценка возможностей использования анализа главных компонент и факторного анализа -----

# критерий сферичности Бартлетта -----
psych::cortest.bartlett(cor(decathlon[4:13]))


# критерий Кайзера-Мейера-Олкина ------
psych::KMO(cor(decathlon[4:13]))


# Анализ главных компонент с использованием функции `prcomp` -----

# создание матрицы данных -----
deca <- as.matrix(decathlon[, 4:13])
rownames(deca) <- decathlon[, 1] %>% pull()

# создание объекта prcomp -----
d_pca <- prcomp(deca, center = TRUE, scale. = TRUE)

summary(d_pca)

# значения первых 3 столбцоы -----
d_pca$rotation[, 1:3]

# значение первых шести строк первых трех главных компонентов -----
d_pca$x[, 1:3] %>% head()

# Выбор количества компонент -----

# Extract eigenvalues/variances -----
get_eig(d_pca) %>%
  knitr::kable(digits = 3, caption = "Таб. Собственные значения / дисперсия")

# собственные значения - вектор дисперсий для главных компонент -----
(pca.var <- d_pca$sdev**2)

# дисперсии главных компонент в процентах -----
(pca.var2 <- round(pca.var / sum(pca.var) * 100, 1))


# Visualize eigenvalues/variances -----
screeplot(d_pca, type = "l", npcs = 10, main = "Screeplot of the first 10 PCs")
abline(h = 1, lty = 3) 

# График 'каменистая осыпь' и правило Kaiser-Guttman -----
tibble(
  x = 1:10,
  y = pca.var
) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, lty = 2) +
  scale_x_continuous(breaks = 1:10, labels = paste0("PC", as.character(1:10))) +
  labs(x = "", y = "Eugenvalue")

# Доля объяснённой дисперсии для каждой компоненты -----
tibble(
  x = 1:10,
  y = pca.var / sum(pca.var)
) %>%
  ggplot(aes(x, y)) +
  geom_col(fill = "grey70") +
  expand_limits(y = c(0, .4)) +
  geom_text(aes(label = paste0(as.character(round(y * 100, 1)), "%")), vjust = -.2, size = 3) +
  scale_x_continuous(breaks = 1:10, labels = paste0("PC", as.character(1:10))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "Proportion of Variance")

# Суммированная доля объяснённой дисперсии -----
tibble(
  x = 1:10,
  y = pca.var / sum(pca.var),
  cumy = cumsum(y)
) %>%
  ggplot(aes(x, cumy)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = paste0(as.character(round(cumy * 100, 1)), "%")), vjust = -.2, size = 3) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = 1:10, labels = paste0("PC", as.character(1:10))) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "Cumulative Proportion")


# Представление данных с использованием главных компонент ------

pca12 <- paste(as.character(round((pca.var2[1] + pca.var2[2])), 1), "%")
pca14 <- paste(as.character(round((sum(pca.var2[1:4]))), 1), "%")

tibble(
  PC1 = d_pca$x[, 1],
  PC2 = d_pca$x[, 2],
  score = decathlon$overall_points,
  rank = decathlon$rank
) %>%
  ggplot(aes(PC1, PC2, label = rank, color = score)) +
  geom_point(show.legend = FALSE) +
  geom_text_repel(show.legend = FALSE, size = 3) +
  scale_color_gradient(low = "grey90", high = "black") +
  labs(
    title = "PC1 vs. PC2",
    x = paste0("PC1 (", as.character(pca.var2[1]), " %)"),
    y = paste0("PC2 (", as.character(pca.var2[2]), " %)")
  )

tibble(
  PC1 = d_pca$rotation[, 1],
  PC2 = d_pca$rotation[, 2],
  vid = rownames(d_pca$rotation)
) %>%
  ggplot(aes(PC1, PC2, label = vid)) +
  geom_point() +
  geom_text_repel() +
  geom_vline(xintercept = 0, lty = 3) +
  geom_hline(yintercept = 0, lty = 3) +
  labs(
    title = "",
    x = paste0("PC1 (", as.character(pca.var2[1]), " %)"),
    y = paste0("PC2 (", as.character(pca.var2[2]), " %)")
  )


fviz_pca_biplot(d_pca)


# создание групповой переменной
decathlonl <- read_csv("decathlon.csv") %>%
  mutate(r_group = factor(ifelse(rank <= 6, "Top 6", "Others")))


fviz_pca_ind(d_pca,
             geom.ind = c("point", "text"),
             pointshape = 21,
             pointsize = 2,
             fill.ind = factor(decathlonl$r_group),
             col.ind = "black",
             palette = c("grey60", "black"),
             addEllipses = TRUE,
             col.var = "black",
             repel = TRUE,
             legend.title = "Top 6"
) +
  labs(title = "Индивидуальные значения - PCA")

# Факторный анализ с использованием функции `factanal` -----

fa_d <- factanal(scale(decathlon[, 4:13]), factors = 3, scores = "Bartlett", rotation = "varimax")
print(fa_d, digits = 3, cutoff = .4, sort = TRUE)

# первые 6 строк значений исходных данных в новых переменных
fa_d$scores %>% head()

tibble(
  running = fa_d$scores[, 1],
  jumping = fa_d$scores[, 2],
  throwing = fa_d$scores[, 3],
  rank = 1:23
) %>%
  ggplot(aes(running, jumping, label = rank)) +
  geom_point(aes(size = throwing), shape = 21) +
  geom_text_repel(vjust = -1) +
  scale_size(name = "Throw", breaks = c(-2, -1, 0, 1, 2), range = c(1, 6))


