library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(readxl)

data <- read_csv(
  "C:/Users/jandr/Downloads/Raw_underwater_vs_lab_pixels_data.csv"
)
data_G0 <- data |> dplyr::filter(Grey_patch == "G0")

data_G0 <- data_G0 |>
  mutate(Status = factor(Status)) |>
  mutate(Site = factor(Site)) |>
  mutate(ID = factor(ID)) |>
  mutate(Case = factor(Case))
Mlm <- glmmTMB(log(R) ~ Site + (1 | ID), data = data_G0, family = gaussian())
summary(Mlm)
M1m_res <- simulateResiduals(Mlm, n = 2000)
plot(M1m_res)

summary(data_G0)
colSums(is.na(data_G0))


data_G1 <- data |> dplyr::filter(Grey_patch == "G1")

data_G1 <- data_G1 |>
  mutate(Status = factor(Status)) |>
  mutate(Site = factor(Site)) |>
  mutate(ID = factor(ID)) |>
  mutate(Case = factor(Case))

Mlm <- glmmTMB(log(R) ~ Status + (1 | ID), data = data_G1, family = gaussian())
summary(Mlm)
M1m_res <- simulateResiduals(Mlm, n = 2000)
plot(M1m_res)


data_G2 <- data |> dplyr::filter(Grey_patch == "G2")

data_G2 <- data_G2 |>
  mutate(Status = factor(Status)) |>
  mutate(Site = factor(Site)) |>
  mutate(ID = factor(ID)) |>
  mutate(Case = factor(Case))

Mlm <- glmmTMB(R ~ Site + (1 | ID), data = data_G2, family = gaussian())
summary(Mlm)
M1m_res <- simulateResiduals(Mlm, n = 2000)
plot(M1m_res)

data_G3 <- data |> dplyr::filter(Grey_patch == "G3")


data_G3 <- data_G3 |>
  mutate(Status = factor(Status)) |>
  mutate(Site = factor(Site)) |>
  mutate(ID = factor(ID)) |>
  mutate(Case = factor(Case))

Mlm <- glmmTMB(R ~ Site + (1 | ID), data = data_G3, family = gaussian())
summary(Mlm)
M1m_res <- simulateResiduals(Mlm, n = 2000)
plot(M1m_res)

data_G4 <- data |> dplyr::filter(Grey_patch == "G4")

data_G4 <- data_G4 |>
  mutate(Status = factor(Status)) |>
  mutate(Site = factor(Site)) |>
  mutate(ID = factor(ID)) |>
  mutate(Case = factor(Case))

Mlm <- glmmTMB(R ~ Site + (1 | ID), data = data_G4, family = gaussian())
summary(Mlm)
M1m_res <- simulateResiduals(Mlm, n = 2000)
plot(M1m_res)

data_G5 <- data |> dplyr::filter(Grey_patch == "G5")

data_G5 <- data_G5 |>
  mutate(Status = factor(Status)) |>
  mutate(Site = factor(Site)) |>
  mutate(ID = factor(ID)) |>
  mutate(Case = factor(Case))

Mlm <- glmmTMB(R ~ Site + (1 | ID), data = data_G5, family = gaussian())
summary(Mlm)
M1m_res <- simulateResiduals(Mlm, n = 2000)
plot(M1m_res)


ggplot(data_G0, aes(x = R, fill = Site)) +
  geom_density(alpha = 0.3) +
  facet_grid(cols = vars(Case, Status)) +
  theme_classic() +
  theme(legend.position = "top")


ggplot(data_G1, aes(x = R, fill = Site)) +
  geom_density(alpha = 0.3) +
  facet_grid(cols = vars(Case, Status)) +
  theme_classic() +
  theme(legend.position = "top")


ggplot(data_G2, aes(x = R, fill = Site)) +
  geom_density(alpha = 0.3) +
  facet_grid(cols = vars(Case, Status)) +
  theme_classic() +
  theme(legend.position = "top")


ggplot(data_G3, aes(x = R, fill = Site)) +
  geom_density(alpha = 0.3) +
  facet_grid(cols = vars(Case, Status)) +
  theme_classic() +
  theme(legend.position = "top")

ggplot(data_G2, aes(x = R, fill = Site)) +
  geom_density(alpha = 0.2) +
  facet_grid(cols = vars(Case, Status)) +
  theme_classic() +
  theme(legend.position = "top")

logs <- function(x) {
  logx <- log(x)
  standardized <- logx / max(logx)
  return(standardized)
}
data <- data |> mutate(across(c(R, G, B), logs, .names = "{.col}_log"))

color <- c("blue", "green", "red")

data_long <- pivot_longer(
  data,
  cols = c(R_log, G_log, B_log),
  names_to = "RGB",
  values_to = "Intensity"
)

ggplot(data_long, aes(x = Intensity, fill = RGB)) +
  labs(
    title = "RGB intensity changes across grey patches distribution values; G0 (Higher intensity), G5 (Lower intensity)"
  ) +
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = color) +
  facet_grid(cols = vars(Grey_patch), rows = vars(RGB)) +
  theme_classic() +
  theme(legend.position = "top")

stat_summary_long <- data_long |>
  group_by(Status, Case, Grey_patch, RGB) |>
  summarise(
    Mean = mean(Intensity),
    SD_max = Mean + sd(Intensity),
    SD_min = Mean - sd(Intensity)
  )

stat_summary <- data |>
  group_by(Status, Case, Grey_patch, Site) |>
  summarise(
    Mean = mean(R_log),
    SD_max = Mean + sd(R_log),
    SD_min = Mean - sd(R_log)
  )


color <- c("white", "#F0FFFF", "#F0FFFF", "#E0EEEE", "#C1CDCD", "#838B8B")


ggplot(
  data_long,
  aes(x = interaction(Case, Status), y = Intensity, fill = Grey_patch)
) +
  geom_boxplot(alpha = 2) +
  geom_jitter(color = "grey", size = 0.5, alpha = 0.2) +
  scale_fill_manual(values = color) +
  facet_grid(cols = vars(Grey_patch), rows = vars(RGB)) +
  geom_errorbar(
    data = stat_summary_long,
    aes(x = interaction(Case, Status), ymax = SD_max, ymin = SD_min),
    size = 1,
    width = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = stat_summary_long,
    aes(x = interaction(Case, Status), y = Mean),
    size = 2,
    color = "darkorange",
    inherit.aes = FALSE
  ) +
  labs(
    title = "RGB intensity changes across grey patches distribution values; G0 (Higher intensity), G5 (Lower intensity)",
    x = "Case and Status",
    y = "Log RGB intensity (0-1)"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 60, vjust = 0.5)
  )


ggplot(data, aes(x = interaction(Case, Status), y = R_log, fill = Grey_patch)) +
  geom_boxplot(alpha = 2) +
  geom_jitter(color = "grey", size = 0.5, alpha = 0.2) +
  scale_fill_manual(values = color) +
  facet_grid(cols = vars(Site), rows = vars(Grey_patch)) +
  geom_errorbar(
    data = stat_summary,
    aes(x = interaction(Case, Status), ymax = SD_max, ymin = SD_min),
    size = 1,
    width = 0.3,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = stat_summary,
    aes(x = interaction(Case, Status), y = Mean),
    size = 2,
    color = "darkorange",
    inherit.aes = FALSE
  ) +
  labs(
    title = "RGB intensity changes across grey patches distribution values; G0 (Higher intensity), G5 (Lower intensity)",
    x = "Case:Status",
    y = "Log R intensity (0-1)"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

#############################################################################################
path <- "C:/Users/jandr/Downloads/Gray_world_whitebalanced_underwater_vs_air_Data_pixels.csv"
data <- read_csv(path)
data <- data |>
  mutate(
    Grey_patch = factor(Grey_patch),
    Site = factor(Site),
    Status = factor(Status),
    ID = factor(ID)
  ) |>
  dplyr::filter(!Grey_patch %in% c("G0", "G5")) |>
  mutate(Ratio_RG = (R / G), Ratio_RB = (R / B))


data_long <- pivot_longer(
  data,
  cols = c(R, G, B, Ratio_RB, Ratio_RG),
  names_to = "RGB",
  values_to = "Intensity"
)

ggplot(data = data) +
  geom_density(
    aes(x = Ratio_RB, color = Site, fill = Site),
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_grid(rows = vars(Grey_patch), cols = vars(Status)) +
  theme_classic()

data_underwater <- data |> dplyr::filter(Site == "Underwater")

Model_base_underwater <- glmmTMB(
  log(R) ~ Grey_patch + (1 | ID:Status),
  data = data_underwater,
  family = gaussian()
)

summary(Model_base_underwater)
Model_base_underwater_res <- simulateResiduals(Model_base_underwater, n = 3000)
plot(Model_base_underwater_res)

Data_Var <- data.frame(Mean = Prediction$fit, Variance = Prediction$se.fit) |>
  mutate(Site = "Underwater")

Data_prediction_underwater <- Data_Var[1, ]

model_underwater <- glmmTMB(
  log(R) ~ Grey_patch + (1 | ID:Grey_patch:Status),
  data = data_underwater,
  family = gaussian()
)
predict(model_underwater, se.fit = TRUE)

summary(model_underwater)
model_underwater_res <- simulateResiduals(model_underwater, n = 2000)

plot(model_underwater_res)


ranef_underwater <- ranef(model_underwater)$cond$`ID:Grey_patch:Status` |>
  rownames_to_column("ID:Grey_Patch:Status") |>
  separate(
    col = "ID:Grey_Patch:Status",
    into = c("ID", "Grey_patch", "Status"),
    sep = ":"
  ) |>
  rename(Random_effect = '(Intercept)') |>
  mutate(Site = "Underwater")


data_lab <- data |> dplyr::filter(Site == "Lab")


model_base_lab <- glmmTMB(
  log(R) ~ Grey_patch + (1 | ID),
  data = data_lab,
  family = gaussian()
)

summary(model_base_lab)
Model_base_underwater_res_lab <- simulateResiduals(model_base_lab, n = 3000)
plot(Model_base_underwater_res_lab)
prediction_lab <- predict(model_base_lab, se.fit = TRUE)

Data_prediction_lab <- data.frame(
  Mean = prediction_lab$fit,
  Variance = prediction_lab$se.fit
) |>
  mutate(Site = "Lab")

Data_prediction_lab <- Data_prediction_lab[1, ]
Data_prediction <- bind_rows(Data_prediction_lab, Data_prediction_underwater)

model_lab <- glmmTMB(
  log(R) ~ Grey_patch + (1 | ID:Grey_patch:Status),
  data = data_lab,
  family = gaussian()
)

summary(model_lab)
model_lab_res <- simulateResiduals(model_underwater, n = 2000)
plot(model_lab_res)

ranef_lab <- ranef(model_lab)$cond$`ID:Grey_patch:Status` |>
  rownames_to_column("ID:Grey_Patch:Status") |>
  separate(
    col = "ID:Grey_Patch:Status",
    into = c("ID", "Grey_patch", "Status"),
    sep = ":"
  ) |>
  rename(Random_effect = '(Intercept)') |>
  mutate(Site = "Lab")

random_effects <- bind_rows(ranef_underwater, ranef_lab)

ggplot(
  data = random_effects,
  aes(x = Grey_patch, y = Random_effect, color = ID, group = ID)
) +
  geom_hline(
    data = Data_prediction,
    aes(yintercept = Variance),
    linetype = "dashed",
    color = "red"
  ) +
  geom_point() +
  geom_line() +
  facet_grid(cols = vars(Site, Status)) +
  theme_classic()

AIC(Model_base_underwater, model_underwater)
AIC(model_base_lab, model_lab)
