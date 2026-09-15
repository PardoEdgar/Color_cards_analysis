library(tidyverse)
library(glmmTMB)
library(DHARMa)

data <- read_csv("C:/Users/jandr/Downloads/Data_pixels_linear_tiff.csv")
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
