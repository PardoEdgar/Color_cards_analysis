library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(readxl)

#############################################################################################
path <-"Gray_world_whitebalanced_underwater_vs_air_Data_pixels.csv"

data <- read_csv(path)
data <- data |>
  mutate(
    Grey_patch = factor(Grey_patch),
    Site = factor(Site),
    Status = factor(Status),
    ID = factor(paste0(
      data$ID,
      "_",
      ifelse(data$Status == "Preserved", "P", "D")
    )))|>
  dplyr::filter(!Grey_patch %in% c("G0", "G5")) |>
  mutate(Ratio_RG = (R / G), Ratio_RB = (R / B))|>   mutate(
    Grey_patch = factor(Grey_patch, levels= c("G1","G2","G3","G4"),
    )) 


data_long <- pivot_longer(
  data,
  cols = c(R, G, B, Ratio_RB, Ratio_RG),
  names_to = "RGB",
  values_to = "Intensity"
)

ggplot(data = data) +
  geom_density(
    aes(x = log(R), color = Site, fill = Site),
    linewidth = 1,
    alpha = 0.5
  ) +
  facet_grid(rows = vars(Grey_patch), cols = vars(Status)) +
  theme_classic()

data_underwater <- data |>
  dplyr::filter(Site == "Underwater") 
  
Model_base_underwater <- glmmTMB(
    log(R) ~ Grey_patch + (1 | ID),
    data = data_underwater,
    family = gaussian()
  )

summary(Model_base_underwater)
qqnorm(resid(Model_base_underwater))
qqline(resid(Model_base_underwater))
Model_base_underwater_res <- simulateResiduals(Model_base_underwater, n = 3000)
plot(Model_base_underwater_res)

newdata_underwater <- data_underwater |>
  distinct(Grey_patch, .keep_all = TRUE)


Prediction_underwater <- predict(
  Model_base_underwater,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Data_Var <- data.frame(
  Mean = exp(Prediction_underwater$fit),
  Variance = exp(Prediction_underwater$se.fit)
) |>
  mutate(Site = "Underwater")

Data_prediction_underwater <- Data_Var[1, ]

model_underwater <- glmmTMB(
  log(R) ~ Grey_patch + (1 | ID:Grey_patch),
  data = data_underwater,
  family = gaussian()
)
summary(model_underwater)
model_underwater_res <- simulateResiduals(model_underwater, n = 2000)
qqnorm(resid(model_underwater))
qqline(resid(model_underwater))
plot(model_underwater_res)


ranef_underwater <- ranef(model_underwater)$cond$`ID:Grey_patch` |>
  rownames_to_column("ID:Grey_Patch") |>
  separate(
    col = "ID:Grey_Patch",
    into = c("ID", "Grey_patch"),
    sep = ":"
  ) |>
  rename(Random_effect = '(Intercept)') |>
  mutate(Site = "Underwater")


data_lab <- data |> dplyr::filter(Site == "Lab")


model_base_lab <- glmmTMB(
  log(R) ~ Grey_patch  + (1 | ID),
  data = data_lab,
  family = gaussian()
)

summary(model_base_lab)
Model_base_res_lab <- simulateResiduals(model_base_lab, n = 2000)
qqnorm(resid(model_base_lab))
qqline(resid(model_base_lab))
plot(Model_base_res_lab)


newdata_lab <- data.frame(
  Grey_patch = levels(data_lab$Grey_patch),
  ID = data_underwater$ID[1]
)

Prediction_lab <- predict(
  model_base_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Data_prediction_lab <- data.frame(
  Mean = exp(Prediction_lab$fit),
  Variance = exp(Prediction_lab$se.fit)
) |>
  mutate(Site = "Lab")

Data_prediction_lab <- Data_prediction_lab[1, ]
Data_prediction <- bind_rows(Data_prediction_lab, Data_prediction_underwater)

model_lab <- glmmTMB(
  log(R) ~ Grey_patch + (1 | ID:Grey_patch),
  data = data_lab,
  family = gaussian()
)
qqnorm(resid(model_lab))
qqline(resid(model_lab))
summary(model_lab)

model_lab_res <- simulateResiduals(model_lab, n = 2000)
plot(model_lab_res)

ranef_lab <- ranef(model_lab)$cond$`ID:Grey_patch` |>
  rownames_to_column("ID:Grey_Patch") |>
  separate(
    col = "ID:Grey_Patch",
    into = c("ID", "Grey_patch"),
    sep = ":"
  ) |>
  rename(Random_effect = '(Intercept)') |>
  mutate(Site = "Lab")

random_effects <- bind_rows(ranef_underwater, ranef_lab)
random_effects <- random_effects |>
  mutate(Status = str_split_i(ID, pattern = "_", 2))


ranef_lab_base <- ranef(model_base_lab)$cond$`ID` |>
  rownames_to_column("ID") |>
  rename(Random_effect = '(Intercept)') |>
  mutate(Site = "Lab") |>
  mutate(Status = ifelse(grepl("_D$", ID), "D", "P"))


ranef_underwater_based <- ranef(Model_base_underwater)$cond$`ID` |>
  rownames_to_column("ID") |>
  rename(Random_effect = '(Intercept)') |>
  mutate(Site = "Underwater") |>
  mutate(Status = ifelse(grepl("_D$", ID), "D", "P"))


data_amador <- data |> dplyr::filter(Site =="Amador")
Model_amador <- glmmTMB(log(R) ~ Grey_patch + (1|ID:Grey_patch), data = data_amador, family = gaussian())
qqnorm(resid(Model_amador))
qqline(resid(Model_amador))
Model_amador_res <- simulateResiduals(Model_amador, n=2000)
plot(Model_amador_res)

########Random effects for complexed model
ggplot(data = random_effects, aes(x=Grey_patch, y=(Random_effect), color = ID, group = ID)) +
  geom_point()+
  geom_line()+
  facet_grid(cols = vars(Site, Status)) +
  theme_classic()

ggplot(
  data = random_effects,
  aes(x = Grey_patch, y = exp(Random_effect), color = ID, group = ID)
) +
  geom_hline(
    data = Data_prediction,
    aes(yintercept = Variance, linetype = Site),
    color = "black",
    linewidth = 1.5
  ) +
  scale_colour_viridis_d() +
  geom_point() +
  geom_line(linewidth = 1.5) +
  facet_grid(cols = vars(Site, Status)) +
  geom_hline(
    data = ranef_model_based,
    aes(yintercept = exp(Random_effect), color = ID, linetype = Site),
    linewidth = 1,
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  theme_classic()


AIC(Model_base_underwater, model_underwater)
AIC(model_base_lab, model_lab)

######################################################################
##Underwater
Simple_model_R <- glmmTMB(log(R) ~ Grey_patch + ( 1 | ID), data = data_underwater , family = gaussian())
Simple_model_G <- glmmTMB(log(G) ~ Grey_patch + ( 1 | ID), data = data_underwater , family = gaussian())
Simple_model_B <- glmmTMB(log(B) ~ Grey_patch + ( 1 | ID), data = data_underwater , family = gaussian())

newdata_underwater <- data_underwater |>
  distinct(Grey_patch, .keep_all = TRUE)

Prediction_base_model_underwater_R <- predict(
  Simple_model_R,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_base_model_underwater_G <- predict(
  Simple_model_G,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_base_model_underwater_B <- predict(
  Simple_model_B,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Plot_data_base_model_R <-
  data.frame(Grey_patch = levels(data_underwater$Grey_patch),
    mean = Prediction_base_model_underwater_R$fit,
    SE = Prediction_base_model_underwater_R$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE) |>  mutate(Channel = "R")

Plot_data_base_model_G <-
  data.frame(Grey_patch = levels(data_underwater$Grey_patch),
             mean = Prediction_base_model_underwater_G$fit,
             SE = Prediction_base_model_underwater_G$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE)|>  mutate(Channel = "G")


Plot_data_base_model_B<-
  data.frame(Grey_patch = levels(data_underwater$Grey_patch),
             mean = Prediction_base_model_underwater_B$fit,
             SE = Prediction_base_model_underwater_B$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean +1.96 * SE) |>  mutate(Channel = "B")

Plot_data_base_model_underwater <- bind_rows(Plot_data_base_model_R,Plot_data_base_model_G,Plot_data_base_model_B) |> mutate(Site = "Underwater")

##Laboratory
Simple_model_R_lab <- glmmTMB(log(R) ~ Grey_patch + ( 1 | ID), data = data_lab , family = gaussian())
Simple_model_G_lab  <- glmmTMB(log(G) ~ Grey_patch + ( 1 | ID), data = data_lab , family = gaussian())
Simple_model_B_lab  <- glmmTMB(log(B) ~ Grey_patch + ( 1 | ID), data = data_lab , family = gaussian())

newdata_lab <- data_lab |>
  distinct(Grey_patch, .keep_all = TRUE)

Prediction_base_model_R_lab <- predict(
  Simple_model_R_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_base_model_G_lab <- predict(
  Simple_model_G_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_base_modeL_B_lab <- predict(
  Simple_model_B_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Plot_data_base_model_R_lab <-
  data.frame(Grey_patch = levels(data_lab$Grey_patch),
             mean = Prediction_base_model_R_lab$fit,
             SE = Prediction_base_model_R_lab$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean +1.96 * SE) |>  mutate(Channel = "R")

Plot_data_base_model_G_lab <-
  data.frame(Grey_patch = levels(data_lab$Grey_patch),
             mean = Prediction_base_model_G_lab$fit,
             SE = Prediction_base_model_G_lab$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE)|>  mutate(Channel = "G")


Plot_data_base_model_B_lab<-
  data.frame(Grey_patch = levels(data_lab$Grey_patch),
             mean = Prediction_base_modeL_B_lab$fit,
             SE = Prediction_base_modeL_B_lab$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE) |>  mutate(Channel = "B")

Plot_data_base_model_lab<- bind_rows(Plot_data_base_model_R_lab,Plot_data_base_model_G_lab,Plot_data_base_model_B_lab) |> mutate(Site = "Lab")

Plot_data_base_model_all <- bind_rows(Plot_data_base_model_underwater, Plot_data_base_model_lab)
Plot_data_base_model_all$Channel <- factor(Plot_data_base_model_all$Channel, levels = c("R","G","B"))

colors <- c("darkred", "darkgreen","darkblue")

plot_1 <- ggplot(Plot_data_base_model_all,aes(x= interaction(Channel, Grey_patch), y=exp(mean), color = Channel)) + geom_point() + 
  geom_errorbar(aes(ymin=exp(Min_CI), ymax= exp(Max_Ci), width = 0.2)) +  geom_hline(yintercept = 0.13)+
  scale_color_manual(values = colors)+
  facet_grid(col= vars(Site))+
  theme_classic()+
  theme(legend.position = "top", axis.text.x = (element_text(hjust = 0.9, angle  = 60)))




#########################################################################
#########################################################################
##Underwater
Complex_model_R_underwater <- glmmTMB(log(R) ~ Grey_patch + ( 1 | ID:Grey_patch), data = data_underwater , family = gaussian())
Complex_model_G_underwater  <- glmmTMB(log(G) ~ Grey_patch + ( 1 | ID:Grey_patch), data = data_underwater , family = gaussian())
Complex_model_B_underwater  <- glmmTMB(log(B) ~ Grey_patch + ( 1 | ID:Grey_patch), data = data_underwater , family = gaussian())

newdata_underwater <- data_underwater |>
  distinct(Grey_patch, .keep_all = TRUE)

Prediction_complex_model_underwater_R <- predict(
  Complex_model_R_underwater ,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_complex_model_underwater_G <- predict(
  Complex_model_G_underwater ,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_complex_model_underwater_B <- predict(
  Complex_model_B_underwater ,
  newdata = newdata_underwater,
  re.form = ~0,
  se.fit = TRUE
)

Plot_data_complex_model_R_underwater  <-
  data.frame(Grey_patch = levels(data_underwater$Grey_patch),
             mean = Prediction_complex_model_underwater_R$fit,
             SE = Prediction_complex_model_underwater_R$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE) |>  mutate(Channel = "R")

Plot_data_complex_model_G_underwater  <-
  data.frame(Grey_patch = levels(data_underwater$Grey_patch),
             mean = Prediction_complex_model_underwater_G$fit,
             SE = Prediction_complex_model_underwater_G$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE)|>  mutate(Channel = "G")


Plot_data_complex_model_B_underwater <-
  data.frame(Grey_patch = levels(data_underwater$Grey_patch),
             mean = Prediction_complex_model_underwater_B$fit,
             SE = Prediction_complex_model_underwater_B$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE) |>  mutate(Channel = "B")

Plot_data_complex_model_underwater <- bind_rows(Plot_data_complex_model_R_underwater ,Plot_data_complex_model_G_underwater ,Plot_data_complex_model_B_underwater ) |> mutate(Site = "Underwater")

##Laboratory
Complex_model_R_lab <- glmmTMB(log(R) ~ Grey_patch + ( 1 | ID:Grey_patch), data = data_lab , family = gaussian())
Complex_model_G_lab  <- glmmTMB(log(G) ~ Grey_patch + ( 1 | ID:Grey_patch), data = data_lab , family = gaussian())
Complex_model_B_lab  <- glmmTMB(log(B) ~ Grey_patch + ( 1 | ID:Grey_patch), data = data_lab , family = gaussian())

newdata_lab <- data_lab |>
  distinct(Grey_patch, .keep_all = TRUE)

Prediction_complex_model_R_lab <- predict(
  Complex_model_R_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_complex_model_G_lab <- predict(
  Complex_model_G_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Prediction_complex_modeL_B_lab <- predict(
  Complex_model_B_lab,
  newdata = newdata_lab,
  re.form = ~0,
  se.fit = TRUE
)

Plot_data_complex_model_R_lab <-
  data.frame(Grey_patch = levels(data_lab$Grey_patch),
             mean = Prediction_complex_model_R_lab$fit,
             SE = Prediction_complex_model_R_lab$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE) |>  mutate(Channel = "R")

Plot_data_complex_model_G_lab <-
  data.frame(Grey_patch = levels(data_lab$Grey_patch),
             mean = Prediction_complex_model_G_lab$fit,
             SE = Prediction_complex_model_G_lab$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE)|>  mutate(Channel = "G")


Plot_data_complex_model_B_lab<-
  data.frame(Grey_patch = levels(data_lab$Grey_patch),
             mean = Prediction_complex_modeL_B_lab$fit,
             SE = Prediction_complex_modeL_B_lab$se.fit
  ) |>
  dplyr::mutate(Min_CI = mean - 1.96 * SE, Max_Ci = mean + 1.96 * SE) |>  mutate(Channel = "B")

Plot_data_complex_model_lab<- bind_rows(Plot_data_complex_model_R_lab,Plot_data_complex_model_G_lab,Plot_data_complex_model_B_lab) |> mutate(Site = "Lab")

Plot_data_complex_model_all <- bind_rows(Plot_data_complex_model_underwater, Plot_data_complex_model_lab)
Plot_data_complex_model_all$Channel <- factor(Plot_data_complex_model_all$Channel, levels = c("R","G","B"))

colors <- c("darkred", "darkgreen","darkblue")

plot_2 <-ggplot(Plot_data_complex_model_all,aes(x= interaction(Channel, Grey_patch), y=exp(mean), color = Channel)) + geom_point() + 
  geom_errorbar(aes(ymin=exp(Min_CI), ymax= exp(Max_Ci), width = 0.2)) +
  scale_color_manual(values = colors)+
  facet_grid(col= vars(Site))+
  theme_classic() + theme(legend.position = "top") +
theme(legend.position = "top", axis.text.x = (element_text(hjust = 0.9, angle  = 60)))

plots <- c(plot_1,plot_2)
gridExtra::grid.arrange(grobs = plots , ncol=2)

AIC(Simple_model_R_lab, Complex_model_R_lab, Model_R_more_complex)
AIC(Simple_model_R, Complex_model_R_underwater, Model_R_more_complex)

Model_R_more_complex <- glmmTMB(
  log(R) ~ Grey_patch + Status+
    (1 | ID) +
    (1 | ID:Grey_patch),
  data = data_lab,
  family = gaussian()
)

model_res <-  simulateResiduals(Model_R_more_complex,n=2000)
plot(model_res)


##########################################################
#Verifying function data pixel extraction from color cards in orhomosaics using estracted marker positions as proxy of pixel position in linear images
library(tidyverse)
library(dplyr)
library(gridExtra)
library(glmmTMB)
library(DHARMa)
library(emmeans)

data_test <- read_csv("C:/Users/jandr/Downloads/Data_pixels_test_pixel_selector.csv") |> dplyr::mutate(Source = "GUI")
colnames(data_test)
data_test <- data_test |> dplyr::filter(!data_test$Grey_patch %in% c("G0","G5")) |> mutate(Grey_patch = factor(Grey_patch, levels = c("G1","G2","G3","G4"))) 

plot1 <- ggplot(data_test, aes(x= Grey_patch, y= R, color = Grey_patch)) + geom_boxplot() +
  geom_point() + coord_cartesian(ylim = c(0,0.2))+facet_grid(cols = vars(Filename))+ theme_classic() + 
  labs(title= "Pixel extraction using verified GUI with manual selection in linear images", x = "Grey Patch", y = "R intensity (0,1)") +
  theme(legend.position = "top")

ggplot(data_test, aes(x= R, color = Grey_patch)) + geom_density() + facet_grid(cols = vars(Filename))+ theme_classic()

data_orthos <- read_csv("C:/Users/jandr/Downloads/Photos_names_Portobelo_Lab.csv")|> dplyr::mutate(Source = "Orthomosaic")
colnames(data_orthos)
data_orthos <- data_orthos |> dplyr::filter(!data_orthos$marker %in% c("G0","G5")) |> mutate(Grey_patch = factor(marker, levels = c("G1","G2","G3","G4")))

plot2 <-  ggplot(data_orthos, aes(x=marker , y= R, color = marker)) + geom_boxplot()+ geom_point() + facet_grid(cols = vars(names)) + coord_cartesian(ylim = c(0,0.2)) +
  theme_classic()   + 
  labs(title= "Pixel extraction using markers positions from orthomosaics to linear images", x = "Grey Patch", y = "R intensity (0,1)") +
  theme(legend.position = "top")

ggplot(data_orthos, aes(x= R, color = marker)) + geom_density() + facet_grid(rows = vars(names))+ theme_classic()

grid.arrange(plot1, plot2, ncol=2) 

#Data_test
lm_GUI <- glmmTMB(log(R) ~ Grey_patch + (1|Filename/Grey_patch),dispformula = ~ Grey_patch, data= data_test, family = gaussian())
summary(lm_GUI)


qqnorm(resid(lm_GUI))
qqline(resid(lm_GUI))

res <-simulateResiduals(lm_GUI, n=2000)
plot(res)
testCategorical(res, data_test$Grey_patch)
testCategorical(res, data_test$Filename)

data_for_prediction <- data.frame(Grey_patch = factor(levels(data_test$Grey_patch), levels =  levels(data_test$Grey_patch)), names = data_test$Filename)
Prediction_GUI <- predict(lm_GUI, newdata = data_for_prediction ,se.fit = TRUE, re.form = NA)
Data_plot_GUI <-  data_for_prediction |> mutate(Mean = Prediction_GUI$fit, SE = Prediction_GUI$se.fit) |>  
  mutate(Mean_log = exp(Mean), High_CI = exp(Mean + SE * 1.96), Low_CI = exp(Mean -SE* 1.96)) |> 
  mutate(Type ="GUI")

#Data_test
lm_orthos <- glmmTMB(log(R) ~ Grey_patch + (1|names/Grey_patch), dispformula = ~Grey_patch * names,data= data_orthos, family = gaussian())
summary(lm_orthos)

qqnorm(resid(lm_orthos))
qqline(resid(lm_orthos))

res_orthos <-simulateResiduals(lm_orthos, n=2000)
plot(res_orthos)
testCategorical(res_orthos, data_orthos$Grey_patch)
testCategorical(res_orthos, data_orthos$names)

data_for_prediction <- data.frame(
  Grey_patch = factor(
    levels(data_orthos$Grey_patch),
    levels = levels(data_orthos$Grey_patch)
  ),  names = data_orthos$names

)
Prediction_orthos <- predict(lm_orthos, newdata = data_for_prediction ,se.fit = TRUE, re.form = NA)

Data_plot_orthos <- data_for_prediction |> mutate(Mean = Prediction_orthos$fit, SE = Prediction_orthos$se.fit) |>  
  mutate(Mean_log = exp(Mean),High_CI = exp(Mean + SE * 1.96), Low_CI = exp(Mean - SE *1.96 )) |> mutate(Type = "Orthos")


Data_plot <- bind_rows(Data_plot_GUI, Data_plot_orthos)

ggplot() +
  geom_point(data = Data_plot, aes(x= interaction(Type,Grey_patch,names), y= Mean_log, color = Type)) + 
  geom_errorbar(data = Data_plot, aes(x= interaction(Type,Grey_patch,names), ymin = Low_CI, ymax = High_CI, color = Type), width = 0.5) + 
  theme_classic() + theme(legend.position = "top") + labs(x= "Type, Grey Patch", y="R intensity (0-1)") 



###############################################################################

data_test <- read_csv("C:/Users/jandr/Downloads/Data_pixels_test_pixel_selector.csv") |> dplyr::mutate(Source = "GUI")
colnames(data_test)
data_test <- data_test |> dplyr::filter(!data_test$Grey_patch %in% c("G0","G5")) |> mutate(Grey_patch = factor(Grey_patch, levels = c("G1","G2","G3","G4"))) 

data_orthos <- read_csv("C:/Users/jandr/Downloads/Photos_names_Portobelo_Lab.csv")|> dplyr::mutate(Source = "Orthomosaic")
colnames(data_orthos)
data_orthos <- data_orthos |> dplyr::filter(!data_orthos$marker %in% c("G0","G5")) |> mutate(Grey_patch = factor(marker, levels = c("G1","G2","G3","G4")))

data <- bind_rows(
  data_test   |> transmute(Source, image = Filename, Grey_patch, R),
  data_orthos |> transmute(Source, image = names,    Grey_patch, R))


ggplot(data, aes(x= R, y= Grey_patch, group = interaction(Grey_patch,Source), color = Grey_patch)) +
  geom_boxplot(aes(linetype = Source),position = position_dodge(0.8), width = 0.7, size=0.7)+
  geom_point(position = position_jitterdodge(dodge.width = 0.8), size= 0.2) + coord_cartesian(xlim = c(0,0.2)) + facet_wrap(~image, ncol=2)+ theme_classic() + 
  labs(title= "Pixel extraction using verified GUI with manual selection in linear images", y = "Grey Patch", x= "R intensity") +
  theme(legend.position = "top")

model_1 <- glmmTMB(log(R) ~ Source * Grey_patch + (1 | image/Grey_patch),dispformula = ~ Grey_patch + image, data = data, family = gaussian())
model_2 <- glmmTMB(log(R) ~ Source * Grey_patch + (1 | image/Grey_patch),dispformula = ~ Grey_patch * image, data = data, family = gaussian())
summary(model)
res_model <- simulateResiduals(model, n = 2000)
plot(res_model)
testCategorical(res_model, data$Source)
testCategorical(res_model, data$image)
testCategorical(res_model, data$Grey_patch)

anova(model_1, model_2)

emm <- emmeans(model_2, ~ Source | Grey_patch, type = "response")
Comparison <-contrast(emm, "revpairwise", , adjust = "bonferroni") 
summary(Comparison, infer = TRUE)

emm <- as.data.frame(emm)

ggplot(emm, aes(x = Grey_patch, y= response, linetype  = Source)) + 
  geom_point(position = position_dodge(1)) +  
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),position = position_dodge(1))+
  theme_classic()

Comparison_data <- as.data.frame(
  summary(Comparison, infer = TRUE)
)

ggplot(Comparison_data,aes(x = Grey_patch, y = ratio)) +
  geom_hline(yintercept = 1,linetype = "dashed",color = "black", size= 0.6) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),width = 0.15) +
  theme_classic() +
  labs(x = "Grey patch",y = "Orthomosaic / GUI")

ggplot(data_test, aes(x=x, y=y, color= Filename)) + geom_point(size= 0.6) + theme_classic()

ggplot(data_orthos, aes(x=X_ran_location_pixel, y=y_ran_location_pixel, color= names)) +
  geom_point(size= 0.6) + theme_classic()


ggplot(data_test, aes(x=x, y=y, color= Filename)) + geom_point() + theme_classic()
ggplot(data_orthos, aes(x=X_ran_location_pixel, y=y_ran_location_pixel, color= names)) + geom_point() + theme_classic()

