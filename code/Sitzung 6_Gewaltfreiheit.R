###############################################################################
# Seminar: Protest in Autokratien
# Sitzung 6: Erfolg von Protest - Der Effekt von Gewaltfreiheit 
###############################################################################

# Packages
# (bei erster Benutzung des Packages: vorher installieren über install.packages("NamePackage"))
# install.packages("tidyverse"), install.packages("readxl"), etc.
library(tidyverse)
library(readxl)
library(fixest)
library(modelsummary)

# NAVCO-Daten laden
NAVCO <-read_excel("daten/NAVCO 1.2 Updated.xlsx")
names(NAVCO) <- make.names(names(NAVCO)) #ich schaue die Daten rechts über den Viewer an und stelle fest,d ass manche Variablennamen
# Leerzeichen enthalten. Mit dieser Funktion ersetze ich diese durch einen .

#### DESKRIPTIVE ANALYSEN ####
# Wie viele Campaigns gibt es?
nrow(NAVCO) 

# Wie viele sind violent vs. nonviolent?
NAVCO %>% 
  filter(NONVIOL ==1) %>% 
  nrow()

NAVCO %>% 
  filter(VIOL == 1) %>% 
  nrow()

# Wie viele sind erfolgreich vs. nicht erfolgreich?
NAVCO %>% 
  filter (SUCCESS == 1) %>% 
  nrow()

NAVCO %>% 
  filter (SUCCESS == 0) %>% 
  nrow()

# Vierfeldertafel SUCCESS/NONVIOLENCE
tab <- table(
  Success = factor(NAVCO$SUCCESS, levels = c(0, 1), labels = c("Failure", "Success")),
  Campaign = factor(NAVCO$NONVIOL, levels = c(0, 1), labels = c("Violent", "Nonviolent"))
)

prop.table(tab, margin = 2)

### PLOTS ####
# Balkendiagramm: Success Rate by Campaign Type
plot1 <- NAVCO %>%
  mutate(
    Success = factor(SUCCESS, levels = c(0, 1), labels = c("Failure", "Success")),
    Campaign = factor(NONVIOL, levels = c(0, 1), labels = c("Violent", "Nonviolent"))
  ) %>%
  count(Campaign, Success) %>%
  group_by(Campaign) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Campaign, y = prop, fill = Success)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Success Rate by Campaign Type",
    x = "Campaign Type",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("visualisierungen/06_success rate by campaign type.png", dpi = 300)

plot2 <- # Boxplots: Number of Participants by Campaign Type
NAVCO %>%
  filter(!is.na(PARTICIPATION) & !is.na(NONVIOL)) %>%
  mutate(campaign_type = factor(NONVIOL,
                                levels = c(0, 1),
                                labels = c("Violent", "Nonviolent"))) %>%
  ggplot(aes(x = campaign_type, y = PARTICIPATION, fill = campaign_type)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.alpha = 0.5) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = c("#d9534f", "#534AB7")) +
  labs(
    title = "Number of Participants by Campaign Type",
    x = "",
    y = "Participants (log)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(face = "bold"))

ggsave("visualisierungen/06_number of participants by campaign type.png", dpi = 300)


#Balkendiagramm: Success by Number of Participants
plot3 <- NAVCO %>%
  filter(!is.na(PARTICIPATION) & !is.na(SUCCESS)) %>%
  mutate(participation_cat = ntile(PARTICIPATION, 4),
         participation_cat = factor(participation_cat,
                                    labels = c("Q1 (small)",
                                               "Q2",
                                               "Q3",
                                               "Q4 (large)"))) %>%
  group_by(participation_cat) %>%
  summarise(success_rate = mean(SUCCESS, na.rm = TRUE),
            n = n(),
            min = min(PARTICIPATION),
            max = max(PARTICIPATION)) %>%
  mutate(label = paste0(scales::comma(min), " – ", scales::comma(max))) %>%
  ggplot(aes(x = participation_cat, y = success_rate)) +
  geom_col(fill = "#534AB7") +
  geom_text(aes(label = scales::percent(success_rate, accuracy = 1)),
            vjust = -0.5, size = 3.5) +
  geom_text(aes(label = paste0("n=", n)),
            vjust = 1.5, color = "white", size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Number of Participants and Success",
    x = "Participants (Quantiles Q1-Q4)",
    y = "Success Rate",
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("visualisierungen/06_success rate by number of participants.png", dpi = 300)

### REGRESSION ####
# Kausalpfad Teil 1
model1 <- lm (PARTICIPATION ~ NONVIOL, data = NAVCO) # bivariates Modell: Gewaltfreiheit --> TN-Zahl 
summary(model1)

model1a <- lm (PARTICIPATION ~ NONVIOL + REGVIOL, data = NAVCO) # multivariates Modell: Gewaltfreiheit --> TN-Zahl, kontrolliert auf Repression
summary(model1a)

model1b <- lm (PERCENTAGE.POPULAR.PARTICIPATION ~ NONVIOL + REGVIOL, data = NAVCO) # multivariates Modell, aber AV TN-Zahl als Anteil an Gesamtbevölkerung 
summary(model1b)

model1c <- feols (PERCENTAGE.POPULAR.PARTICIPATION ~ NONVIOL + REGVIOL| LOCATION , data = NAVCO) # mit Country Fixed Effects
summary(model1c)

# Kausalfad Teil 2
model2 <- lm (SUCCESS ~ PARTICIPATION, data = NAVCO) # bivariates Modell: TN-Zahl --> Success
summary(model2)

model2a <- lm (SUCCESS ~ PARTICIPATION + REGVIOL, data = NAVCO) # multivariates Modell: TN-Zahl --> Success, kontrolliert auf Repression
summary(model2a)

model2b <- lm (SUCCESS ~ PERCENTAGE.POPULAR.PARTICIPATION + REGVIOL, data = NAVCO) # multivariates Modell, aber AV TN-Zahl als Anteil an Gesamtbevölkerung 
summary(model2b)

model2c <- feols (SUCCESS ~ PERCENTAGE.POPULAR.PARTICIPATION + REGVIOL| LOCATION , data = NAVCO) # mit Country Fixed Effects
summary(model2c)

# Regressionstabelle exportieren
modelle <- list(model1, model1a, model1b, model1c, model2, model2a, model2b, model2c)

modelsummary(modelle, 
             stars = c('*' = .1, '**' = .05, '***' = .01), 
             output = "visualisierungen/06_regression tables.html", 
             title = "1-2: AV TN-Zahl, 3-4: AV TN-Zahl als Anteil an Gesamtbevölkerung, 5-8: AV Erfolg ",
             fmt = 3,
             coef_rename = c("NONVIOL" = "Gewaltfreie Strategie",
                             "REGVIOL" = "Repression", 
                             "PERCENTAGE.POPULAR.PARTICIPATION"= "TN-Zahl als Anteil an Gesamtbevölkerung",
                             "PARTICIPATION" = "TN-Zahl",
                             "LOCATION" = "Land"
             ),
             gof_map = c("nobs", "r.squared", "FE: LOCATION"), 
)
