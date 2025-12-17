###############################################################################
###########################     LIBRARIES     #################################
###############################################################################

library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(emmeans)
library(viridis)

###############################################################################
###########################     1) COLOR — DAY 0 SEED     #####################
###############################################################################

# Load file
df <- read.csv("Color_D0D12D26D3640_Cleaned_forR_v2.csv",
               sep = ",", dec = ",")

# Clean numeric columns
df_clean1 <- df %>%
  mutate(
    L = as.numeric(gsub(",", ".", L)),
    a. = as.numeric(a.),
    b. = as.numeric(gsub(",", ".", b.)),
    Moisture_content = as.numeric(gsub("%", "", Moisture_content_...)),
    MW_duration = MW_duration_.min.
  ) %>%
  mutate(Moisture_content = ifelse(Moisture_content < 1,
                                   Moisture_content * 100,
                                   Moisture_content))

# Filter Day 0 & Seed
df_seed1 <- df_clean1 %>%
  filter(Day == 0,
         Type %in% c("Seed", "Dry seed", "Replicat_dry_seed")) %>%
  mutate(
    MW_duration_plot = case_when(
      Step %in% c("Initial", "After_hydration") ~ "Before MW",
      Step %in% c("MW", "After_drying") & MW_duration %in% c(0,5,10) ~ paste0(MW_duration, " min")
    ),
    MW_duration_plot = factor(MW_duration_plot,
                              levels = c("Before MW", "5 min", "10 min")),
    # change 11->12, 21->20, 39->37
    Moisture_content= case_when(
      Moisture_content == "11" ~ "12%",
      Moisture_content == "21" ~ "20%",
      Moisture_content == "39" ~ "37%",
    ),
    # Rename steps
    Step = case_when(
      Step == "Initial" ~ "Native",
      Step == "After_hydration" ~ "Hydration",
      Step == "MW" ~ "Microwave",
      Step == "After_drying" ~ "Drying"
    ),
    Step = factor(Step, levels = c("Native","Hydration","Microwave","Drying"))
  ) %>%
  filter(!is.na(MW_duration_plot))

# Summary mean & SD
df_summary <- df_seed1 %>%
  group_by(Step, MW_duration_plot, Moisture_content) %>%
  summarise(
    mean_L = mean(L, na.rm = TRUE),
    sd_L   = sd(L, na.rm = TRUE),
    mean_a = mean(a., na.rm = TRUE),
    sd_a   = sd(a., na.rm = TRUE),
    mean_b = mean(b., na.rm = TRUE),
    sd_b   = sd(b., na.rm = TRUE),
    .groups = "drop"
  )

# Plot function
plot_sd_line <- function(df, var_mean, var_sd, var_label) {
  ggplot(df,
         aes(x = Step, y = !!sym(var_mean),
             group = MW_duration_plot,
             color = MW_duration_plot)) +
    geom_errorbar(aes(ymin = !!sym(var_mean) - !!sym(var_sd),
                      ymax = !!sym(var_mean) + !!sym(var_sd)),
                  width = 0.3,
                  position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), linewidth = 0.9) +
    geom_point(size = 1.8, position = position_dodge(width = 0.2)) +
    facet_wrap(~Moisture_content, scales = "fixed") +
    labs(
      title = paste("(c) Seed Color", var_label, "Value"),
      x = "Step",
      y = var_label,
      color = "MW Duration"
    ) + theme_bw()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Generate plots
plot_sd_line(df_summary, "mean_L", "sd_L", "L*")
plot_sd_line(df_summary, "mean_a", "sd_a", "a*")
plot_sd_line(df_summary, "mean_b", "sd_b", "b*")

#export the sheet
library(openxlsx)
write.xlsx(df_summary, file = "df_color_summary.xlsx", sheetName = "Color", rowNames = FALSE)
###############################################################################
###########################     2) COLOR — DAYS FLOUR     #####################
###############################################################################

df_flour <- df_clean1 %>%
  filter(Type == "Flour",
         Day %in% c(0, 12, 26, 36, 40),
         Moisture_content %in% c(11, 21, 39),
         MW_duration %in% c(0, 5, 10)) %>%
  mutate(
    Duration_plot = factor(ifelse(MW_duration == 0, "0 min", paste0(MW_duration, " min")),
                           levels = c("0 min", "5 min", "10 min")),
    Moisture_content= case_when(
      Moisture_content == "11" ~ "12%",
      Moisture_content == "21" ~ "20%",
      Moisture_content == "39" ~ "37%"
  ))

df_summary <- df_flour %>%
  group_by(Day, Moisture_content, Duration_plot) %>%
  summarise(
    L_mean = mean(L, na.rm = TRUE),
    L_sd = sd(L, na.rm = TRUE),
    a_mean = mean(a., na.rm = TRUE),
    a_sd = sd(a., na.rm = TRUE),
    b_mean = mean(b., na.rm = TRUE),
    b_sd = sd(b., na.rm = TRUE)
  ) %>% ungroup()

#export the sheet
library(openxlsx)
write.xlsx(df_summary, file = "df_color_summary.xlsx", sheetName = "COlor", rowNames = FALSE)

plot_nuage <- function(param_mean, param_sd, ylab_text) {
  ggplot(df_summary, aes(x = factor(Day),
                         y = !!sym(param_mean),
                         color = factor(Duration_plot),
                         group = Duration_plot)) +
    geom_point(size = 1.8, position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = !!sym(param_mean) - !!sym(param_sd),
                      ymax = !!sym(param_mean) + !!sym(param_sd)),
                  width = 0.5, position = position_dodge(width = 0.3)) +
    geom_line(position = position_dodge(width = 0.3)) +
    facet_wrap(~ Moisture_content) +
    labs(
      title = paste("(c) Flour Color", ylab_text, "Value"),
      x = "Day",
      y = ylab_text,
      color = "MW Duration"
    ) +
    theme_bw()
}

plot_nuage("L_mean", "L_sd", "L*")
plot_nuage("a_mean", "a_sd", "a*")
plot_nuage("b_mean", "b_sd", "b*")

###### 3D plot for color ##########
###### better to do it across different days ######## 
rgb_colors <- convertColor(df_summary[, c("mean_L","mean_a","mean_b")],
                           from="Lab", to="sRGB", scale.in=100)

library(plotly)
install.packages("plotly")
plot_ly(df_summary, x=~mean_L, y=~mean_a, z=~mean_b,
        type="scatter3d",
        mode="markers",
        marker=list(size=4, color = rgb_colors))
library(plotly)

plot_ly(df_summary,
        x = ~mean_L,
        y = ~mean_a,
        z = ~mean_b,
        type = "scatter3d",
        mode = "markers",
        color = ~Step,             # Color points by 'Step'
        colors = c("red","green"," blue"),  # Optional: choose colors
        marker = list(size = 4)) %>%
  layout(scene = list(
    xaxis = list(title = "L*"),
    yaxis = list(title = "a*"),
    zaxis = list(title = "b*")
  ))

plot_ly(df_summary,
        x = ~mean_L,
        y = ~mean_a,
        z = ~mean_b,
        type = "scatter3d",
        mode = "markers",
        color = ~Moisture_content,             # Color points by 'Step'
        colors = c("red","green"," blue"),  # Optional: choose colors
        marker = list(size = 4)) %>%
  layout(scene = list(
    xaxis = list(title = "L*"),
    yaxis = list(title = "a*"),
    zaxis = list(title = "b*")
  ))

###############################################################################
###########################     3) MOISTURE (%) — DAYS     ###################
###############################################################################

df_moisture <- read.csv("Moisture_D0D12D26D3640_cleaned_forR_v2.csv",
                        sep = ",", dec = ",", stringsAsFactors = FALSE)

df_moisture <- df_moisture %>%
  mutate(
    dry_base = as.numeric(gsub(",", ".", `Dry_Base`)),
    Moisture_content = as.numeric(gsub(",", ".", `Moisture_content_...`)),
    MW_duration = as.numeric(gsub(",", ".", `MW_duration_.min.`))
  )

df_flour_days <- df_moisture %>%
  filter(Type %in% c("Flour","Dry_Fresh_Flour","Replicate_Fresh_Flour"),
         Day %in% c(0, 12, 26, 36, 40),
         Moisture_content %in% c(11, 21, 39),
         MW_duration %in% c(0, 5, 10)) %>%
  mutate(
    Duration_plot = factor(ifelse(MW_duration == 0, "0 min", paste0(MW_duration, " min")),
                           levels = c("0 min", "5 min", "10 min")),
    Moisture_content= case_when(
      Moisture_content == "11" ~ "12%",
      Moisture_content == "21" ~ "20%",
      Moisture_content == "39" ~ "37%"
  ))

df_day_summary <- df_flour_days %>%
  group_by(Day, Moisture_content, Duration_plot) %>%
  summarise(
    mean_val = mean(dry_base, na.rm = TRUE),
    sd_val = sd(dry_base, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_day_summary, aes(x = factor(Day), y = mean_val,
                           color = Duration_plot, group = Duration_plot)) +
  geom_point(size = 1.8) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_val - sd_val,
                    ymax = mean_val + sd_val),
                width = 0.5) +
  facet_wrap(~ Moisture_content, scales = "fixed") +
  labs(
    title = "(a) Flour Water Moisture (d.b.)",
    x = "Day", 
    y = "Water Moisture (d.b.%)",
    color = "MW Duration"
  ) +
  ylim(0,35) +
  theme_bw()

###############################################################################
###########################     4) MOISTURE (%) — STEPS     ##################
###############################################################################

df_flour <- df_moisture %>%
  filter(Type %in% c("Flour","Dry_Fresh_Flour","Replicate_Fresh_Flour"),
         Step %in% c("Initial", "After_hydration", "MW", "After_drying"),
         Moisture_content %in% c(11, 21, 39),
         is.na(MW_duration) | MW_duration %in% c(0, 5, 10)) %>%
  mutate(
    Duration_plot = ifelse(MW_duration == 0, "Before MW",
                           ifelse(is.na(MW_duration), "-", paste0(MW_duration, " min"))),
    Duration_plot = factor(Duration_plot, levels = c("-", "Before MW", "5 min", "10 min")),
    # change 11->12, 21->20, 39->37
    Moisture_content= case_when(
      Moisture_content == "11" ~ "12%",
      Moisture_content == "21" ~ "20%",
      Moisture_content == "39" ~ "37%",
    ),
    Step = case_when(
      Step == "Initial" ~ "Native",
      Step == "After_hydration" ~ "Hydration",
      Step == "MW" ~ "Microwave",
      Step == "After_drying" ~ "Drying"
    ),
    Step = factor(Step, levels = c("Native","Hydration","Microwave","Drying"))
  )

df_flour_summary <- df_flour %>%
  group_by(Step, Duration_plot, Moisture_content) %>%
  summarise(
    mean_val = mean(dry_base, na.rm = TRUE),
    sd_val   = sd(dry_base, na.rm = TRUE),
    .groups = "drop"
  )

plot_moisture_steps <- function(df) {
  ggplot(df, aes(x = Step, y = mean_val,
                 color = Duration_plot, group = Duration_plot)) +
    geom_point(size = 1.8, position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2), linewidth = 0.9) +
    geom_errorbar(aes(ymin = mean_val - sd_val,
                      ymax = mean_val + sd_val),
                  width = 0.3,
                  position = position_dodge(width = 0.2)) +
    facet_wrap(~ Moisture_content, scales = "fixed") +
    labs(
      title = "(a) Flour Water Moisture (d.b.)",
      x = "Step",
      y = "Water moisture (d.b.%)",
      color = "MW Duration"
    ) +
    ylim(0,40) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_moisture_steps(df_flour_summary)

#export the sheet
library(openxlsx)
write.xlsx(df_flour_summary, file = "df_moisture_summary.xlsx", sheetName = "Moisture", rowNames = FALSE)

###############################################################################
###########################     5) WATER ACTIVITY (a_w) — DAYS ###############
###############################################################################

df_aw <- read.csv("Activity_D0D12D26D3640_cleaned_forR_v2.csv",
                  sep = ",", dec = ",", stringsAsFactors = FALSE)

df_aw_flour_days <- df_aw %>%
  filter(Type %in% c("Flour","Dry_Fresh_Flour"),
         Day %in% c(12, 26, 36, 40)) %>%
  rename(
    Moisture_content = Moisture_content_...,
    MW_duration = MW_duration_.min.
  ) %>%
  filter(Moisture_content %in% c(11, 21, 39),
         MW_duration %in% c(0, 5, 10)) %>%
  mutate(
    Original_Result = Result,
    Original_MC = Moisture_content,
    Result = as.numeric(Result),
    Duration_plot = factor(ifelse(MW_duration == 0, "0 min", paste0(MW_duration, " min")),
                           levels = c("0 min", "5 min", "10 min")),
    Moisture_content= case_when(
      Moisture_content == "11" ~ "12%",
      Moisture_content == "21" ~ "20%",
      Moisture_content == "39" ~ "37%")
  )

df_aw_summary <- df_aw_flour_days %>%
  group_by(Day, Moisture_content, Duration_plot) %>%
  summarise(
    mean_aw = mean(Result, na.rm = TRUE),
    sd_aw = sd(Result, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_aw_summary,
       aes(x = factor(Day),
           y = mean_aw,
           color = Duration_plot,
           group = Duration_plot)) +
  geom_point(size = 1.8) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = mean_aw - sd_aw,
                    ymax = mean_aw + sd_aw),
                width = 0.3) +
  facet_wrap(~ Moisture_content, ncol = 3, scales = "fixed") +
  labs(
    title = "(a) Evolution of Flour Water Activity (aw)",
    x = "Day",
    y = "Water Activity (aw)",
    color = "MW Duration"
  ) +
  theme_bw()

###############################################################################
###########################     6) SENSORY ANALYSIS ###########################
###############################################################################

data_sens <- read.csv("Sensory_odor_color_D263640_cleaned_forR.csv",
                      sep = ";", dec = ",", stringsAsFactors = FALSE)

data_sens_clean <- data_sens %>%
  select(Day, MW_duration_.min., Moisture_content_..., Measurement, Score) %>%
  rename(MW = MW_duration_.min.,
         Moisture = Moisture_content_...) %>%
  mutate(Moisture = factor(Moisture),
         MW = factor(MW),
         Day = factor(Day))

# Sensory Color
color_data <- data_sens_clean %>% filter(Measurement == "Sensory_color")
ggplot(color_data, aes(x = Day, y = Score, color = Moisture, group = interaction(Moisture, MW))) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.3)) +
  stat_summary(fun = mean, geom = "line", size = 1, position = position_dodge(width = 0.3)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar",
               width = 0.2, position = position_dodge(width = 0.3)) +
  facet_wrap(~ MW) +
  labs(title = "Evolution of Sensory Color Score ", x = "Day", y = "Score", color = "Moisture (%)") +
  theme_minimal() +
  scale_color_viridis_d()

# Sensory Odor
odor_data <- data_sens_clean %>% filter(Measurement == "Rancid_odor")
ggplot(odor_data, aes(x = Day, y = Score, color = Moisture, group = interaction(Moisture, MW))) +
  stat_summary(fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.3)) +
  stat_summary(fun = mean, geom = "line", size = 1, position = position_dodge(width = 0.3)) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar",
               width = 0.2, position = position_dodge(width = 0.3)) +
  facet_wrap(~ MW) +
  labs(title = "Evolution of Rancid Odor Score", x = "Day", y = "Score", color = "Moisture (%)")+
  theme_minimal() +
  scale_color_viridis_d()

# Statistical Analysis
model_odor <- aov(Score ~ MW * Moisture * Day, data = odor_data)
summary(model_odor)

model_color <- aov(Score ~ MW * Moisture * Day, data = color_data)
summary(model_color)

# Correlation Color ↔ Odor
data_wide <- data_sens_clean %>%
  pivot_wider(names_from = Measurement, values_from = Score, values_fn = mean)

cor_color_odor <- cor(data_wide$Sensory_color, data_wide$Rancid_odor, use = "complete.obs")
print(paste("Correlation color - odor:", round(cor_color_odor, 2)))
