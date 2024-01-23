
###triple check all stats tests - 
#make sure levene's tests are accurately coded (significant results = homogeneity of variance FALSE)
#check that paired or not paired t-tests are accurate
#update stats in manuscript as necessary
#figure out how to wade through novelty results -- start with graph of raw freezing from ALL novelty mice
#then consider only suppression, generalization, and novel cue freezing for subsets


#setwd('/Users/hcmeyer/Meyer Lab Boston University/Research/Experiments/Nidorina')
library(readxl); library(writexl);library(ggplot2); library(forcats); library(ggsci); library(patchwork); library(ez); library(rstatix); library(multcomp); library(tidyverse); library(cowplot); library(car)

fig_stats <- function(x) 
{x %>% summarize(mean = mean(Freezing),
                 sd   = sd(Freezing),
                 n    = n(),
                 sem  = sd / sqrt(n))}

theme_set(
  theme_light() +
    theme(legend.justification = c(1, 1),
          legend.box.margin = margin(t = 5, r = 5, b = 0, l = 0),
          panel.grid = element_blank(),
          text = element_text(size = 22),
          title = element_text(size = 22),
          legend.text = element_text(size = 18),
          axis.text = element_text(size = 18),
          axis.line = element_line(colour = "black", size = 0.3),
          panel.border = element_blank())
)

#
#### Curate data ####
#Acquisition
SS1 <- read_xlsx(sheet = 2, 'Nidorina_masterdoc_MT8.xlsx') #All data is MT8
SS2 <- read_xlsx(sheet = 3, 'Nidorina_masterdoc_MT8.xlsx')
SS3 <- read_xlsx(sheet = 4, 'Nidorina_masterdoc_MT8.xlsx')
SS4 <- read_xlsx(sheet = 5, 'Nidorina_masterdoc_MT8.xlsx')

SS1_long <- SS1 %>%
  mutate(Fear = rowMeans(x = select(.data = SS1, starts_with("Fear")))) %>%
  mutate(Safety = rowMeans(x = select(.data = SS1, starts_with("Safety")))) %>%
  mutate(Discrim = Fear - Safety) %>%
  select(MouseID:Age, Baseline, Fear, Safety, Discrim) %>% mutate(Day = "Day1") %>%
  pivot_longer(cols = Baseline:Discrim, names_to = "StimType", values_to = "Freezing")
SS2_long <- SS2 %>% 
  mutate(Fear = rowMeans(x = select(.data = SS2, starts_with("Fear")))) %>%
  mutate(Safety = rowMeans(x = select(.data = SS2, starts_with("Safety")))) %>%
  mutate(Discrim = Fear - Safety) %>%
  select(MouseID:Age, Baseline, Fear, Safety, Discrim) %>% mutate(Day = "Day2") %>%
  pivot_longer(cols = Baseline:Discrim, names_to = "StimType", values_to = "Freezing")
SS3_long <- SS3 %>% 
  mutate(Fear = rowMeans(x = select(.data = SS3, starts_with("Fear")))) %>%
  mutate(Safety = rowMeans(x = select(.data = SS3, starts_with("Safety")))) %>%
  mutate(Discrim = Fear - Safety) %>%
  select(MouseID:Age, Baseline, Fear, Safety, Discrim) %>% mutate(Day = "Day3") %>%
  pivot_longer(cols = Baseline:Discrim, names_to = "StimType", values_to = "Freezing")
SS4_long <- SS4 %>% 
  mutate(Fear = rowMeans(x = select(.data = SS4, starts_with("Fear")))) %>%
  mutate(Safety = rowMeans(x = select(.data = SS4, starts_with("Safety")))) %>%
  mutate(Discrim = Fear - Safety) %>%
  select(MouseID:Age, Baseline, Fear, Safety, Discrim) %>% mutate(Day = "Day4") %>%
  pivot_longer(cols = Baseline:Discrim, names_to = "StimType", values_to = "Freezing")
Acquisition <- bind_rows(SS1_long, SS2_long, SS3_long, SS4_long)
Acquisition <- Acquisition %>% filter(!is.na(Freezing))
write_xlsx(Acquisition, 'Nido_Acquisition.xlsx')

#curate the data
Summation <- read_xlsx(sheet = 7, 'Nidorina_masterdoc_MT8.xlsx')
Summation_Fos <- Summation %>% 
  filter(!Cohort == "1", !Cohort == "2", !Cohort == "3", !Cohort == "4", !Cohort == "5", !Cohort == "Combo") %>%
  select(MouseID, Sex, Age, Baseline:Safety) %>%
  mutate(Discrim = Fear - Safety) %>%
  mutate(Inhib = Fear - Compound) %>%
  mutate(Suppression = Compound/Fear) %>%
  pivot_longer(cols = Baseline:Suppression, names_to = "StimType", values_to = "Freezing")
write_xlsx(Summation_Fos, 'Nido_Summation_Fos.xlsx')

Summation_Novel <- Summation %>% filter(!is.na(Novel))
Summation_Novel <- Summation_Novel %>%
  select(MouseID, Sex, Age, FearFreq, NovelFreq, Baseline:Novel) %>%
  mutate(Discrim = Fear - Safety) %>%
  mutate(Inhib = Fear - Compound) %>%
  mutate(Suppression = Compound/Fear) %>%
  mutate(Generalization = Summation_Novel$"Novel Compound"/Fear) %>%
  pivot_longer(cols = Baseline:Generalization, names_to = "StimType", values_to = "Freezing")
write_xlsx(Summation_Novel, 'Nido_Summation_Novel.xlsx')

#### demographics ####
pnd_stats <- function(x) 
{x %>% summarize(mean = mean(PND),
                 max = max(PND),
                 min = min(PND),
                 sd   = sd(PND),
                 n    = n(),
                 sem  = sd / sqrt(n))}

Master_DC1 <- read_xlsx(sheet = 2, 'Nidorina_masterdoc_MT8.xlsx')
Master_DC1_Adol <- Master_DC1 %>% filter(Age == "Adolescent")
Adol_age <- Master_DC1_Adol %>% pnd_stats
Master_DC1_Adult <- Master_DC1 %>% filter(Age == "Adult")
Adult_age <- Master_DC1_Adult %>% pnd_stats

#### ACQUISITION CURVES ####
#graph the data
Acquisition <- read_xlsx(sheet = 1, 'Nido_Acquisition.xlsx') #This will include ALL of the mice from the project

Acquisition_sub <- Acquisition %>% filter(Age == "Adult", !StimType == "Baseline", !StimType == "Discrim")
Acquisition_sub_stats <- Acquisition_sub %>% 
  group_by(StimType, Day, Sex) %>% 
  fig_stats %>% 
  unite(Group, c(Sex, StimType), sep = " ", remove = FALSE)

p_Acq <- ggplot(Acquisition_sub_stats, aes(x = Day, y = mean, group = Group)) +
  geom_point(aes(shape = Sex, color = StimType), size = 4, show.legend = FALSE) +
  geom_line(aes(linetype = Sex, color = StimType), lwd = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.05, size = 1, color = "black") +
  scale_shape_manual(values=c(19, 1)) +
  scale_color_manual(values=c("red3", "steelblue1")) +
  scale_x_discrete(name = NULL, labels = (c("Day 1", "Day 2", "Day 3","Day 4"))) +
  scale_y_continuous("Average cued freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0))
p_Acq
#750 x 500 (size of graph - including legend)

#analyze the data (ANOVA)
Acquisition_stats_sub <- Acquisition %>% filter(!StimType == "Baseline", !StimType == "Discrim")
Acquisition_model <- anova_test(data = Acquisition_stats_sub, dv = Freezing, wid = MouseID, between = c(Age, Sex), within = c(Day, StimType), effect.size = "pes")
Acquisition_model

#main effect of age (post hoc)
Age_ph <- Acquisition_stats_sub %>% group_by(Age) %>%   fig_stats

#interaction between age and day (post hoc)
levene_test <- leveneTest(Freezing ~ Age*Day, data = Acquisition_stats_sub) #Levene's test for homogeneity of variances
levene_test
AgeBYDay_ph <- Acquisition_stats_sub %>% group_by(Day) %>% t_test(Freezing ~ Age, var.equal = TRUE, paired = FALSE)
AgeBYDay_ph
AgeBYDay_ph_bonf <- p.adjust(AgeBYDay_ph$p, method = "bonferroni")
AgeBYDay_ph_bonf

#interaction between age and day (post hoc)
levene_test <- leveneTest(Freezing ~ StimType*Day, data = Acquisition_stats_sub) #Levene's test for homogeneity of variances
levene_test
StimTypeBYDay_ph <- Acquisition_stats_sub %>% group_by(Day) %>% t_test(Freezing ~ StimType, var.equal = TRUE, paired = TRUE)
StimTypeBYDay_ph
StimTypeBYDay_bonf <- p.adjust(StimTypeBYDay_ph$p, method = "bonferroni")
StimTypeBYDay_bonf

#main effect of sex (post hoc)
Sex_ph <- Acquisition_stats_sub %>% group_by(Sex) %>%   fig_stats

#interaction between sex and day (post hoc)
levene_test <- leveneTest(Freezing ~ Sex*Day, data = Acquisition_stats_sub) #Levene's test for homogeneity of variances
levene_test
SexBYDay_ph <- Acquisition_stats_sub %>% group_by(Day) %>% t_test(Freezing ~ Sex, var.equal = TRUE, paired = FALSE)
SexBYDay_ph
SexBYDay_ph_bonf <- p.adjust(SexBYDay_ph$p, method = "bonferroni")
SexBYDay_ph_bonf

#interaction between sex and StimType (post hoc)
levene_test <- leveneTest(Freezing ~ Sex*StimType, data = Acquisition_stats_sub) #Levene's test for homogeneity of variances
levene_test
SexBYStimType_ph <- Acquisition_stats_sub %>% group_by(StimType) %>% t_test(Freezing ~ Sex, var.equal = TRUE, paired = FALSE)
SexBYStimType_ph
SexBYStimType_ph_bonf <- p.adjust(SexBYStimType_ph$p, method = "bonferroni")
SexBYStimType_ph_bonf

#interaction between sex and StimType (post hoc)
levene_test <- leveneTest(Freezing ~ Sex*StimType*Day, data = Acquisition_stats_sub) #Levene's test for homogeneity of variances
levene_test
SexBYStimType_ph <- Acquisition_stats_sub %>% group_by(Day, StimType) %>% t_test(Freezing ~ Sex, var.equal = TRUE, paired = FALSE)
SexBYStimType_ph
SexBYStimType_ph_bonf <- p.adjust(SexBYStimType_ph$p, method = "bonferroni")
SexBYStimType_ph_bonf

#### ACQUISITION DISCRIMINATION MAGNITUDE ####
#graph the data
Acquisition_Discrim <- Acquisition %>% filter(StimType == "Discrim")
Acquisition_Discrim_Age <- Acquisition_Discrim %>% filter(Age == "Adult")
Acquisition_Discrim_Age_stats <- Acquisition_Discrim_Age %>% 
  group_by(Day, Sex) %>% fig_stats

p_Acquisition_Discrim <- ggplot(Acquisition_Discrim_Age_stats, aes(x = Day, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Acquisition_Discrim_Age, inherit.aes = FALSE,
             aes(x = Day, y = Freezing, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL, labels = c("Day 1", "Day 2", "Day 3", "Day 4")) +
  scale_y_continuous("Discrimination Index (%)", limits = c(-50, 70), breaks = seq(-50, 70, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  theme(legend.position = c(0.3,1.05))
p_Acquisition_Discrim

#analyze the data
Acquisition_Discrim_model <- anova_test(data = Acquisition_Discrim, dv = Freezing, wid = MouseID, between = c(Age, Sex), within = Day, effect.size = "pes")
Acquisition_Discrim_model

#main effect of sex (post hoc)
Sex_ph <- Acquisition_Discrim %>% group_by(Sex) %>%   fig_stats

#main effect of day (post hoc)
levene_test <- leveneTest(Freezing ~ Day, data = Acquisition_Discrim) #Levene's test for homogeneity of variances
levene_test
Day_ph <- Acquisition_Discrim %>% t_test(Freezing ~ Day, var.equal = FALSE, paired = FALSE)
Day_ph
Day_ph_bonf <- p.adjust(Day_ph$p, method = "bonferroni")
Day_ph_bonf

#marginal interaction between sex and day (post hoc)
levene_test <- leveneTest(Freezing ~ Sex*Day, data = Acquisition_Discrim) #Levene's test for homogeneity of variances
levene_test
SexBYDay_ph <- Acquisition_Discrim %>% group_by(Day) %>% t_test(Freezing ~ Sex, var.equal = FALSE, paired = FALSE)
SexBYDay_ph
SexBYDay_ph_bonf <- p.adjust(SexBYDay_ph$p, method = "bonferroni")
SexBYDay_ph_bonf


#### ACQUISITION BASELINE ####
#graph the data
Acquisition_Baseline <- Acquisition %>% filter(StimType == "Baseline")
Acquisition_Baseline_Age <- Acquisition_Baseline %>% filter(Age == "Adolescent")
Acquisition_Baseline_Age_stats <- Acquisition_Baseline_Age %>% 
  group_by(Day, Sex) %>% fig_stats

p_Acquisition_Baseline <- ggplot(Acquisition_Baseline_Age_stats, aes(x = Day, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Acquisition_Baseline_Age, inherit.aes = FALSE,
             aes(x = Day, y = Freezing, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL, labels = c("Day 1", "Day 2", "Day 3", "Day 4")) +
  scale_y_continuous("Baseline Freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  theme(legend.position = c(0.3,1.05))
p_Acquisition_Baseline

#analyze the data
Acquisition_Baseline_model <- anova_test(data = Acquisition_Baseline, dv = Freezing, wid = MouseID, between = c(Age, Sex), within = Day, effect.size = "pes")
Acquisition_Baseline_model

#main effect of Age (post hoc)
Age_ph <- Acquisition_Baseline %>% group_by(Age) %>% fig_stats

#main effect of Sex (post hoc)
Sex_ph <- Acquisition_Baseline %>% group_by(Sex) %>% fig_stats

#main effect of day (post hoc)
levene_test <- leveneTest(Freezing ~ Day, data = Acquisition_Baseline) #Levene's test for homogeneity of variances
levene_test
Day_ph <- Acquisition_Baseline %>% t_test(Freezing ~ Day, var.equal = TRUE, paired = FALSE)
Day_ph
Day_ph_bonf <- p.adjust(Day_ph$p, method = "bonferroni")
Day_ph_bonf

#interaction between age and day (post hoc)
levene_test <- leveneTest(Freezing ~ Age*Day, data = Acquisition_Baseline) #Levene's test for homogeneity of variances
levene_test
AgeBYDay_ph <- Acquisition_Baseline %>% group_by(Day) %>% t_test(Freezing ~ Age, var.equal = TRUE, paired = FALSE)
AgeBYDay_ph
AgeBYDay_ph_bonf <- p.adjust(AgeBYDay_ph$p, method = "bonferroni")
AgeBYDay_ph_bonf

#marginal interaction between sex and day (post hoc)
levene_test <- leveneTest(Freezing ~ Sex*Day, data = Acquisition_Baseline) #Levene's test for homogeneity of variances
levene_test
SexBYDay_ph <- Acquisition_Baseline %>% group_by(Day) %>% t_test(Freezing ~ Sex, var.equal = TRUE, paired = FALSE)
SexBYDay_ph
SexBYDay_ph_bonf <- p.adjust(SexBYDay_ph$p, method = "bonferroni")
SexBYDay_ph_bonf

##### SUMMATION TEST (Fos subset) ####
Summation_Fos <- read_xlsx(sheet = 1, 'Nido_Summation_Fos.xlsx')

#graph the data
Summation <- Summation_Fos %>% filter(!StimType == "Baseline", !StimType == "Discrim", !StimType == "Inhib", !StimType == "Suppression")
Summation_Age <- Summation %>% filter(Age == "Adolescent") %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety"))
Summation_sub_stats <- Summation_Age %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety")) %>%
  group_by(Sex, StimType) %>% fig_stats

p_Summation <- ggplot(Summation_sub_stats, aes(x = Sex, y = mean, fill = StimType)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Summation_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = StimType, group = StimType), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Average cued freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 1)) +
  scale_fill_manual(name = NULL, values = c("red3", "purple3", "steelblue1")) +
  theme(legend.position = c(1.1,1.05))
p_Summation

#analyze the data
Summation_Fos_model <- anova_test(data = Summation, dv = Freezing, wid = MouseID, between = c(Age, Sex), within = StimType, effect.size = "pes")
Summation_Fos_model

#main effect of sex (post hoc)
Sex_ph <- Summation %>% group_by(Sex) %>%   fig_stats

#main effect of stim type (post hoc)
levene_test <- leveneTest(Freezing ~ StimType, data = Summation) #Levene's test for homogeneity of variances
levene_test
Stim_ph <- Summation %>% t_test(Freezing ~ StimType, var.equal = FALSE, paired = TRUE)
Stim_ph
Stim_ph_bonf <- p.adjust(Stim_ph$p, method = "bonferroni")
Stim_ph_bonf

#interaction between sex and stim type (post hoc)
levene_test <- leveneTest(Freezing ~ Sex*StimType, data = Summation) #Levene's test for homogeneity of variances
levene_test
SexBYStim_ph <- Summation %>% group_by(StimType) %>% t_test(Freezing ~ Sex, var.equal = FALSE, paired = FALSE)
SexBYStim_ph
SexBYStim_ph_bonf <- p.adjust(SexBYStim_ph$p, method = "bonferroni")
SexBYStim_ph_bonf

##### SUMMATION TEST (Fos subset) DISCRIM INDEX ####
#graph the data
Summation_Discrim <- Summation_Fos %>% filter(StimType == "Discrim")
Discrim_Age <- Summation_Discrim %>% filter(Age == "Adult")
Discrim_Age_stats <- Discrim_Age %>% 
  group_by(Sex) %>% fig_stats

p_Discrim <- ggplot(Discrim_Age_stats, aes(x = Sex, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Discrim_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL, labels = c("Female", "Male")) +
  scale_y_continuous("Discrimination Index (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  theme(legend.position = c(0.3,1.05))
p_Discrim

#analyze the data
Discrim_model <- anova_test(data = Summation_Discrim, dv = Freezing, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Discrim_model

##### SUMMATION TEST (Fos subset) SUPRESSION RATIO ####
#graph the data
Summation_Supress <- Summation_Fos %>% filter(StimType == "Suppression")
Supress_Age <- Summation_Supress %>% filter(Age == "Adolescent")
Supress_Age_stats <- Supress_Age %>% 
  group_by(Sex) %>% fig_stats

p_Supress <- ggplot(Supress_Age_stats, aes(x = Sex, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Supress_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL, labels = c("Female", "Male")) +
  scale_y_continuous("Suppression Ratio", limits = c(0, 1), breaks = seq(0, 1, .1), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  theme(legend.position = c(0.3,1.05))
p_Supress

#analyze the data
Supression_model <- anova_test(data = Summation_Supress, dv = Freezing, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Supression_model


##### SUMMATION TEST (Fos subset) BASELINE ####
Summation_Baseline <- Summation_Fos %>% filter(StimType == "Baseline")
#no graph, all values are LOW (<5% freezing)

#analyze the data
Baseline_model <- anova_test(data = Summation_Baseline, dv = Freezing, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Baseline_model

#marginal main effect of age (post hoc)
age_ph <- Summation_Baseline %>% group_by(Age) %>%   fig_stats

##### SUMMATION TEST (Novel subset - all mice) #####
Summation_Novel <- read_xlsx(sheet = 1, 'Nido_Summation_Novel.xlsx')

#graph the data
Novelty <- Summation_Novel %>% 
  filter(!StimType == "Baseline", !StimType == "Discrim", !StimType == "Inhib", 
         !StimType == "Suppression", !StimType == "Generalization") %>%
  select(MouseID, Sex, Age, StimType, Freezing)

Novelty_Age <- Novelty %>% filter(Age == "Adolescent") %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel"))
Novelty_sub_stats <- Novelty_Age %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel")) %>%
  group_by(Sex, StimType) %>% fig_stats

p_Novelty <- ggplot(Novelty_sub_stats, aes(x = Sex, y = mean, fill = StimType)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Novelty_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = StimType, group = StimType), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Average cued freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 1)) +
  scale_fill_manual(name = NULL, values = c("red3", "purple3", "steelblue1", "turquoise", "orange")) +
  theme(legend.position = c(1.1,1.05))
p_Novelty

#analyze the data
Novelty_model <- anova_test(data = Novelty, dv = Freezing, wid = MouseID, between = c(Age, Sex), within = StimType, effect.size = "pes")
Novelty_model

#main effect of stim type (post hoc)
levene_test <- leveneTest(Freezing ~ StimType, data = Novelty) #Levene's test for homogeneity of variances
levene_test
Stim_ph <- Novelty %>% t_test(Freezing ~ StimType, var.equal = TRUE, paired = FALSE)
Stim_ph
Stim_ph_bonf <- p.adjust(Stim_ph$p, method = "bonferroni")
Stim_ph_bonf

##### SUMMATION TEST (Novel subset - Mid-Fear High-Novel mice) #####
Summation_Novel <- read_xlsx(sheet = 1, 'Nido_Summation_Novel.xlsx')

#graph the data
Novelty_midF_highN <- Summation_Novel %>% filter(!StimType == "Baseline", !StimType == "Discrim", !StimType == "Inhib", 
                                      !StimType == "Suppression", !StimType == "Generalization",
                                      !FearFreq == "3kHz", !NovelFreq == "7.5kHz", !NovelFreq == "8kHz")
Novelty_midF_highN_Age <- Novelty_midF_highN %>% filter(Age == "Adolescent") %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel"))
Novelty_midF_highN_sub_stats <- Novelty_midF_highN_Age %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel")) %>%
  group_by(Sex, StimType) %>% fig_stats

p_Novelty_midF_highN <- ggplot(Novelty_midF_highN_sub_stats, aes(x = Sex, y = mean, fill = StimType)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Novelty_midF_highN_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = StimType, group = StimType), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Average cued freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 1)) +
  scale_fill_manual(name = NULL, values = c("red3", "purple3", "steelblue1", "turquoise", "orange")) +
  theme(legend.position = c(1.1,1.05))
p_Novelty_midF_highN

##### SUMMATION TEST (Novel subset - Low-Fear High-Novel mice) #####
Summation_Novel <- read_xlsx(sheet = 1, 'Nido_Summation_Novel.xlsx')

#graph the data
Novelty_lowF_highN <- Summation_Novel %>% filter(!StimType == "Baseline", !StimType == "Discrim", !StimType == "Inhib", 
                                                 !StimType == "Suppression", !StimType == "Generalization",
                                                 !FearFreq == "12kHz", !FearFreq == "13kHz", !NovelFreq == "7.5kHz", !NovelFreq == "8kHz")
Novelty_lowF_highN_Age <- Novelty_lowF_highN %>% filter(Age == "Adult") %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel"))
Novelty_lowF_highN_sub_stats <- Novelty_lowF_highN_Age %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel")) %>%
  group_by(Sex, StimType) %>% fig_stats

p_Novelty_lowF_highN <- ggplot(Novelty_lowF_highN_sub_stats, aes(x = Sex, y = mean, fill = StimType)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Novelty_lowF_highN_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = StimType, group = StimType), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Average cued freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 1)) +
  scale_fill_manual(name = NULL, values = c("red3", "purple3", "steelblue1", "turquoise", "orange")) +
  theme(legend.position = c(1.1,1.05))
p_Novelty_lowF_highN

##### SUMMATION TEST (Novel subset - Mid-Novel mice) #####
Summation_Novel <- read_xlsx(sheet = 1, 'Nido_Summation_Novel.xlsx')

#graph the data
Novelty_midN <- Summation_Novel %>% filter(!StimType == "Baseline", !StimType == "Discrim", !StimType == "Inhib", 
                                                 !StimType == "Suppression", !StimType == "Generalization",
                                                 !NovelFreq == "20kHz", !NovelFreq == "21kHz")
Novelty_midN_Age <- Novelty_midN %>% filter(Age == "Adolescent") %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel"))
Novelty_midN_sub_stats <- Novelty_midN_Age %>%
  mutate(StimType = fct_relevel(StimType, "Fear", "Compound", "Safety", "Novel Compound", "Novel")) %>%
  group_by(Sex, StimType) %>% fig_stats

p_Novelty_midN <- ggplot(Novelty_midN_sub_stats, aes(x = Sex, y = mean, fill = StimType)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Novelty_midN_Age, inherit.aes = FALSE,
             aes(x = Sex, y = Freezing, fill = StimType, group = StimType), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 1, size = 3) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem),
                width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Average cued freezing (%)", limits = c(0, 100), breaks = seq(0, 100, 10), expand = c(0, 1)) +
  scale_fill_manual(name = NULL, values = c("red3", "purple3", "steelblue1", "turquoise", "orange")) +
  theme(legend.position = c(1.1,1.05))
p_Novelty_midN
