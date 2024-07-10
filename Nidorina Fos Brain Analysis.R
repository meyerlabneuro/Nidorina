#setwd('/Users/hcmeyer/Meyer Lab Boston University/Research/Experiments/Nidorina')
library(readxl); library(forcats); library(ggsci); library(patchwork); library(ez); library(rstatix); library(multcomp); library(tidyverse); library(cowplot); library(car)

brain_stats <- function(x) 
{x %>% summarize(mean = mean(Ratio),
                 sd   = sd(Ratio),
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

#### Analyze by age, sex ####
## Prelimbic
Prelimbic <- read_xlsx(sheet = 1, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio)) %>%
  mutate(Age = fct_relevel(Age, "Adult", "Adolescent"))
Prelimbic_model <- anova_test(data = Prelimbic, dv = Ratio, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Prelimbic_model
PL_ph <- Prelimbic %>% t_test(Ratio ~ Age, var.equal = TRUE, paired = FALSE)
PL_ph

PL_stats <- Prelimbic %>% mutate(Age = fct_relevel(Age, "Adult", "Adolescent")) %>% group_by(Age, Sex) %>% brain_stats

p_PL <- ggplot(PL_stats, aes(x = Age, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Prelimbic, inherit.aes = FALSE,
             aes(x = Age, y = Ratio, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Fos/DAPI (%)", limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  ggtitle("Prelimbic Cortex") +
  theme(legend.position = c(0.38, 1.1))
p_PL #470 x 350

## Infralimbic
Infralimbic <- read_xlsx(sheet = 2, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio)) %>%
  mutate(Age = fct_relevel(Age, "Adult", "Adolescent"))
Infralimbic_model <- anova_test(data = Infralimbic, dv = Ratio, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Infralimbic_model
IL_ph <- Infralimbic %>% t_test(Ratio ~ Age, var.equal = TRUE, paired = FALSE)
IL_ph

IL_stats <- Infralimbic %>% mutate(Age = fct_relevel(Age, "Adult", "Adolescent")) %>% group_by(Sex, Age) %>% brain_stats

p_IL <- ggplot(IL_stats, aes(x = Age, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Infralimbic, inherit.aes = FALSE,
             aes(x = Age, y = Ratio, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Fos/DAPI (%)", limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0)) + 
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  ggtitle("Infralimbic Cortex") +
  theme(legend.position = c(0.38, 1.1))
p_IL

## Orbitofrontal
Orbitofrontal <- read_xlsx(sheet = 3, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio)) %>%
  mutate(Age = fct_relevel(Age, "Adult", "Adolescent"))
Orbitofrontal_model <- anova_test(data = Orbitofrontal, dv = Ratio, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Orbitofrontal_model
OFC_ph <- Orbitofrontal %>% t_test(Ratio ~ Age, var.equal = TRUE, paired = FALSE)
OFC_ph

OFC_stats <- Orbitofrontal %>% mutate(Age = fct_relevel(Age, "Adult", "Adolescent")) %>% group_by(Sex, Age) %>% brain_stats

p_OFC <- ggplot(OFC_stats, aes(x = Age, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Orbitofrontal, inherit.aes = FALSE,
             aes(x = Age, y = Ratio, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Fos/DAPI (%)", limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  ggtitle("Orbitofrontal Cortex") +
  theme(legend.position = c(0.38, 1.1))
p_OFC

## Retrosplenial
Retrosplenial <- read_xlsx(sheet = 4, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio)) %>%
  mutate(Age = fct_relevel(Age, "Adult", "Adolescent"))
Retrosplenial_model <- anova_test(data = Retrosplenial, dv = Ratio, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Retrosplenial_model
RSC_ageph <- Retrosplenial %>% t_test(Ratio ~ Age, var.equal = TRUE, paired = FALSE)
RSC_ageph
RSC_sexph <- Retrosplenial %>% t_test(Ratio ~ Sex, var.equal = TRUE, paired = FALSE)
RSC_sexph

RSC_stats <- Retrosplenial %>% mutate(Age = fct_relevel(Age, "Adult", "Adolescent")) %>% group_by(Sex, Age) %>% brain_stats

p_RSC <- ggplot(RSC_stats, aes(x = Age, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Retrosplenial, inherit.aes = FALSE,
             aes(x = Age, y = Ratio, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Fos/DAPI (%)", limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  ggtitle("Retrosplenial Cortex") +
  theme(legend.position = c(0.38, 1.1))
p_RSC

## Hippocampus
VentralHipp <- read_xlsx(sheet = 5, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio)) %>%
  mutate(Age = fct_relevel(Age, "Adult", "Adolescent"))
VentralHipp_model <- anova_test(data = VentralHipp, dv = Ratio, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
VentralHipp_model
VH_ph <- VentralHipp %>% t_test(Ratio ~ Age, var.equal = TRUE, paired = FALSE)
VH_ph
VH_ph <- VentralHipp %>% t_test(Ratio ~ Sex, var.equal = TRUE, paired = FALSE)
VH_ph

VH_stats <- VentralHipp %>% mutate(Age = fct_relevel(Age, "Adult", "Adolescent")) %>% group_by(Sex, Age) %>% brain_stats

p_VH <- ggplot(VH_stats, aes(x = Age, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = VentralHipp, inherit.aes = FALSE,
             aes(x = Age, y = Ratio, fill = Sex, group = Sex), show.legend = FALSE,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1, size = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Fos/DAPI (%)", limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  ggtitle("Ventral Hippocampus") +
  theme(legend.position = c(0.38, 1.1))
p_VH

## Amygdala
Amygdala <- read_xlsx(sheet = 6, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio)) %>%
  mutate(Age = fct_relevel(Age, "Adult", "Adolescent"))
Amygdala_model <- anova_test(data = Amygdala, dv = Ratio, wid = MouseID, between = c(Age, Sex), effect.size = "pes")
Amygdala_model
AMY_ph <- Amygdala %>% t_test(Ratio ~ Age, var.equal = TRUE, paired = FALSE)
AMY_ph
AMY_ph <- Amygdala %>% t_test(Ratio ~ Sex, var.equal = TRUE, paired = FALSE)
AMY_ph

AMY_stats <- Amygdala %>% mutate(Age = fct_relevel(Age, "Adult", "Adolescent")) %>% group_by(Sex, Age) %>% brain_stats

#Graph 
p_AMY <- ggplot(AMY_stats, aes(x = Age, y = mean, fill = Sex)) +
  geom_col(position = position_dodge2(width = 0.7)) +
  geom_point(data = Amygdala, inherit.aes = FALSE,
             aes(x = Age, y = Ratio, fill = Sex, group = Sex), show.legend = FALSE,
             position = positiopn_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.1, linewidth = 0.7, position = position_dodge(width = 0.9)) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous("Fos/DAPI (%)", limits = c(0, 50), breaks = seq(0, 50, 10), expand = c(0, 0)) +
  scale_fill_manual(name = NULL, values = c("gray25", "gray60")) +
  ggtitle("Basolateral Amygdala") +
  theme(legend.position = c(0.38, 1.1))
p_AMY

#### Analyze by performance ####
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
library('corrplot') #package corrplot
Summation <- read_xlsx('Nido_Summation_Fos.xlsx') %>% 
  filter(!StimType == "Baseline", !StimType == "Inhib") %>% 
  rename(Value = Freezing) %>% rename(Variable = StimType)

#Prelimbic
Prelimbic <- read_xlsx(sheet = 1, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
Prelimbic <- Prelimbic %>% rename(Value = Ratio)
PL_performance <- bind_rows(Prelimbic, Summation)
PL_performance_wide <- PL_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))

PL_corr_build <- data.frame(Ratio=PL_performance_wide$Ratio,
                            Fear=PL_performance_wide$Fear,
                            Compound=PL_performance_wide$Compound,
                            Safety=PL_performance_wide$Safety,
                            Discrim=PL_performance_wide$Discrim,
                            Suppression=PL_performance_wide$Suppression)
PL_corr <- cor(PL_corr_build) # get correlations

corrplot(PL_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = COL1('YlGn')) #plot matrix
cor.mtest(PL_corr) #returns p-values in a table

#Infralimbic
Infralimbic <- read_xlsx(sheet = 2, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
Infralimbic <- Infralimbic %>% rename(Value = Ratio)
IL_performance <- bind_rows(Infralimbic, Summation)
IL_performance_wide <- IL_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))

IL_corr_build <- data.frame(Ratio=IL_performance_wide$Ratio,
                            Fear=IL_performance_wide$Fear,
                            Compound=IL_performance_wide$Compound,
                            Safety=IL_performance_wide$Safety,
                            Discrim=IL_performance_wide$Discrim,
                            Suppression=IL_performance_wide$Suppression)
IL_corr <- cor(IL_corr_build) # get correlations

corrplot(IL_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = COL1('YlGn')) #plot matrix
cor.mtest(IL_corr) #returns p-values in a table

#Orbitofrontal
Orbitofrontal <- read_xlsx(sheet = 3, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
Orbitofrontal <- Orbitofrontal %>% rename(Value = Ratio)
OFC_performance <- bind_rows(Orbitofrontal, Summation)
OFC_performance_wide <- OFC_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))

OFC_corr_build <- data.frame(Ratio=OFC_performance_wide$Ratio,
                             Fear=OFC_performance_wide$Fear,
                             Compound=OFC_performance_wide$Compound,
                             Safety=OFC_performance_wide$Safety,
                             Discrim=OFC_performance_wide$Discrim,
                             Suppression=OFC_performance_wide$Suppression)
OFC_corr <- cor(OFC_corr_build) # get correlations

corrplot(OFC_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = COL1('YlGn')) #plot matrix
cor.mtest(OFC_corr) #returns p-values in a table

#Retrosplenial
Retrosplenial <- read_xlsx(sheet = 4, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
Retrosplenial <- Retrosplenial %>% rename(Value = Ratio)
RSC_performance <- bind_rows(Retrosplenial, Summation)
RSC_performance_wide <- RSC_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))

RSC_corr_build <- data.frame(Ratio=RSC_performance_wide$Ratio,
                            Fear=RSC_performance_wide$Fear,
                            Compound=RSC_performance_wide$Compound,
                            Safety=RSC_performance_wide$Safety,
                            Discrim=RSC_performance_wide$Discrim,
                            Suppression=RSC_performance_wide$Suppression)
RSC_corr <- cor(RSC_corr_build) # get correlations

corrplot(RSC_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = COL1('YlGn')) #plot matrix
cor.mtest(RSC_corr) #returns p-values in a table

#VH
VentralHipp <- read_xlsx(sheet = 5, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
VentralHipp <- VentralHipp %>% rename(Value = Ratio)
VH_performance <- bind_rows(VentralHipp, Summation)
VH_performance_wide <- VH_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))

VH_corr_build <- data.frame(Ratio=VH_performance_wide$Ratio,
                            Fear=VH_performance_wide$Fear,
                            Compound=VH_performance_wide$Compound,
                            Safety=VH_performance_wide$Safety,
                            Discrim=VH_performance_wide$Discrim,
                            Suppression=VH_performance_wide$Suppression)
VH_corr <- cor(VH_corr_build) # get correlations

corrplot(VH_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = COL1('YlGn')) #plot matrix
cor.mtest(VH_corr) #returns p-values in a table

#BLA
Amygdala <- read_xlsx(sheet = 6, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
Amygdala <- Amygdala %>% rename(Value = Ratio)
BLA_performance <- bind_rows(Amygdala, Summation)
BLA_performance_wide <- BLA_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))

BLA_corr_build <- data.frame(Ratio=BLA_performance_wide$Ratio,
                Fear=BLA_performance_wide$Fear,
                Compound=BLA_performance_wide$Compound,
                Safety=BLA_performance_wide$Safety,
                Discrim=BLA_performance_wide$Discrim,
                Suppression=BLA_performance_wide$Suppression)
BLA_corr <- cor(BLA_corr_build) # get correlations

corrplot(BLA_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = COL1('YlGn')) #plot matrix
cor.mtest(BLA_corr) #returns p-values in a table

#All regions
Prelimbic <- Prelimbic %>% mutate(Region = "PL")
Infralimbic <- Infralimbic %>% mutate(Region = "IL")
Orbitofrontal <- Orbitofrontal %>% mutate(Region = "OFC")
Retrosplenial <- Retrosplenial %>% mutate(Region = "RSC")
VentralHipp <- VentralHipp %>% mutate(Region = "VH")
Amygdala <- Amygdala %>% mutate(Region = "BLA")
AllBrain <- bind_rows(Prelimbic, Infralimbic, Orbitofrontal, Retrosplenial, VentralHipp, Amygdala)
AllBrain <- AllBrain %>% rename(Value = Ratio)
#fix from here
AllBrain_performance <- bind_rows(AllBrain, Summation)
AllBrain_performance_wide <- AllBrain_performance %>% pivot_wider(names_from = Variable, values_from = Value) #%>%
select(MouseID:Age, Ratio, Fear, Compound, Safety, Discrim, Suppression) %>%
  filter(!is.na(Ratio))
