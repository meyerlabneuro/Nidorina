#setwd('/Users/hcmeyer/Meyer Lab Boston University/Research/Experiments/Nidorina')
#install,packages("conflicted")
#install.packages("dplyr")
library(conflicted); library(dplyr); library(readxl);library(writexl); library(forcats); library(ggsci); library(patchwork); library(ez); library(rstatix); library(multcomp); library(tidyverse); library(cowplot); library(car)

# Set dplyr::select as the preferred function in case of conflict
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

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
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.9), alpha = 0.5, shape = 21, stroke = 0.7, size = 2) +
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
library('RColorBrewer')
Summation <- read_xlsx('Nido_Summation_Fos.xlsx') %>% 
  filter(!StimType == "Baseline", !StimType == "Inhib") %>% 
  rename(Value = Freezing) %>% rename(Variable = StimType)

#Prelimbic
Prelimbic <- read_xlsx(sheet = 1, 'Fos ratios_Manuscript.xlsx') %>% filter(!is.na(Ratio))
Prelimbic <- Prelimbic %>% rename(Value = Ratio)
PL_performance <- bind_rows(Prelimbic, Summation)
PL_performance_wide <- PL_performance %>% pivot_wider(names_from = Variable, values_from = Value) %>%
  filter(!is.na(Ratio))
#write_xlsx(PL_performance_wide, 'pl_corr.xlsx')

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
#write_xlsx(IL_performance_wide, 'il_corr.xlsx')

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
#write_xlsx(OFC_performance_wide, 'ofc_corr.xlsx')


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
#write_xlsx(RSC_performance_wide, 'rsc_corr.xlsx')

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
#write_xlsx(VH_performance_wide, 'vh_corr.xlsx')

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
write_xlsx(BLA_performance_wide, 'bla_corr.xlsx')

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
AllBrain <- AllBrain %>% rename(Ratio = Value)
AllBrain <- AllBrain %>% select(-Variable) #get rid of column that just says "Ratio" over and over 
#AllBrain <- AllBrain %>% dplyr::select(-Variable) #just in case the above line ever errors out 

#fix from here (gabby tries to fix below)
Summation <- Summation %>% rename( #renaming summation's variable and value since AllBrain also has value and variable columns
  SummationVariable = Variable,
  SummationValue = Value)

AllBrain_performance <- AllBrain %>%
  left_join(Summation, by = c("MouseID", "Age", "Sex"), #if MouseID, Age, Sex match between data from Summation and AllBrain, 
            #add corresponding SummationValue and SummationVariable to appropriate AllBrain row
            relationship = "many-to-many") #this means we expect a ton of different combinations of brain regions and stimtypes

AllBrain_performance_wide <- AllBrain_performance %>% pivot_wider(names_from = SummationVariable, values_from = SummationValue) %>%
select(MouseID, Sex, Age, Ratio, Region, Fear, Compound, Safety, Discrim, Suppression) %>%
  filter(!is.na(Ratio)) #make it wide, drop Na values

#make all brain correlation
AllBrain_corr_build <- data.frame(Ratio=AllBrain_performance_wide$Ratio,
                             Fear=AllBrain_performance_wide$Fear,
                             Compound=AllBrain_performance_wide$Compound,
                             Safety=AllBrain_performance_wide$Safety,
                             Discrim=AllBrain_performance_wide$Discrim,
                             Suppression=AllBrain_performance_wide$Suppression)
AllBrain_corr <- cor(AllBrain_corr_build) # get correlations

# Open a new plotting window since this is BIG graph
plot.new()
dev.off()

#Make pretty matrix table for All Brain
corrplot(AllBrain_corr, method = "color", tl.col = 'black', addCoef.col ='black', number.cex = 0.8, col = brewer.pal(n = 9, name = "YlGn"), 
         title = "Correlation Matrix of AllBrain Performance Measures",
         mar = c(0,0,1,0)) #plot matrix
cor.mtest(AllBrain_corr) #returns p-values in a table

#P-Values 
#function to compute p-values (since not used in the corrplot directly)
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
#function to add asterisks to significant p-values when p-values printed
format_p_values <- function(p.mat, alpha = c(0.001, 0.01, 0.05)) {
  formatted_p.mat <- matrix("", nrow = nrow(p.mat), ncol = ncol(p.mat))
  rownames(formatted_p.mat) <- rownames(p.mat)
  colnames(formatted_p.mat) <- colnames(p.mat)
  
  for (i in 1:nrow(p.mat)) {
    for (j in 1:ncol(p.mat)) {
      if (p.mat[i, j] < alpha[1]) {
        formatted_p.mat[i, j] <- "***"
      } else if (p.mat[i, j] < alpha[2]) {
        formatted_p.mat[i, j] <- "**"
      } else if (p.mat[i, j] < alpha[3]) {
        formatted_p.mat[i, j] <- "*"
      } else {
        formatted_p.mat[i, j] <- ""
      }
    }
  }
  formatted_p.mat
}

# region correlation Get p-values -----------------------------------------------------
AllBrain_pvalues <- cor.mtest(AllBrain_corr)
print(AllBrain_pvalues)
AllBrain_pvalues_stars <- format_p_values(AllBrain_pvalues)
print(AllBrain_pvalues_stars)

PL_pvalues <- cor.mtest(PL_corr)
print(PL_pvalues)
PL_pvalues_stars <- format_p_values(PL_pvalues)
print(PL_pvalues_stars)

IL_pvalues <- cor.mtest(IL_corr)
print(IL_pvalues)
IL_pvalues_stars <- format_p_values(IL_pvalues)
print(IL_pvalues_stars)

OFC_pvalues <- cor.mtest(OFC_corr)
print(OFC_pvalues)
OFC_pvalues_stars <- format_p_values(OFC_pvalues)
print(OFC_pvalues_stars)

RSC_pvalues <- cor.mtest(RSC_corr)
print(RSC_pvalues)
RSC_pvalues_stars <- format_p_values(RSC_pvalues)
print(RSC_pvalues_stars)

VH_pvalues <- cor.mtest(VH_corr)
print(VH_pvalues)
VH_pvalues_stars <- format_p_values(VH_pvalues)
print(VH_pvalues_stars)

BLA_pvalues <- cor.mtest(BLA_corr)
print(BLA_pvalues)
BLA_pvalues_stars <- format_p_values(BLA_pvalues)
print(BLA_pvalues_stars)
# endregion

# PCA for All Brain
library(ggplot2); library(factoextra)

#ensuring the quantitative columns are numeric
AllBrain_performance_wide <- AllBrain_performance_wide %>%
  mutate(across(c(Ratio, Fear, Compound, Safety, Discrim, Suppression), as.numeric))

#select data for PCA
pca_data <- AllBrain_performance_wide %>%
 select(Ratio, Fear, Compound, Safety, Discrim, Suppression)

pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)
summary(pca_result)

#scree plot -> this gives a bar graph with the percentage of variance explained by our 6 dimensions
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
  #according to the rule of proportion of variance - selected PCs should be able to explain at least 80% of variance, 
  #so we can take PC 1, 2, 3 and ignore 4, 5, and 6 and not lose any information! 

#make pca loading plot
loadings <- as.data.frame(pca_result$rotation)

loading_plot <- ggplot(loadings, aes(x = PC1, y = PC2, label = rownames(loadings))) +
  geom_point(color = "blue", size = 3) +
  geom_text(vjust = -0.3, hjust = -0.1, size = 3) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.2, "cm")), color = "cornflowerblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "PCA Loading Plot",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()
print(loading_plot)
  #ratio appears to negatively correlated with discrim
  #ratio is not very likely to be correlated with fear, compound, or suppression
  #there is likely a positive correlation between ratio and safety

#biplot - merge loading plot and simple PCA plot
fviz_pca_biplot(pca_result, geom = "point", label = "var")
  #each point is a sample from our dataset
  #fear compound suppression leaning towards dim2
  #ratio leaning towards dim1

#color by Sex 
fviz_pca_biplot(pca_result, geom.ind = "point", pointshape = 21, 
                col.ind = AllBrain_performance_wide$Sex, 
                palette = c("#00AFBB", "#E7B800"),
                addEllipses = TRUE, label = "var")
  #dim 2 contributes more variance to females than males (fear compound suppression discrimin)
  #dim 1 contributes more variance in males than females (safety, ratio)
  #there is still a big overlap but cool how females have more variance explained in more variables than males 

#color by Age
fviz_pca_biplot(pca_result, geom.ind = "point", pointshape = 21, 
                col.ind = AllBrain_performance_wide$Age, 
                palette = c("#00AFBB", "#E7B800"),
                addEllipses = TRUE, label = "var")
  #dim 1 contributes more variance to adolescents than adults (more variance in safety data)
  #adult variances are explained among everything BUT safety 
