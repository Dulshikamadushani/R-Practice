library(tidyverse)
library(ggpubr)
library(ggrepel)
install.packages("ggpubr")
install.packages("ggrepel")

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sample.data.bac <- read.csv("C:/Users/Dulshika/Downloads/BacterialAlpha.csv", na.strings = "na")
sample.data.bac$Time_Point <- as.factor(sample.data.bac$Time_Point)
sample.data.bac$Crop <- as.factor(sample.data.bac$Crop)
sample.data.bac$Crop <- factor(sample.data.bac$Crop, levels = c("Soil", "Cotton", "Soybean"))

str(sample.data.bac)

bac.even <- ggplot(sample.data.bac, aes(x = Time_Point, y = even, color = Crop)) +
  geom_boxplot(position = position_dodge(0.85)) +
  geom_point(position = position_jitterdodge(0.05)) +
  ylab("Pielou's evenness") +
  xlab("Hours post sowing") + 
  scale_color_manual(values = cbbPalette, name = "", labels = c("Soil no seeds", "Cotton spermosphere", "Soybean spermosphere")) + 
  theme_classic()
bac.even

sample.data.bac.nosoil <- subset(sample.data.bac, Crop != "Soil")

water.imbibed <- ggplot(sample.data.bac.nosoil, aes(Time_Point, 1000 * Water_Imbibed, color = Crop)) + 
  geom_jitter(width = 0.5, alpha = 0.5) + 
  stat_summary(fun = mean, geom = "line", aes(group = Crop)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  xlab("Hours post sowing") +
  ylab("Water Imbibed (mg)") +
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("", "")) + 
  theme_classic() + 
  theme(strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~Crop, scales = "free") 
water.imbibed
  
water.imbibed.cor <- ggplot(sample.data.bac.nosoil, aes(y = even, x = 1000 * Water_Imbibed, color = Crop)) +
  geom_point(aes(shape = Time_Point)) +
  geom_smooth(se = FALSE, method = lm) +
  xlab("Water Imbibed (mg)") +
  ylab("Pielou's evenness") +
  scale_color_manual(values = c(cbbPalette[[2]], cbbPalette[[3]]), name = "", labels = c("Cotton", "Soybean")) + 
  scale_shape_manual(values = c(15, 16, 17, 18), name = "", labels = c("0 hrs", "6 hrs", "12 hrs", "18 hrs")) +
  theme_classic() + 
  theme(strip.background = element_blank(), legend.position = "none") +
  facet_wrap(~Crop, scales = "free")

water.imbibed.cor

figure2 <- ggarrange(
  water.imbibed,  
  bac.even,  
  water.imbibed.cor,  
  labels = "auto",  
  nrow = 3,  
  ncol = 1,  
  legend = FALSE 
)

figure2

bac.even + 
  stat_compare_means(method = "anova") 

bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.format")

bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "p.adj.signif")

bac.even + 
  geom_pwc(aes(group = Crop), method = "t_test", label = "{p.adj.format}{p.adj.signif}")

water.imbibed.cor + 
  stat_cor()

water.imbibed.cor + 
  stat_cor(label.y = 0.7) +
  stat_regline_equation()

diff.abund <- read.csv("diff_abund (1).csv")

str(diff.abund)

diff.abund$log10_pvalue <- -log10(diff.abund$p_CropSoybean)
diff.abund.label <- diff.abund[diff.abund$log10_pvalue > 30,]

ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  theme_classic() + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label))

volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean)) + 
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, color = diff_CropSoybean, label = Label)) + 
  scale_color_manual(values = cbbPalette, name = "Significant") +
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano

volcano <- ggplot() + 
  geom_point(data = diff.abund, aes(x = lfc_CropSoybean, y = log10_pvalue)) + 
  geom_point(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue), color = "red", shape = 17, size = 4) +
  geom_text_repel(data = diff.abund.label, aes(x = lfc_CropSoybean, y = log10_pvalue, label = Label), color = "red") + 
  theme_classic() + 
  xlab("Log fold change Soil vs. Soybean") +
  ylab("-log10 p-value")
volcano

library(lme4)
library(emmeans)
library(multcomp)
library(multcompView)

STAND <- read.csv("raw_data_valent2023_pythium_seedtreatment.csv", na.strings = "na")
ave_stand <- STAND %>%
  filter(days_post_planting != "173 days after planting") %>%
  group_by(Plot, Treatment_name, Rep, days_post_planting) %>%
  dplyr::summarize(
    ave.stand = mean(stand, na.rm=TRUE)) 

lm <- lmer(ave.stand ~ Treatment_name*days_post_planting + (1|Rep), data = ave_stand)
car::Anova(lm)

lsmeans <- emmeans(lm, ~Treatment_name|days_post_planting) # estimate lsmeans of variety within siteXyear
Results_lsmeansEC <- multcomp::cld(lsmeans, alpha = 0.05, reversed = TRUE, details = TRUE,  Letters = letters) # contrast with Tukey ajustment
Results_lsmeansEC

library(emmeans)
library(multcomp)
library(multcompView)

lsmeans <- emmeans(lm, ~ Treatment_name | days_post_planting)

Results_lsmeansEC <- multcomp::cld(
  lsmeans,
  alpha = 0.05,
  Letters = letters,
  adjust = "tukey",
  reversed = TRUE,
  details = TRUE
)

Results_lsmeansEC

sig.diff.letters <- data.frame(Results_lsmeansEC$emmeans$Treatment_name, 
                               Results_lsmeansEC$emmeans$days_post_planting,
                               str_trim(Results_lsmeansEC$emmeans$.group))
colnames(sig.diff.letters) <- c("Treatment_name", 
                                "days_post_planting",
                                "Letters")

# for plotting with letters from significance test
ave_stand2 <- ave_stand %>%
  group_by(Treatment_name, days_post_planting) %>%
  dplyr::summarize(
    ave.stand2 = mean(ave.stand, na.rm=TRUE),
    se = sd(ave.stand)/sqrt(4)) %>%
  left_join(sig.diff.letters) 

ggplot(ave_stand, aes(x = Treatment_name, y = ave.stand)) + 
  stat_summary(fun=mean,geom="bar") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5) +
  ylab("Number of emerged plants") + 
  geom_jitter(width = 0.02, alpha = 0.5) +
  geom_text(data = ave_stand2, aes(label = Letters, y = ave.stand2+(3*se)), vjust = -0.5) +
  xlab("")+
  theme_classic() +
  theme(
    strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid"),
    strip.text.x = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~days_post_planting)




