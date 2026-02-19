library(tidyverse)   
library(ggpubr)      
library(rstatix)

toxin.data <- read.csv("MycotoxinData.csv", na.strings = "na")
toxin.data <- toxin.data[!is.na(toxin.data$DON), ]
toxin.data$Treatment <- factor(toxin.data$Treatment)
toxin.data$Cultivar  <- factor(toxin.data$Cultivar)

# Q2: Reorder Treatment levels
toxin.data$Treatment <- factor(
  toxin.data$Treatment,
  levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70")
)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cultivar_cols <- cbbPalette[c(2, 3)]


# Q1: Boxplot of DON by Treatment
p_DON <- ggplot(toxin.data, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    shape = 21,
    color = "black",
    size = 2,
    alpha = 0.6,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
  ) +
  scale_fill_manual(values = cultivar_cols) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)

p_DON



# Q3: 15ADON plot
p_15ADON <- ggplot(toxin.data, aes(x = Treatment, y = X15ADON, fill = Cultivar)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    shape = 21,
    color = "black",
    size = 2,
    alpha = 0.6,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
  ) +
  scale_fill_manual(values = cultivar_cols) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)

p_15ADON

# Q3: Seed mass plot
p_seedmass <- ggplot(toxin.data, aes(x = Treatment, y = MassperSeed_mg, fill = Cultivar)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(
    shape = 21,
    color = "black",
    size = 2,
    alpha = 0.6,
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75)
  ) +
  scale_fill_manual(values = cultivar_cols) +
  labs(y = "DON (ppm)", x = "") +
  theme_classic() +
  facet_wrap(~Cultivar)

p_seedmass

# Q4: Combine three figures
fig_ABC <- ggarrange(
  p_DON, p_15ADON, p_seedmass,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  legend = "top"
)

fig_ABC

# What did the common.legend option do?
# common.legend = TRUE makes one shared legend for all three plots,
# instead of repeating a separate legend for each plot.


# Q5: Add pairwise t-test comparisons
p_DON <- p_DON + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_15ADON <- p_15ADON + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_seedmass <- p_seedmass + theme(axis.text.x = element_text(angle = 45, hjust = 1))

don_df <- dplyr::group_by(toxin.data, Cultivar)
pwc_DON <- rstatix::pairwise_t_test(don_df, DON ~ Treatment, p.adjust.method = "bonferroni")
pwc_DON <- rstatix::add_xy_position(pwc_DON, x = "Treatment", step.increase = 0.10)

p_DON_pwc <- p_DON +
  ggpubr::stat_pvalue_manual(
    pwc_DON,
    label = "p.signif",
    tip.length = 0.01,
    hide.ns = TRUE
  )

adon_df <- dplyr::group_by(toxin.data, Cultivar)
pwc_15ADON <- rstatix::pairwise_t_test(adon_df, X15ADON ~ Treatment, p.adjust.method = "bonferroni")
pwc_15ADON <- rstatix::add_xy_position(pwc_15ADON, x = "Treatment", step.increase = 0.10)

p_15ADON_pwc <- p_15ADON +
  ggpubr::stat_pvalue_manual(
    pwc_15ADON,
    label = "p.signif",
    tip.length = 0.01,
    hide.ns = TRUE
  )

seed_df <- dplyr::group_by(toxin.data, Cultivar)
pwc_seed <- rstatix::pairwise_t_test(seed_df, MassperSeed_mg ~ Treatment, p.adjust.method = "bonferroni")
pwc_seed <- rstatix::add_xy_position(pwc_seed, x = "Treatment", step.increase = 0.10)

p_seedmass_pwc <- p_seedmass +
  ggpubr::stat_pvalue_manual(
    pwc_seed,
    label = "p.signif",
    tip.length = 0.01,
    hide.ns = TRUE
  )
fig_ABC_pwc <- ggarrange(
  p_DON_pwc, p_15ADON_pwc, p_seedmass_pwc,
  ncol = 3, nrow = 1,
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  legend = "top"
)

fig_ABC_pwc





