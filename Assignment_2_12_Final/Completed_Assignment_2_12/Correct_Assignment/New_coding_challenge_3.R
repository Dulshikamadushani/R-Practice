library(tidyverse)
library(ggpubr)
library(rstatix)

toxin.data <- read.csv("MycotoxinData.csv", na.strings = "na")
toxin.data <- toxin.data[!is.na(toxin.data$DON), ]
toxin.data$Treatment <- factor(toxin.data$Treatment)
toxin.data$Cultivar  <- factor(toxin.data$Cultivar)
toxin.data$Treatment <- factor(
  toxin.data$Treatment,
  levels = c("Fg", "Fg + 37", "Fg + 40", "Fg + 70", "NTC")
)
cultivar_cols <- c("orange", "blue")



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


# Q5
make_pwc <- function(yvar) {
  pwc <- rstatix::pairwise_t_test(
    data = dplyr::group_by(toxin.data, Cultivar),
    formula = as.formula(paste(yvar, "~ Treatment")),
    p.adjust.method = "bonferroni"
  )
  pwc <- rstatix::add_significance(pwc, p.col = "p.adj")
  p_txt <- ifelse(
    pwc$p.adj < 0.0001,
    "<0.0001",
    formatC(pwc$p.adj, format = "fg", digits = 3)
  )
  pwc$label <- paste0(p_txt, pwc$p.adj.signif)
  pwc <- rstatix::add_xy_position(pwc, x = "Treatment", step.increase = 0.12)
  
  pwc
}

pwc_DON <- make_pwc("DON")
p_DON_pwc <- p_DON +
  ggpubr::stat_pvalue_manual(
    pwc_DON,
    label = "label",
    tip.length = 0.01,
    hide.ns = FALSE,
    size = 3
  )
pwc_15ADON <- make_pwc("X15ADON")
p_15ADON_pwc <- p_15ADON +
  ggpubr::stat_pvalue_manual(
    pwc_15ADON,
    label = "label",
    tip.length = 0.01,
    hide.ns = FALSE,
    size = 3
  )
pwc_seed <- make_pwc("MassperSeed_mg")
p_seedmass_pwc <- p_seedmass +
  ggpubr::stat_pvalue_manual(
    pwc_seed,
    label = "label",
    tip.length = 0.01,
    hide.ns = FALSE,
    size = 3
  )
fig_ABC_pwc <- ggarrange(
  p_DON_pwc, p_15ADON_pwc, p_seedmass_pwc,
  ncol = 3, nrow = 1,
  labels = c("a", "b", "c"),     
  common.legend = TRUE,
  legend = "top"
)

fig_ABC_pwc










