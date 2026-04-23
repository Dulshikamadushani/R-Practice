library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpattern)
library(officer)
library(rvg)

# 1. Read Data
df <- read.csv(
  "Data file.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA", "na", "Na", "nA")
)

names(df) <- trimws(names(df))

# 2. Rename Columns
df <- df %>%
  rename(
    sample_no = `Sample No.`,
    sample_depth = `Sample depth`,
    rep = `Reps`,
    ph = `Soil pH`,
    ec = `Soil EC / ?Scm-1`,
    nacl_ip = `NaCl IP mg/kg`,
    nacl_tp = `NaCl TP mg/kg`,
    nacl_org = `NaCl Org P mg/kg`,
    nabd_ip = `NaBD IP mg/kg`,
    nabd_tp = `NaBD TP mg/kg`,
    nabd_org = `NaBD Org P mg/kg`,
    naoh_ip = `NaOH IP mg/kg`,
    naoh_tp = `NaOH TP mg/kg`,
    naoh_org = `NaOH Org P mg/kg`,
    hcl_ip = `HCl IP mg/kg`,
    hcl_tp = `HCl TP mg/kg`,
    hcl_org = `HCl Org P mg/kg`,
    residue = `Residue mg/kg`
  )

# 3. Clean and Process Data
df_clean <- df %>%
  mutate(across(c(sample_no, sample_depth, ph, ec, contains("_"), residue), as.numeric)) %>%
  mutate(rep = as.character(rep)) %>%
  fill(sample_depth, .direction = "down")

# 4. Calculate Percentages
pct_rep <- df_clean %>%
  transmute(
    depth = sample_depth,
    rep = rep,
    nacl_tp = replace_na(nacl_tp, 0),
    nabd_tp = replace_na(nabd_tp, 0),
    naoh_tp = replace_na(naoh_tp, 0),
    hcl_tp = replace_na(hcl_tp, 0),
    residue = replace_na(residue, 0)
  ) %>%
  mutate(
    total_p = nacl_tp + nabd_tp + naoh_tp + hcl_tp + residue,
    nacl_pct = (nacl_tp / total_p) * 100,
    nabd_pct = (nabd_tp / total_p) * 100,
    naoh_pct = (naoh_tp / total_p) * 100,
    hcl_pct = (hcl_tp / total_p) * 100,
    residue_pct = (residue / total_p) * 100
  )

# 5. Renaming panel titles
pct_long <- pct_rep %>%
  select(depth, rep, contains("_pct")) %>%
  pivot_longer(
    cols = contains("_pct"),
    names_to = "fraction",
    values_to = "percent"
  ) %>%
  mutate(
    fraction = factor(
      fraction,
      levels = c("nacl_pct", "nabd_pct", "naoh_pct", "hcl_pct", "residue_pct"),
      labels = c(
        "Loosely sorbed Pt",
        "Redox sensitive Pt",
        "Fe/Al Oxide Pt",
        "Apatatite bound Pt",
        "Residue Pt"
      )
    )
  )

# 6. Summary Statistics
sum_df <- pct_long %>%
  group_by(depth, fraction) %>%
  summarise(
    mean_percent = mean(percent, na.rm = TRUE),
    se_percent = sd(percent, na.rm = TRUE) / sqrt(sum(!is.na(percent))),
    .groups = "drop"
  ) %>%
  group_by(depth) %>%
  arrange(fraction, .by_group = TRUE) %>%
  mutate(xmax = cumsum(mean_percent)) %>%
  ungroup()

# 6A. Calculate Total P (mean across replicates)
totalP_df <- df_clean %>%
  transmute(
    depth = sample_depth,
    total_p = rowSums(across(c(nacl_tp, nabd_tp, naoh_tp, hcl_tp, residue)), na.rm = TRUE)
  ) %>%
  group_by(depth) %>%
  summarise(
    total_p_mean = mean(total_p, na.rm = TRUE),
    .groups = "drop"
  )

# 7. Formatting Depth Labels
depth_order <- sort(unique(sum_df$depth))
depth_key <- data.frame(depth = depth_order, depth_label = as.character(depth_order))

sum_df <- sum_df %>%
  left_join(depth_key, by = "depth") %>%
  mutate(depth_f = factor(depth_label, levels = rev(depth_key$depth_label)))

sum_df <- sum_df %>%
  left_join(totalP_df, by = "depth")

outline_df <- sum_df %>%
  distinct(depth_f) %>%
  mutate(total = 100)

# 7A. Labels for Total P
label_df <- sum_df %>%
  distinct(depth_f, total_p_mean) %>%
  mutate(
    x_label = 102,
    label = sprintf("%.2f", total_p_mean)
  )

# 8. Colors on plot
fill_cols <- c(
  "Loosely sorbed Pt" = "#7B3294",
  "Redox sensitive Pt" = "#50C878",
  "Fe/Al Oxide Pt" = "#d95f02",
  "Apatatite bound Pt" = "#3182BD",
  "Residue Pt" = "#9E9E9E"
)

pattern_vals <- c(
  "Loosely sorbed Pt" = "none",
  "Redox sensitive Pt" = "stripe",
  "Fe/Al Oxide Pt" = "crosshatch",
  "Apatatite bound Pt" = "none",
  "Residue Pt" = "circle"
)

pattern_angle_vals <- c(
  "Loosely sorbed Pt" = 0,
  "Redox sensitive Pt" = 45,
  "Fe/Al Oxide Pt" = 45,
  "Apatatite bound Pt" = 90,
  "Residue Pt" = 45
)

# 9. Create Plot 
p <- ggplot(
  sum_df,
  aes(
    y = depth_f,
    x = mean_percent,
    fill = fraction,
    pattern = fraction,
    pattern_angle = fraction
  )
) +
  geom_col_pattern(
    position = position_stack(reverse = TRUE),
    colour = "black",
    linewidth = 0.6,
    width = 0.82,
    pattern_fill = "grey30",
    pattern_colour = "grey30",
    pattern_density = 0.04,
    pattern_spacing = 0.015,
    pattern_res = 600,
    pattern_key_scale_factor = 0.7
  ) +
  
  # Thick Outer Border
  geom_col(
    data = outline_df,
    aes(x = total, y = depth_f),
    inherit.aes = FALSE,
    fill = NA, colour = "black",
    linewidth = 0.5,
    width = 0.82
  ) +
  
  # Manual subdivision ticks
  annotate(
    "segment",
    x = seq(0, 100, 2),
    xend = seq(0, 100, 2),
    y = 0.5,
    yend = 0.35,
    colour = "black"
  ) +
  
  # Total P values at right side
  geom_text(
    data = label_df,
    aes(x = x_label, y = depth_f, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 8,
    family = "Times New Roman"
  ) +
  
  # Total P column title
  annotate(
    "text",
    x = 102,
    y = length(unique(sum_df$depth_f)) + 0.8,
    label = "P[t] (mg/kg)",
    parse = TRUE,
    hjust = 0,
    size = 8,
    family = "Times New Roman"
  ) +
  
  scale_fill_manual(
    values = fill_cols,
    name = "P fraction",
    labels = c(
      expression("Loosely sorbed " * P[t]),
      expression("Redox sensitive " * P[t]),
      expression("Fe/Al Oxide " * P[t]),
      expression("Apatatite bound " * P[t]),
      expression("Residue " * P[t])
    ),
    guide = guide_legend(
      override.aes = list(
        pattern = unname(pattern_vals),
        pattern_angle = unname(pattern_angle_vals),
        pattern_fill = rep("grey30", length(pattern_vals)),
        pattern_colour = rep("grey30", length(pattern_vals)),
        pattern_density = rep(0.04, length(pattern_vals)),
        pattern_spacing = rep(0.015, length(pattern_vals)),
        colour = rep("black", length(pattern_vals)),
        linewidth = rep(0.6, length(pattern_vals))
      )
    )
  ) +
  scale_pattern_manual(
    values = pattern_vals,
    guide = "none"
  ) +
  scale_pattern_angle_manual(
    values = pattern_angle_vals,
    guide = "none"
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0.01, 0)
  ) +
  
  coord_cartesian(xlim = c(0, 100), clip = "off") +
  labs(
    x = expression(PO[4]^{"3-"} ~ - ~ P ~ fraction ~ percentage ~ "(%)"),
    y = "Sediment Depth (cm)"
  ) +
  theme_bw(base_size = 20, base_family = "Times New Roman") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20),
    axis.ticks = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.3, "cm"),
    
    axis.text = element_text(colour = "black"),
    legend.title = element_text(size=26, colour = "black"),
    legend.text= element_text(size=22),
    legend.key.size = unit(1.2, "cm"),
    legend.key.height = unit(1.2, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.spacing.y = unit(0.2, "cm"),
    legend.position = c(1.3, 0.5),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
    plot.margin = unit(c(0.5, 4, 0.5, 0.5), "cm"),
    aspect.ratio = 1.2
  )

# 10. Save
print(p)

ggsave(
  "P_fraction_proportion.png",
  plot = p, width = 17, height = 12, dpi = 600, bg = "white"
)

# 11. Export editable figure 
ppt <- read_pptx("Figures_editable.pptx")

ppt <- add_slide(ppt, layout = "Blank", master = "Office Theme")

ppt <- ph_with(
  x = ppt,
  value = dml(ggobj = p),
  location = ph_location(left = 0.4, top = 0.4, width = 12.5, height = 8.8)
)

print(ppt, target = "Figures_editable.pptx")