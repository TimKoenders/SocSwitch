#### 02_contribution_visualisation.R --------------------------------------------------------
#### Clean-up -----------------------------------------------------------------
rm(list = ls())

#### Packages -----------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(forcats)

#### Output path --------------------------------------------------------------
out_dir <- "figures"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

#### Colors -------------------------------------------------------------------
col_support  <- "#AFC8F5"  # light blue
col_loss     <- "#F4A7A3"  # light red
col_gain     <- "#A9D8A3"  # light green
col_grid     <- "grey88"
col_text     <- "grey20"

#### Theme --------------------------------------------------------------------
theme_contrib <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 13, colour = col_text),
      axis.title = element_text(size = 10, colour = col_text),
      axis.text = element_text(size = 10, colour = col_text),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = col_grid, linewidth = 0.4),
      plot.margin = margin(8, 10, 8, 10)
    )
}

#### Data ---------------------------------------------------------------------
transition_mat <- tibble::tribble(
  ~from,      ~to,       ~value,
  "SPÖ'19",   "SPÖ'24",  10.8,
  "SPÖ'19",   "ÖVP'24",   1.0,
  "SPÖ'19",   "FPÖ'24",   1.2,
  "SPÖ'19",   "GRÜNE'24", 0.5,
  "SPÖ'19",   "NEOS'24",  0.6,
  "SPÖ'19",   "KPÖ'24",   0.3,
  "SPÖ'19",   "OTH'24",   0.2,
  "SPÖ'19",   "NON'24",   1.4,
  
  "ÖVP'19",   "SPÖ'24",   0.6,
  "ÖVP'19",   "ÖVP'24",  16.7,
  "ÖVP'19",   "FPÖ'24",   6.6,
  "ÖVP'19",   "GRÜNE'24", 0.6,
  "ÖVP'19",   "NEOS'24",  1.4,
  "ÖVP'19",   "KPÖ'24",   0.0,
  "ÖVP'19",   "OTH'24",   0.7,
  "ÖVP'19",   "NON'24",   1.6,
  
  "FPÖ'19",   "SPÖ'24",   0.2,
  "FPÖ'19",   "ÖVP'24",   0.4,
  "FPÖ'19",   "FPÖ'24",  11.0,
  "FPÖ'19",   "GRÜNE'24", 0.0,
  "FPÖ'19",   "NEOS'24",  0.1,
  "FPÖ'19",   "KPÖ'24",   0.1,
  "FPÖ'19",   "OTH'24",   0.3,
  "FPÖ'19",   "NON'24",   0.7,
  
  "GRÜNE'19", "SPÖ'24",   2.7,
  "GRÜNE'19", "ÖVP'24",   0.8,
  "GRÜNE'19", "FPÖ'24",   0.3,
  "GRÜNE'19", "GRÜNE'24", 3.9,
  "GRÜNE'19", "NEOS'24",  1.1,
  "GRÜNE'19", "KPÖ'24",   0.3,
  "GRÜNE'19", "OTH'24",   0.4,
  "GRÜNE'19", "NON'24",   0.9,
  
  "NEOS'19",  "SPÖ'24",   0.3,
  "NEOS'19",  "ÖVP'24",   1.0,
  "NEOS'19",  "FPÖ'24",   0.5,
  "NEOS'19",  "GRÜNE'24", 0.3,
  "NEOS'19",  "NEOS'24",  3.1,
  "NEOS'19",  "KPÖ'24",   0.1,
  "NEOS'19",  "OTH'24",   0.2,
  "NEOS'19",  "NON'24",   0.6,
  
  "KPÖ'19",   "SPÖ'24",   0.1,
  "KPÖ'19",   "ÖVP'24",   0.0,
  "KPÖ'19",   "FPÖ'24",   0.0,
  "KPÖ'19",   "GRÜNE'24", 0.0,
  "KPÖ'19",   "NEOS'24",  0.0,
  "KPÖ'19",   "KPÖ'24",   0.2,
  "KPÖ'19",   "OTH'24",   0.0,
  "KPÖ'19",   "NON'24",   0.2,
  
  "OTH'19",   "SPÖ'24",   0.3,
  "OTH'19",   "ÖVP'24",   0.0,
  "OTH'19",   "FPÖ'24",   0.2,
  "OTH'19",   "GRÜNE'24", 0.3,
  "OTH'19",   "NEOS'24",  0.1,
  "OTH'19",   "KPÖ'24",   0.1,
  "OTH'19",   "OTH'24",   0.5,
  "OTH'19",   "NON'24",   0.0,
  
  "NON'19",   "SPÖ'24",   0.9,
  "NON'19",   "ÖVP'24",   1.6,
  "NON'19",   "FPÖ'24",   2.8,
  "NON'19",   "GRÜNE'24", 0.6,
  "NON'19",   "NEOS'24",  0.4,
  "NON'19",   "KPÖ'24",   0.3,
  "NON'19",   "OTH'24",   0.5,
  "NON'19",   "NON'24",  17.3
)

#### Panel A: aggregate SPÖ support -------------------------------------------
panel_a <- tibble(
  year = factor(c("2019", "2024"), levels = c("2019", "2024")),
  value = c(16.2, 15.9)
)

p_a <- ggplot(panel_a, aes(x = year, y = value)) +
  geom_col(width = 0.52, fill = col_support) +
  geom_text(aes(label = sprintf("%.1f", value)), vjust = -0.35, size = 4.1) +
  scale_y_continuous(
    limits = c(0, 18),
    breaks = seq(0, 18, 3),
    expand = expansion(mult = c(0, 0.07))
  ) +
  labs(
    title = "A. Aggregate SPÖ support",
    x = NULL,
    y = "Percent of eligible electorate"
  ) +
  theme_contrib()

#### Panel B: gross defections from SPÖ ---------------------------------------
panel_b <- transition_mat %>%
  dplyr::filter(from == "SPÖ'19", to != "SPÖ'24") %>%
  dplyr::transmute(
    destination = dplyr::case_when(
      to == "FPÖ'24"   ~ "FPÖ",
      to == "ÖVP'24"   ~ "ÖVP",
      to == "GRÜNE'24" ~ "Greens",
      to == "NEOS'24"  ~ "NEOS",
      to == "KPÖ'24"   ~ "KPÖ",
      to == "OTH'24"   ~ "Other",
      to == "NON'24"   ~ "Non-vote"
    ),
    value = value
  ) %>%
  dplyr::arrange(dplyr::desc(value), destination) %>%
  dplyr::mutate(destination = factor(destination, levels = rev(destination)))

p_b <- ggplot(panel_b, aes(x = value, y = destination)) +
  geom_col(width = 0.62, fill = col_loss) +
  geom_text(aes(label = sprintf("%.1f", value)), hjust = -0.15, size = 3.8) +
  scale_x_continuous(
    breaks = c(0, 0.5, 1.0, 1.5),
    expand = expansion(mult = c(0, 0.04))
  ) +
  coord_cartesian(xlim = c(0, 1.6), clip = "off") +
  labs(
    title = "B. Gross defections from SPÖ",
    x = "Percentage points",
    y = NULL
  ) +
  theme_contrib()

#### Panel C: net voter exchanges involving the SPÖ ---------------------------
outgoing <- transition_mat %>%
  dplyr::filter(from == "SPÖ'19", to != "SPÖ'24") %>%
  dplyr::transmute(
    actor = dplyr::case_when(
      to == "FPÖ'24"   ~ "FPÖ",
      to == "ÖVP'24"   ~ "ÖVP",
      to == "GRÜNE'24" ~ "Greens",
      to == "NEOS'24"  ~ "NEOS",
      to == "KPÖ'24"   ~ "KPÖ",
      to == "OTH'24"   ~ "Other",
      to == "NON'24"   ~ "Non-vote"
    ),
    outflow = value
  )

incoming <- transition_mat %>%
  dplyr::filter(to == "SPÖ'24", from != "SPÖ'19") %>%
  dplyr::transmute(
    actor = dplyr::case_when(
      from == "FPÖ'19"   ~ "FPÖ",
      from == "ÖVP'19"   ~ "ÖVP",
      from == "GRÜNE'19" ~ "Greens",
      from == "NEOS'19"  ~ "NEOS",
      from == "KPÖ'19"   ~ "KPÖ",
      from == "OTH'19"   ~ "Other",
      from == "NON'19"   ~ "Non-vote"
    ),
    inflow = value
  )

panel_c <- dplyr::full_join(incoming, outgoing, by = "actor") %>%
  dplyr::mutate(
    inflow = tidyr::replace_na(inflow, 0),
    outflow = tidyr::replace_na(outflow, 0),
    net = inflow - outflow,
    direction = ifelse(net >= 0, "Net gain", "Net loss")
  ) %>%
  dplyr::arrange(net, actor) %>%
  dplyr::mutate(
    actor = factor(actor, levels = rev(actor))
  )

p_c <- ggplot(panel_c, aes(x = net, y = actor, fill = direction)) +
  geom_vline(xintercept = 0, linewidth = 0.6, colour = "grey45") +
  geom_col(width = 0.62) +
  geom_text(
    aes(
      label = sprintf("%+.1f", net),
      hjust = ifelse(net >= 0, -0.15, 1.15)
    ),
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Net loss" = col_loss, "Net gain" = col_gain),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = c(-1, 0, 1, 2),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  coord_cartesian(xlim = c(-1.7, 2.9), clip = "off") +
  labs(
    title = "C. Net voter exchanges involving the SPÖ",
    x = "Net percentage points",
    y = NULL
  ) +
  theme_contrib()


#### Panel D: exchanges between left and non-left blocs -----------------------
left_to_non_left <- 5.2
non_left_to_left <- 2.0
net_exchange <- non_left_to_left - left_to_non_left

panel_d <- tibble(
  flow = c("Left to non-left", "Non-left to left", "Net exchange"),
  value = c(-left_to_non_left, non_left_to_left, net_exchange),
  type = c("Loss", "Gain", "Net")
) %>%
  dplyr::mutate(
    flow = factor(
      flow,
      levels = c("Net exchange", "Non-left to left", "Left to non-left")
    )
  )

p_d <- ggplot(panel_d, aes(x = value, y = flow, fill = type)) +
  geom_vline(xintercept = 0, linewidth = 0.6, colour = "grey45") +
  geom_col(width = 0.62) +
  geom_text(
    aes(
      label = sprintf("%+.1f", value),
      hjust = ifelse(value >= 0, -0.15, 1.15)
    ),
    size = 3.8
  ) +
  scale_fill_manual(
    values = c("Loss" = col_loss, "Gain" = col_gain, "Net" = "grey70"),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(-5, 2, 1),
    expand = expansion(mult = c(0.02, 0.06))
  ) +
  coord_cartesian(xlim = c(-5.5, 2.2), clip = "off") +
  labs(
    title = "D. Exchanges between left and non-left parties",
    x = "Percentage points",
    y = NULL
  ) +
  theme_contrib()

#### Combine: 2 columns, 2 rows -----------------------------------------------
top_row <- p_a + p_b + plot_layout(widths = c(0.95, 1.15))
bottom_row <- p_c + p_d + plot_layout(widths = c(1.10, 1.20))

combined_plot <- top_row / bottom_row + plot_layout(heights = c(1, 1))

#### Display ------------------------------------------------------------------
print(combined_plot)