install.packages(c("ggplot2", "dplyr", "readxl", "scales"))
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx") %>%
  filter(!is.na(Weight), !is.na(DATACLASS)) %>%
  mutate(
    Weight = as.numeric(Weight),  # Ensure Weight is numeric
    new_dataclass = case_when(
      DATACLASS == "COMPFLAKE" ~ "Complete flake",
      DATACLASS == "DISTFLAKE" ~ "Distal flake",
      DATACLASS == "MEDFLAKE" ~ "Medial flake",
      DATACLASS == "PROXFLAKE" ~ "Proximal flake",
      DATACLASS %in% c("COMPTOOL", "DISTTOOL", "MEDTOOL", "PROXTOOL") ~ "Tools",
      DATACLASS == "SHATTER" ~ "Shatter",
      DATACLASS %in% c("CORE", "COREFRAG") ~ "Core & Fragment",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(new_dataclass)) %>%
  mutate(new_dataclass = factor(new_dataclass, levels = c(
    "Complete flake", "Distal flake", "Medial flake", "Proximal flake",
    "Tools", "Shatter", "Core & Fragment"
  )))

# 1. Function to define log-scaled breaks dynamically per dataclass
get_log_breaks <- function(data) {
  range_vals <- range(data$Weight, na.rm = TRUE)
  min_val <- max(0.1, range_vals[1])
  max_val <- range_vals[2]
  nice_breaks <- c(0.1, 0.3, 1.0, 3.0, 10.0, 30.0, 100.0, 300.0, 1000.0)
  nice_breaks[nice_breaks >= min_val & nice_breaks <= max_val]
}

# 2. Function for dynamic Y-axis breaks
get_y_breaks <- function(limits) {
  max_val <- max(limits, na.rm = TRUE)
  if (is.na(max_val) || max_val == 0) return(c(0, 1))
  seq(0, max_val, by = max(1, round(max_val / 5)))
}

# 3. Log-Transformed Histogram Plot with Facet Wrap
p_weight_histograms <- ggplot(artifact_data, aes(x = Weight, fill = new_dataclass)) +
  geom_histogram(bins = 55, color = "black") +
  facet_wrap(~ new_dataclass, scales = "free_y") +
  scale_x_log10(
    limits = c(0.1, 1000),
    oob = scales::squish,
    breaks = c(0.1, 0.3, 1, 3, 10, 30, 100, 300, 1000),
    labels = label_number(accuracy = 0.1, trim = FALSE)
  ) +
  scale_y_continuous(
    breaks = function(limits) get_y_breaks(limits)
  ) +
  scale_fill_manual(
    values = c("lightgreen", "pink", "#9370DB", "#4682B4",
               "turquoise", "yellow", "#B8860B")
  ) +
  labs(
    title = "Weight Distribution by Artifact Type (Log Scale)",
    x = "Weight (g)", y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(face = "bold", hjust = 0.5, size = 16),
    axis.title   = element_text(face = "bold", size = 14),
    axis.text    = element_text(size = 10),
    strip.text   = element_text(face = "bold", size = 10),
    legend.position = "none"
  )

print(p_weight_histograms)
ggsave("Weight of Dataclasses.pdf", plot = p_weight_histograms, width = 14, height = 8, dpi = 300)
