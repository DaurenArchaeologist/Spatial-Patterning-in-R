install.packages(c("spatstat", "spatstat.geom", "spatstat.core", "spatstat.linnet",
                    "readxl", "dplyr", "ggplot2"))
library(spatstat)
library(spatstat.geom)
library(spatstat.core)
library(spatstat.linnet)
library(readxl)
library(dplyr)
library(ggplot2)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx") %>% 
  filter(!is.na(Longitude), !is.na(Latitude))

study_window <- owin(
  xrange = c(78.64065, 78.64306),
  yrange = c(43.31997775, 43.32382)
)
x_breaks <- seq(78.6410, 78.6430, by = 0.0005)
y_breaks <- seq(43.3200, 43.3240, by = 0.0010)

# plot rawmaterial
ppp_rm <- artifact_data %>%
  filter(!is.na(RAWMATERIAL)) %>%
  with(ppp(Longitude, Latitude,
           marks = factor(RAWMATERIAL),
           window = study_window))

seg_res_rm  <- segregation.test(ppp_rm, sigma = bw.ppl, nsim = 99) # spatial segregation
print(seg_res_rm)
ProbRM      <- relrisk(ppp_rm, sigma = bw.ppl) # the relative intensity of each type of point across space
dominant_rm <- im.apply(ProbRM, which.max) #calculate which has the max intensity at each location
dominant_rm <- eval.im(factor(dominant_rm,
                              levels = seq_along(levels(ppp_rm$marks)),
                              labels = levels(ppp_rm$marks)))
rm_df <- as.data.frame(as.im(dominant_rm))
colnames(rm_df) <- c("x","y","material")
cols_rm <- c("goldenrod2","tomato2","forestgreen")
gg_rm <- ggplot(rm_df, aes(x = x, y = y, fill = material)) +
  geom_tile() +
  scale_fill_manual(
    values = cols_rm,
    name   = "Raw Material"
  ) +
  labs(
    title = "Spatial Segregation by Raw Material",
    x     = expression("East" %->% "(Longitude)"),
    y     = expression("North" %->% "(Latitude)")
  ) +
  scale_x_continuous(breaks = x_breaks, labels = sprintf("%.4f", x_breaks)) +
  scale_y_continuous(breaks = y_breaks, labels = sprintf("%.3f", y_breaks)) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title        = element_text(face = "plain", size = 14),
    axis.text         = element_text(size = 10),
    legend.title      = element_text(face = "bold", size = 12),
    legend.text       = element_text(size = 10),
    panel.grid.major  = element_line(color = "gray80", linetype = "solid"),
    panel.grid.minor  = element_blank()
  )
ggsave("Spatial_Segregation_RawMaterial.pdf", gg_rm, width = 10, height = 8, dpi = 300)

# plot dataclasses
artifact_data <- artifact_data %>%
  mutate(DATACLASS_merged = case_when(
    DATACLASS %in% c("CORE","COREFRAG")               ~ "Core & Fragment",
    DATACLASS == "COMPFLAKE"                         ~ "Complete flake",
    DATACLASS == "DISTFLAKE"                         ~ "Distal flake",
    DATACLASS == "MEDFLAKE"                          ~ "Medial flake",
    DATACLASS == "PROXFLAKE"                         ~ "Proximal flake",
    DATACLASS == "SHATTER"                           ~ "Shatter",
    DATACLASS %in% c("COMPTOOL","DISTTOOL","MEDTOOL")~ "Tools",
    TRUE                                             ~ NA_character_
  )) %>%
  filter(!is.na(DATACLASS_merged))

ppp_dc <- with(artifact_data,
               ppp(Longitude, Latitude,
                   marks = factor(DATACLASS_merged,
                                  levels = c("Core & Fragment","Complete flake",
                                             "Distal flake","Medial flake",
                                             "Proximal flake","Shatter","Tools")),
                   window = study_window)
)
seg_res_dc  <- segregation.test(ppp_dc, sigma = bw.ppl, nsim = 99)
print(seg_res_dc)
ProbClass   <- relrisk(ppp_dc, sigma = bw.ppl)
dominant_dc <- im.apply(ProbClass, which.max)
dominant_dc <- eval.im(factor(dominant_dc,
                              levels = 1:7,
                              labels = levels(ppp_dc$marks)))
dc_df <- as.data.frame(as.im(dominant_dc))
colnames(dc_df) <- c("x","y","type")
cols_dc <- c("goldenrod2","forestgreen","skyblue2",
             "tomato2","orchid2","gray60","purple")
gg_dc <- ggplot(dc_df, aes(x = x, y = y, fill = type)) +
  geom_tile() +
  scale_fill_manual(
    values = cols_dc,
    name   = "Artifact Type"
  ) +
  labs(
    title = "Spatial Segregation by Artifact Type",
    x     = expression("East" %->% "(Longitude)"),
    y     = expression("North" %->% "(Latitude)")
  ) +
  scale_x_continuous(breaks = x_breaks, labels = sprintf("%.4f", x_breaks)) +
  scale_y_continuous(breaks = y_breaks, labels = sprintf("%.3f", y_breaks)) +
  coord_fixed() +
  theme_minimal() +
  theme(
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title        = element_text(face = "plain", size = 14),
    axis.text         = element_text(size = 10),
    legend.title      = element_text(face = "bold", size = 12),
    legend.text       = element_text(size = 10),
    panel.grid.major  = element_line(color = "gray80", linetype = "solid"),
    panel.grid.minor  = element_blank()
  )
ggsave("Spatial_Segregation_ArtifactType.pdf", gg_dc, width = 10, height = 8, dpi = 300)
