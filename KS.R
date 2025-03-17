install.packages(c("sf", "raster", "terra", "spatstat.core", "readxl"))
install.packages("ggplot2")
library(sf)
library(raster) 
library(terra)  
library(spatstat.core)
library(readxl)
library(ggplot2)

artifact_data <- read_excel("/Users/kisa/Desktop/Surface Artifacts Full Data Dauren.xlsx")

artifact_sf <- st_as_sf(artifact_data, coords = c("Longitude", "Latitude"), crs = 4326)
elevation_raster <- raster("/Users/kisa/Desktop/map.tif")
artifact_sf <- st_transform(artifact_sf, crs = st_crs(elevation_raster))
artifact_elev <- extract(elevation_raster, st_coordinates(artifact_sf))
set.seed(123)  #  Generate random points for comparison
raster_extent_sf <- st_as_sfc(st_bbox(elevation_raster)) # Convert DEM bounding box to an sf polygon
random_points <- st_sample(raster_extent_sf, size = nrow(artifact_sf)) # Generate random points within this extent
random_elev <- extract(elevation_raster, st_coordinates(random_points))

# Remove NA and infinite values
artifact_elev <- artifact_elev[!is.na(artifact_elev) & is.finite(artifact_elev)]
random_elev <- random_elev[!is.na(random_elev) & is.finite(random_elev)]

# Add small noise to break duplicates
artifact_elev <- artifact_elev + runif(length(artifact_elev), min = -0.01, max = 0.01)
random_elev   <- random_elev + runif(length(random_elev), min = -0.01, max = 0.01)
# Perform KS Test with fluttered values: Compare artifact elevations vs. random elevations
ks_test <- ks.test(artifact_elev, random_elev)
print(ks_test) # D = 0.5991, p-value < 2.2e-16

c
#### FROM EMILY: ####
xmin <- min(artifact_data$Longitude, na.rm = TRUE)
xmax <- max(artifact_data$Longitude, na.rm = TRUE)
ymin <- min(artifact_data$Latitude, na.rm = TRUE)
ymax <- max(artifact_data$Latitude, na.rm = TRUE)
print(c(xmin, xmax, ymin, ymax))
study_window <- owin(xrange = c(78.64065, 78.64306), yrange = c(43.31997775, 43.32382))
artifact_ppp <- ppp(x = artifact_data$Longitude, y = artifact_data$Latitude, marks = NULL, window = study_window)
# Dauren: I decided not to include these plots, they're visibly different from quadrat count so not easy to understand
elev_rpj = projectRaster(elevation_raster, crs = 4326)
elev_crop = crop(elev_rpj, extent(xmin, xmax, ymin, ymax))
plot(elev_crop)
elev_matrix = matrix(elev_crop[], nrow = nrow(elev_crop), ncol = ncol(elev_crop), byrow = T)
elev_matrix = elev_matrix[nrow(elev_matrix):1,]
elev_im = im(elev_matrix, xrange = c(xmin, xmax), yrange = c(ymin, ymax))
plot(elev_im)
#
cdf.test(artifact_ppp, elev_im, test = "ks", model = "Poisson") #this shows that there is dependence on elevation 
pdf("KS.pdf", width=10, height=8)
options(scipen = 10)
par(cex = 1.5)  # Dauren: Increase text size (including legend)
ks_result <- cdf.test(artifact_ppp, elev_im, test = "ks", model = "Poisson")
plot(ks_result, 
     main = "KS Test of Elevation",
     ylab = expression(Cumulative ~ probability),
     xlab = expression(Elevation),
     lwd = 2,
     yaxt = "n" # Dauren: Suppress default y-axis labels
)
axis(2, at = seq(0, 1, by = 0.2), labels = paste0(seq(0, 100, by = 20), "%"), las = 1) # Dauren: Add y-axis with percentage labels
dev.off()
auc(artifact_ppp, elev_im) #this shows that the explanatory power of elevation for the location of artifacts is relatively weak
###########

# Density on Elevation histogram but modified to show frequency only.
pdf("Frequency_by_Elevation.pdf", width = 8, height = 6)
# Compute histogram to get frequency counts (without plotting)
hist_data <- hist(artifact_elev, plot = FALSE, breaks = 20)
# Compute density and scale it to match histogram frequencies
dens <- density(artifact_elev)
dens$y <- dens$y * max(hist_data$counts) / max(dens$y)  # Scale density to histogram counts
# Plot an empty histogram to set the correct y-axis
plot(hist_data$mids, hist_data$counts, type = "n", 
     main = "Artifact Frequency by Elevation", 
     xlab = "Elevation (m)", ylab = "Frequency")
# Overlay the density plot
lines(dens, col = "red", lwd = 2)
# Add legend
legend("topright", legend = "Artifacts", col = "red", lwd = 2)
dev.off()
