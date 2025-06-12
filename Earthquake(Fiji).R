install.packages("mapproj")
# Load the quakes dataset
data("quakes")

# Load necessary packages
# If you don't have them, install them first: install.packages(c("maps", "ggplot2"))
library(maps)
library(ggplot2)
# Figure 1: Distribution of Magnitude
ggplot(quakes, aes(x = mag)) +
  geom_histogram(binwidth = 0.1, fill = "#2c7fb8", color = "white", alpha = 0.9) +
  labs(title = "Figure 1: Distribution of Earthquake Magnitude",
       x = "Magnitude (Richter Scale)",
       y = "Frequency") +
  theme_minimal()
# Figure 2: Distribution of Depth
ggplot(quakes, aes(x = depth)) +
  geom_histogram(binwidth = 25, fill = "#f03b20", color = "white", alpha = 0.9) +
  labs(title = "Figure 2: Distribution of Earthquake Depth",
       x = "Depth (km)",
       y = "Frequency") +
  theme_minimal()
# Figure 3: Geographical Scatter Plot
ggplot(quakes, aes(x = long, y = lat)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(title = "Figure 3: Geographical Scatter Plot of Epicenters",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()
# Figure 4: Density Heatmap of Epicenters
ggplot(quakes, aes(x = long, y = lat)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white") +
  scale_fill_viridis_c() +
  labs(title = "Figure 4: Density Heatmap of Seismic Event Epicenters",
       x = "Longitude",
       y = "Latitude",
       fill = "Density") +
  theme_minimal()
# Figure 5: Magnitude vs. Depth
ggplot(quakes, aes(x = depth, y = mag)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Figure 5: Relationship between Magnitude and Depth",
       x = "Depth (km)",
       y = "Magnitude") +
  theme_minimal()
# Figure 6: Magnitude vs. Stations
ggplot(quakes, aes(x = stations, y = mag)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Figure 6: Relationship between Magnitude & Stations",
       x = "Number of Reporting Stations",
       y = "Magnitude") +
  theme_minimal()
library(corrplot)
# Figure 7: Correlogram
cor_matrix=cor(quakes)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black", # Add correlation coefficient
         tl.col = "black", tl.srt = 45, # Text label color and rotation
         diag = FALSE, # Hide correlation of variable with itself
         main = "\n\nFigure 7: Correlogram of Dataset Variables",
         mar=c(0,0,4,0))

# Figure 8 : Basic plot of the earthquake locations
plot(quakes$long, quakes$lat, 
     main = "Figure 8: Locations of Seismic Events Near Fiji",
     xlab = "Longitude", 
     ylab = "Latitude",
     pch = 19, 
     col = "red")

#Figure 9: Bin magnitude
quakes$mag_bin <- cut(quakes$mag, breaks = seq(4, 6.5, by = 0.5))

# Faceted plot with color
ggplot(quakes, aes(x = depth, y = mag, color = mag_bin)) +
  geom_point(alpha = 0.6, size = 2) +
  facet_wrap(~mag_bin) +
  scale_color_manual(
    values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")) +
  labs(
    title = "Figure 9: Faceted Plot: Depth vs Magnitude",
    x = "Depth (km)",
    y = "Magnitude",
    color = "Magnitude Bin"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Optional: remove legend to avoid redundancy

#Figure 10: Requires hexbin package
install.packages("hexbin")
library(hexbin)
ggplot(quakes, aes(x = long, y = lat)) +
  stat_binhex(bins = 30) +
  scale_fill_viridis_c() +
  labs(title = "Figure 10: Hexbin Plot of Epicenters", x = "Longitude", y = "Latitude")

# Load required libraries
library(dplyr)
library(ggplot2)
library(maps)
library(gridExtra)
library(knitr)

# Load dataset
data(quakes)

# 1. Summary Statistics Table
cat("Summary Statistics:\n")
summary_table <- summary(quakes) %>% kable()
print(summary_table)

# 2. Frequency Tables
# Create bins for continuous variables
quakes_binned <- quakes %>%
  mutate(
    depth_bin = cut(depth, breaks = seq(40, 680, by = 40)),
    mag_bin = cut(mag, breaks = seq(4.0, 6.4, by = 0.2)),
    stations_bin = cut(stations, breaks = c(0, 20, 40, 60, 80, 100))
  )

# Frequency tables
cat("\nDepth Frequency Table:\n")
depth_freq <- table(quakes_binned$depth_bin) %>% kable()
print(depth_freq)

cat("\nMagnitude Frequency Table:\n")
mag_freq <- table(quakes_binned$mag_bin) %>% kable()
print(mag_freq)

cat("\nStations Frequency Table:\n")
stations_freq <- table(quakes_binned$stations_bin) %>% kable()
print(stations_freq)

# 3. Plots
# Set theme
theme_set(theme_minimal())

# Histograms
p1 <- ggplot(quakes, aes(x = depth)) +
  geom_histogram(binwidth = 30, fill = "steelblue", color = "white") +
  labs(title = "Depth Distribution", x = "Depth (km)", y = "Count")

p2 <- ggplot(quakes, aes(x = mag)) +
  geom_histogram(binwidth = 0.1, fill = "firebrick", color = "white") +
  labs(title = "Magnitude Distribution", x = "Richter Magnitude", y = "")

p3 <- ggplot(quakes, aes(x = stations)) +
  geom_histogram(binwidth = 10, fill = "darkgreen", color = "white") +
  labs(title = "Stations Reporting", x = "Number of Stations", y = "")

# Boxplots
p4 <- ggplot(quakes, aes(y = depth)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Depth Distribution", y = "Depth (km)", x = "") +
  coord_flip()

p5 <- ggplot(quakes, aes(y = mag)) +
  geom_boxplot(fill = "firebrick") +
  labs(title = "Magnitude Distribution", y = "Richter Magnitude", x = "") +
  coord_flip()

p6 <- ggplot(quakes, aes(y = stations)) +
  geom_boxplot(fill = "darkgreen") +
  labs(title = "Stations Reporting", y = "Number of Stations", x = "") +
  coord_flip()

# Scatter plots
p7 <- ggplot(quakes, aes(x = depth, y = mag)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Depth vs Magnitude", x = "Depth (km)", y = "Magnitude")

p8 <- ggplot(quakes, aes(x = stations, y = mag)) +
  geom_point(alpha = 0.5, color = "purple") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Stations vs Magnitude", x = "Number of Stations", y = "Magnitude")

# Geographic plot
world_map <- map_data("world")
p9 <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgray", color = "white") +
  geom_point(data = quakes, 
             aes(x = long, y = lat, color = mag, size = depth), 
             alpha = 0.6) +
  coord_fixed(xlim = c(165, 190), ylim = c(-40, -10), ratio = 1.5) +
  scale_color_gradient(low = "yellow", high = "red") +
  scale_size_continuous(range = c(1, 5)) +
  labs(title = "Earthquake Locations", 
       x = "Longitude", 
       y = "Latitude",
       color = "Magnitude",
       size = "Depth (km)") +
  theme(legend.position = "bottom")

# Arrange plots
grid.arrange(p1, p2, p3, ncol = 3)
grid.arrange(p4, p5, p6, ncol = 3)
grid.arrange(p7, p8, ncol = 2)
print(p9)
print(p7)
