# Part 1. Plot study area map
library(ggplot2)
library(sf)
library(raster)
library(tmap)
library(viridis)
library(RColorBrewer)
library(classInt)

# 1. Read study area shapefiles
boundary <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/bjhhgd.shp")
admin_boundaries <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/hhgd.shp")
water <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/river.shp")
survey_points <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/vallage55.shp")
china_province <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/china_province.shp")
china_city <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/china_city.shp")

# 2. Transform all shapefiles to WGS84 (EPSG:4326)
boundary <- st_transform(boundary, crs = 4326)
admin_boundaries <- st_transform(admin_boundaries, crs = 4326)
water <- st_transform(water, crs = 4326)
survey_points <- st_transform(survey_points, crs = 4326)
china_province <- st_transform(china_province, crs = 4326)
china_city <- st_transform(china_city, crs = 4326)

# 3. Create buffered extent of study area boundary for basemap
sf::sf_use_s2(FALSE)

# Step 1: Get bounding box
bbox <- st_bbox(boundary)

# Step 2: Expand bbox by a fixed margin
bbox_expanded <- bbox
bbox_expanded["xmin"] <- bbox["xmin"] - 0.2
bbox_expanded["xmax"] <- bbox["xmax"] + 0.2
bbox_expanded["ymin"] <- bbox["ymin"] - 0.3
bbox_expanded["ymax"] <- bbox["ymax"] + 0.2

# Step 3: Convert expanded bbox to sf polygon
boundary_rect_buffered <- st_as_sfc(bbox_expanded)

# Step 4: Fix invalid geometries for province/city layers
china_province_valid <- st_make_valid(china_province)
china_city_valid <- st_make_valid(china_city)

# Step 5: Clip to buffered extent
china_province_cropped <- st_intersection(china_province_valid, boundary_rect_buffered)
china_city_cropped <- st_intersection(china_city_valid, boundary_rect_buffered)

# 4. Convert 'parts' field of survey points to factor
survey_points$parts <- factor(survey_points$parts, levels = c(1, 2, 3))

# 5. Read background raster (e.g., NDVI)
background <- raster("H:/science/F5-program/F3-Assessment/f2-figures/f1-overview/yuanshi/NDVI.tif")

# 6. Project raster to WGS84 (EPSG:4326)
background <- projectRaster(background, crs = CRS("+init=epsg:4326"))

# 7. Extract raster values
background_values <- values(background)

# 8. Define breaks: linear stretch and natural breaks
min_value <- min(background_values, na.rm = TRUE)
max_value <- max(background_values, na.rm = TRUE)
breaks_linear <- seq(min_value, max_value, by = 25)

# 9. Choose style ('linear' or 'natural')
map_style <- "linear"

# Select breaks according to style
if (map_style == "linear") {
  breaks <- breaks_linear
  style <- "cont"
} else if (map_style == "natural") {
  breaks <- breaks_natural
  style <- "fixed"
}

# 10. Plot study area map
map <- 
  tm_shape(china_province_cropped) +
  tm_fill(col = "gray98") +
  tm_borders(col = "gray30", lwd = 2, lty = "solid") +
  
  tm_shape(china_city_cropped) +
  tm_fill(col = "gray98") +
  tm_borders(col = "gray65", lwd = 1.5, lty = "solid") +
  
  tm_shape(background) + 
  tm_raster(
    palette = brewer.pal(9, "PiYG"),
    style = style,
    breaks = breaks,
    title = "NDVI",
    labels = as.character(round(breaks, 2)),
    legend.reverse = TRUE,
    legend.hist = TRUE
  ) +
  
  tm_shape(boundary) +
  tm_borders(col = "black", lwd = 2.5) +
  
  tm_shape(water) +
  tm_lines(col = "#9ECAE1", lwd = 2) +
  
  tm_shape(admin_boundaries) +
  tm_borders(col = "black", lwd = 1.5) +
  
  tm_shape(survey_points) +
  tm_dots(
    col = "parts",
    palette = c("1" = "yellow", "2" = "red", "3" = "blue"),
    size = 0.5,
    shape = 1,
    border.lwd = 3,
    title = "Survey Parts",
    labels = c("Northern Villages", "Central Villages", "Southern Villages")
  ) +
  
  tm_layout(
    scale = TRUE,
    compass.type = "8star",
    frame = TRUE,
    frame.lwd = 3,
    outer.margins = c(0.01, 0.01, 0.01, 0.01),
    inner.margins = c(0.001, 0.001, 0.001, 0.001)
  )

# 10.1 Add scale bar
map <- map + tm_scale_bar(
  position = c(0.05, 0.004),
  text.size = 1,
  lwd = 2
)

# 10.2 Add north arrow
map <- map + tm_compass(
  position = c("right", "top"),
  size = 6
)

# 10.3 Customize legend
map <- map + tm_legend(
  position = c(1.05, 0.3),
  legend.width = 2,
  legend.height = 12,
  legend.title.size = 2,
  legend.text.size = 1.5,
  legend.format = list(fun = function(x) format(x, digits = 2))
)

# 10.4 Add grid with latitude/longitude labels
map <- map + tm_grid(
  n.x = 3,
  n.y = 4,
  line = TRUE,
  labels.size = 1,
  alpha = 0.5,
  labels.format = list(
    fun = function(x) paste0(abs(x), "°", ifelse(x >= 0, ifelse(x > 0, "E", "W"), ifelse(x < 0, "W", "E")))
  )
) +
  tm_grid(
    n.x = 3,
    n.y = 4,
    line = TRUE,
    labels.size = 1,
    alpha = 0.5,
    labels.format = list(
      fun = function(x) paste0(abs(x), "°", ifelse(x >= 0, "N", "S"))
    )
  )

# 11. Print map
map

# 12. Save map as image for composition
tmap_save(map, "map_output.png")

# Part 2. Add statistical plots
library(gridExtra)
library(ggplot2)
library(tmap)
library(cowplot)
library(dplyr)
library(gridExtra)
library(grid)

# Convert raster to data frame with lon/lat/value
background_points <- raster::rasterToPoints(background)
background_df <- as.data.frame(background_points)
colnames(background_df) <- c("longitude", "latitude", "value")

# Part 2.1 Line plot with confidence band
# 1. Compute mean background value by latitude
mean_background_by_latitude <- background_df %>%
  group_by(latitude) %>%
  summarise(mean_background = mean(value, na.rm = TRUE))

# 2. Create line plot of mean background by latitude
line_plot <- ggplot(mean_background_by_latitude, aes(x = latitude, y = mean_background)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = mean_background - 50, ymax = mean_background + 50), fill = "skyblue", alpha = 0.4) +
  theme_minimal() +
  labs(x = "Latitude", y = NULL) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 12,  hjust = 0.5)
  )
line_plot

# 3. Rotate line plot using coord_flip()
line_plot_rotated <- line_plot + coord_flip()
line_plot_rotated

# 4. Arrange line plot and map with plot_grid
img <- png::readPNG("map_output.png")
map_grob <- rasterGrob(img)
final_plot <- plot_grid(line_plot_rotated, map_grob, ncol = 2, rel_widths = c(1, 3), align = "v")
final_plot

# Part 2.2 Bar plot of value distribution
# 1. Divide background values into 5 classes
background_breaks <- seq(min(background_df$value, na.rm = TRUE), max(background_df$value, na.rm = TRUE), length.out = 6)
background_df$background_category <- cut(background_df$value, breaks = background_breaks, include.lowest = TRUE, labels = paste0("Class ", 1:5))

# 2. Compute percentage of each class
background_count <- background_df %>%
  group_by(background_category) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# 3. Plot bar chart
background_barplot <- ggplot(background_count, aes(x = factor(background_category), y = percentage, fill = background_category)) +
  geom_bar(stat = "identity", color = "black", linewidth = 0.5) +
  scale_fill_brewer(palette = "PiYG") +
  labs(x = "Background Classes", y = "Percentage (%)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 18, color = "black"),
    axis.title.y = element_text(size = 18, color = "black"),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    plot.title = element_text(size = 18, hjust = 0.5),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(0.3, "cm")
  )

background_barplot
