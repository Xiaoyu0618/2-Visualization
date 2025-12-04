library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(prettymapr)
library(ggspatial)
library(grid)

# Read map data (ensure paths are correct)
china_outline <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-china/f1-shp/china_outline.shp")
china_border <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-china/f1-shp/china_border.shp")
china_province <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-china/f1-shp/china_province.shp")
china_ninedash <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-china/f1-shp/china_ninedash.shp")
qtp_region <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-china/f1-shp/Tibet_Plateau.shp")
hhgd_region <- st_read("H:/science/F5-program/F3-Assessment/f2-figures/f1-china/f1-shp/hhgd_border.shp")

# Get world map and unify CRS
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = st_crs(china_outline))
qtp_region <- st_transform(qtp_region, st_crs(china_outline))
hhgd_region <- st_transform(hhgd_region, st_crs(china_outline))

# Set plotting extent
china_bbox <- st_bbox(china_outline)
xlim <- c(china_bbox$xmin - 1, china_bbox$xmax + 1)
ylim <- c(china_bbox$ymin - 1, china_bbox$ymax + 1)

# Canvas background and margins
par(bg = "white", mar = c(0.1, 0.1, 0.1, 0.1))

# Draw world base map
plot(
  st_geometry(world),
  col = "gray97",
  border = "gray70",
  lwd = 0.5,
  xlim = xlim,
  ylim = ylim,
  main = ""
)

# Add China boundary layers
plot(
  st_geometry(china_province),
  col = "gray93",
  border = "#A0A0A0",
  lwd = 1,
  add = TRUE
)

plot(
  st_geometry(china_border),
  col = NA,
  border = "#3A3A3A",
  lwd = 1,
  add = TRUE
)

plot(
  st_geometry(china_ninedash),
  col = "#3A3A3A",
  lty = 1,
  lwd = 1,
  add = TRUE
)

plot(
  st_geometry(qtp_region),
  col = adjustcolor("blue", alpha.f = 0.9),
  lwd = 1,
  add = TRUE
)

plot(
  st_geometry(hhgd_region),
  col = adjustcolor("#B2CFFF"),
  border = "white",
  lwd = 1,
  add = TRUE
)

# Add legend
legend(
  "bottomleft",
  inset = 0.01,
  legend = c("Tibet Plateau", "Hehuang valley"),
  col = c("blue", "#B2CFFF"),
  lty = c(1, 1),
  lwd = c(2, 2),
  bty = "n",
  pt.cex = 2,
  cex = 1.5
)

# Add scalebar and north arrow
prettymapr::addscalebar(
  plotunit = "km",
  pos = "bottomleft",
  padin = c(0.2, 0.8),
  tick.cex = 0.9,
  lwd = 2
)

addnortharrow(
  pos = "topleft",
  scale = 0.9,
  padin = c(0.25, 0.5),
  col = "black",
  border = "black"
)
