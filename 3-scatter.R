# Part 1. Load required packages
library(terra)
library(data.table)
library(parallel)
library(future.apply)
library(RColorBrewer)

# Part 2. Set working and output directories
setwd("H:/science/F5-program/F6-HKUST/f2-Carbon measurement/f2-data/f5-send/f1-original data")
output_dir <- "H:/science/F5-program/F6-HKUST/f2-Carbon measurement/f2-data/f5-send/f4-results"

# Part 3. Detect available years
print(paste("Current working directory:", getwd()))

if (!dir.exists("ODIAC")) {
  stop("Folder 'ODIAC' does not exist, please check the path!")
}

odiac_files <- list.files("ODIAC", pattern = "odiac.*\\.Tif$", full.names = TRUE, ignore.case = TRUE)

if (length(odiac_files) == 0) {
  stop("No ODIAC .tif files found!")
}

years <- gsub(".*odiac(\\d{4}).*\\.Tif$", "\\1", odiac_files, ignore.case = TRUE)
years <- sort(unique(as.numeric(years)))

print("Detected years:")
print(years)

# Part 4. Process rasters by year and remove outliers
results <- list()
terraOptions(memfrac = 0.8)
target_crs <- "EPSG:4326"

for (year in years) {
  print(paste("Processing year:", year))
  
  file_paths <- c(
    paste0("ODIAC/odiac", year, "_Clip_Resample.Tif"),
    paste0("GHS_POP/GHS_POP_", year, "_clip.Tif"),
    paste0("GHS_BUILT_H/GHS_BUILT_H_", year, "_clip.Tif"),
    paste0("GHS_BUILT_S/GHS_BUILT_S_", year, "_clip.Tif"),
    paste0("GHS_BUILT_V/GHS_BUILT_V_", year, "_clip.Tif")
  )
  
  layer_names <- c("odiac", "ghs_pop", "ghs_built_h", "ghs_built_s", "ghs_built_v")
  
  if (!all(file.exists(file_paths))) {
    warning(paste("Missing files in year", year, "- skipped!"))
    next
  }
  
  rasters <- lapply(file_paths, rast)
  
  for (i in seq_along(rasters)) {
    r <- rasters[[i]]
    
    if (is.na(crs(r))) {
      crs(r) <- target_crs
      message(paste(layer_names[i], "CRS missing → set to WGS84"))
    } else if (crs(r) != target_crs) {
      message(paste(layer_names[i], "CRS mismatch → reprojected to WGS84"))
      r <- project(r, target_crs)
    }
    
    rasters[[i]] <- r
  }
  
  rasters <- rast(rasters)
  names(rasters) <- layer_names
  
  ref_raster <- rasters[[1]]
  
  if (!all(ext(rasters) == ext(ref_raster)) || !all(res(rasters) == res(ref_raster))) {
    warning(paste(
      "Extent or resolution mismatch in year", year,
      "- skipped!"
    ))
    next
  }
  
  dt <- as.data.table(values(rasters))
  dt[, year := year]
  dt <- na.omit(dt)
  
  if (nrow(dt) == 0) {
    warning(paste("No valid data in year", year, "- skipped!"))
    next
  }
  
  X <- as.matrix(dt[, .(odiac, ghs_pop, ghs_built_h, ghs_built_s, ghs_built_v)])
  
  center_vec <- colMeans(X)
  cov_mat <- cov(X)
  dists <- mahalanobis(X, center = center_vec, cov = cov_mat)
  
  threshold <- qchisq(0.99, df = 5)
  
  dt[, md := dists]
  dt <- dt[md < threshold]
  
  results[[as.character(year)]] <- dt
  fwrite(dt, file = file.path(output_dir, paste0("year_", year, ".csv")))
  
  print(paste("Finished processing year:", year,
              "- valid pixel count:", nrow(dt)))
}

# Part 5. Load packages for scatter plot visualization
library(ggplot2)
library(scales)
library(data.table)
library(RColorBrewer)
library(patchwork)

# Part 6. Create output directory for scatter plots
plot_dir <- file.path(output_dir, "Scatter diagram")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

plots_list <- list()

# Part 7. Define unified scatter plotting function
plot_year_data <- function(dt, year) {
  p <- ggplot(dt, aes(x = ghs_built_s, y = odiac)) +
    geom_point(
      aes(color = ghs_built_h, size = ghs_built_v),
      alpha = 0.5,
      shape = 16
    ) +
    scale_color_gradientn(colors = brewer.pal(11, "PiYG")) +
    labs(
      title = year,
      x = "GHS_BUILT_S",
      y = "ODIAC",
      color = "GHS_BUILT_H"
    ) +
    scale_size_binned(
      n.breaks = 6,
      nice.breaks = TRUE,
      name = "GHS_BUILT_V"
    ) + 
    guides(
      color = guide_colorbar(order = 1),
      size = guide_legend(order = 2)
    ) +
    theme_minimal(base_size = 14, base_family = "serif") +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_line(color = "grey90"),
      panel.background = element_rect(fill = "white", color = "black", size = 1),
      plot.background = element_rect(fill = "white", color = NA),
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(0.25, "cm")
    )
  
  return(p)
}

# Part 8. Generate and save scatter plots for each year
for (year in names(results)) {
  dt <- results[[year]]
  
  p <- plot_year_data(dt, year)
  plots_list[[year]] <- p
  
  print(p)
  
  ggsave(
    filename = file.path(plot_dir, paste0("year_", year, ".png")),
    plot = p,
    width = 8, height = 6, dpi = 300
  )
  
  ggsave(
    filename = file.path(plot_dir, paste0("year_", year, ".pdf")),
    plot = p,
    width = 8, height = 6
  )
}

# Part 9. Combine all yearly scatter plots into overview figure
combined_plot <- wrap_plots(plots_list, ncol = 3)

print(combined_plot)

ggsave(
  filename = file.path(plot_dir, "combined_Scatter.png"),
  plot = combined_plot,
  width = 24, height = 12, dpi = 300
)

ggsave(
  filename = file.path(plot_dir, "combined_Scatter.pdf"),
  plot = combined_plot,
  width = 24, height = 12
)