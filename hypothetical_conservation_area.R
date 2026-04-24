############################################################
# Simulation script to create rasters for a hypothetical SDM
# for an endangered frog species. GPS records = 8
# housing development proposed to be built at the white rectange
############################################################

library(terra)

set.seed(42)

# ---------------------------
# Create simple smooth surface
# ---------------------------
r = rast(ncols = 200, nrows = 200,
          xmin = 0, xmax = 100,
          ymin = 0, ymax = 100)

x = xFromCell(r, 1:ncell(r))
y = yFromCell(r, 1:ncell(r))

# Gaussian-like suitability surface (synthetic "SDM output")
z = exp(-((x - 60)^2 + (y - 60)^2) / 800) +
  0.6 * exp(-((x - 30)^2 + (y - 80)^2) / 500)

z = z / max(z)

r[] = z

# ---------------------------
# Fake presence points
# ---------------------------
pres_x = c(58, 62, 65, 55, 40, 70, 50, 60)
pres_y = c(58, 60, 63, 65, 75, 55, 70, 62)

# ---------------------------
# Development site polygon (simple square)
# ---------------------------
dev_x = c(45, 55, 55, 45, 45)
dev_y = c(45, 45, 55, 55, 45)

# ---------------------------
# Plot
# ---------------------------
plot(r,
     main = "Mock SDM Output: Species Suitability Surface",
     #col = terrain.colors(50)
     col = colorRampPalette(c("navy", "cyan", "yellow", "red"))(50)
     )

points(pres_x, pres_y,
       pch = 16, col = "black")

lines(dev_x, dev_y,
      col = "black", lwd = 2)

text(50, 42,
     labels = "Development site\nP(suitability) ≈ 0.72",
     col = "black")


# ---------------------------
# CURRENT suitability (your existing r)
# ---------------------------
current = r

# ---------------------------
# FUTURE climate shift
# (simple warming + drying + spatial shift)
# ---------------------------

x = xFromCell(r, 1:ncell(r))
y = yFromCell(r, 1:ncell(r))

# ---------------------------
# Strongly collapsing current core
# ---------------------------
core_loss = exp(-((x - 60)^2 + (y - 60)^2) / 450)

# ---------------------------
# Very weak and fragmented future refugia
# ---------------------------
refuge_1 = 0.25 * exp(-((x - 80)^2 + (y - 80)^2) / 900)
refuge_2 = 0.20 * exp(-((x - 20)^2 + (y - 75)^2) / 700)

# ---------------------------
# Net future suitability (strong decline)
# ---------------------------
future_z = (0.45 * core_loss) + refuge_1 + refuge_2

# enforce stronger contraction signal
future_z = future_z^1.5

# normalise
future_z = future_z / max(future_z)

future = r
future[] = future_z

par(mfrow = c(1, 2))

cols = colorRampPalette(c("navy", "cyan", "yellow", "red"))(50)

# CURRENT
plot(current,
     main = "Current Climate Suitability",
     col = cols)

points(pres_x, pres_y,
       pch = 16, col = "black")

lines(dev_x, dev_y,
      col = "red", lwd = 2)

# FUTURE
plot(future,
     main = "Future Climate Scenario",
     col = cols)

points(pres_x, pres_y,
       pch = 16, col = "black")

lines(dev_x, dev_y,
      col = "red", lwd = 2)

par(mfrow = c(1, 1))

diff = future - current

plot(diff,
     main = "Future - Current Suitability Change",
     col = colorRampPalette(c("blue", "white", "red"))(50))



df_current = as.data.frame(current, xy = TRUE)
df_future  = as.data.frame(future, xy = TRUE)
df_diff    = as.data.frame(future - current, xy = TRUE)

names(df_current)[3] = "value"
names(df_future)[3]  = "value"
names(df_diff)[3]    = "value"

cols = scale_fill_gradientn(
  colours = c("blue", "white", "red")
)

dev_box = data.frame(
  xmin = 45,
  xmax = 55,
  ymin = 45,
  ymax = 55
)

pres_df = cbind(pres_x, pres_y) %>%
  as.data.frame() %>%
  rename(x = pres_x, y = pres_y)
pres_df

make_plot = function(df, title, colour = "inferno") {
  ggplot(df, aes(x, y, fill = value)) +
    geom_raster() +
    
    # presence points
    geom_point(
      data = pres_df,
      aes(x = x, y = y),
      inherit.aes = FALSE,
      size = 2,
      colour = "black"
    ) +
    
    # development box
    geom_rect(
      data = dev_box,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = NA,
      colour = "white",
      linewidth = 1
    ) +
    
    scale_fill_viridis_c(option = colour) +
    ggtitle(title) +
    theme_minimal()
}

p1 = make_plot(df_current, "Current Climate")
p2 = make_plot(df_future, "Future Climate")
p3 = make_plot(df_diff, "Change (Future - Current)", colour = "magma")

hypo = (p1 | p2 | p3) +
  plot_layout(nrow = 2) 

hypo

mean(current[]) - mean(future[])

ggsave("hypothetical_conserved_area.png", plot = hypo, 
       width = 8, height = 6, dpi = 200)
