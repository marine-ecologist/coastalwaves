
ggplot() + theme_bw() +
  geom_sf(
    data = whitsundays |>mutate(habitat = if_else(!grepl("Reef", LOC_NAME_S), "island", "reef")),
    aes(fill = habitat),
    color = "black",
    linewidth=0.1
  ) +
  scale_fill_manual(values = c("island" = "#ffffc0", "reef" = "#e6f8ff")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = whitsundays_waves, aes(fill = wave_energy), shape = 21, size = 4) +
  scale_fill_distiller(palette = "RdYlBu", guide = guide_colorbar(title = "Wave Energy"))



library(sf)
library(dplyr)

# 1. Sample points along each linestring
sampled <- whitsundays_union |>
  st_line_sample(density = 1/100) |>
  st_cast("POINT") |>
  st_sf() |>
  mutate(id = row_number())

# 2. Add dummy wave energy (or your real values)
set.seed(1)
sampled <- sampled |> mutate(wave_energy = runif(n(), 0, 100))

# 3. Group back into lines with segments (e.g. every 10 pts = 1 segment)
sampled <- sampled |> mutate(segment = rep(1:ceiling(n()/10), each = 10)[1:n()])

lines_colored <- sampled |>
  group_by(segment) |>
  summarise(
    wave_energy = mean(wave_energy),
    geometry = st_cast(st_combine(st_geometry(st_as_sf(cur_data()))), "LINESTRING"),
    .groups = "drop"
  ) |>
  filter(as.numeric(st_length(geometry)) <= 1200)

# 4. Plot
library(ggplot2)
ggplot() +
  geom_sf(data = lines_colored, aes(color = wave_energy), linewidth = 1.2) +
  scale_color_viridis_c()


ggplot() + theme_bw() +
  geom_sf(
    data = whitsundays |>mutate(habitat = if_else(!grepl("Reef", LOC_NAME_S), "island", "reef")),
    aes(fill = habitat),
    color = "black",
    linewidth=0.1
  ) +
  scale_fill_manual(values = c("island" = "#ffffc0", "reef" = "#e6f8ff")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = lines_colored, aes(color = wave_energy), linewidth = 1.2) +
  scale_color_distiller(palette = "Reds", direction=-1, guide = guide_colorbar(title = "Wave Energy"))

