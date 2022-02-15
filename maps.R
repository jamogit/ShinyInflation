library(geofi)

d1 <- get_municipalities(year = 2020)
d2 <- get_zipcodes(year = 2020)
d3 <- get_statistical_grid(resolution = 5)
d4 <- get_population_grid(resolution = 5)

library(ggplot2)
library(dplyr)
theme_set(
  theme_minimal(base_family = "Arial") +
    theme(legend.position= "none",
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank()
    )
)
p1 <- ggplot(d1, aes(fill = kunta)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "municipalities")
p2 <- ggplot(d1 %>% count(maakunta_code), aes(fill = maakunta_code)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "Aggregated municipality data \nat region (maakunta) level \n(one of many!)")
p3 <- ggplot(d2, aes(fill = as.integer(posti_alue))) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "zipcodes")
p4 <- ggplot(d3, aes(fill = nro)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "statistical grid")
p5 <- ggplot(d4, aes(fill = id_nro)) + geom_sf(colour = alpha("white", 1/3)) + labs(subtitle = "population grid")
p6 <- ggplot(municipality_central_localities, aes(color = as.integer(kuntatunnus))) + geom_sf() + labs(subtitle = "Central municipality localities")

library(patchwork)
wrap_plots(list(p1,p2,p3,p4,p5,p6), ncol = 3) + 
  patchwork::plot_annotation(title = "Spatial data in geofi-package")


p1 <- ggplot(d1, border = NA, aes=(fill = vuosi)) +
  #geom_sf(colour = alpha("black", 3/3)) +
  geom_sf(data = d1) +
  theme_void() + 
  theme(
    legend.position = "none"
  )

p1



library(geofi)
finmapdata <- get_municipalities(year = 2020) %>%
  transmute(
    geom = d1$geom,
    vuosi = d1$vuosi
  )

plot(finmapdata, border = NA, col="black", main = NULL, options(width = 0.1))

