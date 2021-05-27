# 0. load packages
requiredPackages <- c('tidyverse', 'haven', 'readxl', 'utf8', 'rjson', 'osmdata', 'sf', 'sp', 'broom', 'viridis', 'cowplot', 'reshape2', 'spatialreg', 'spdep')
for (i in requiredPackages) {
  if (!require(i,character.only = T)) {
    install.packages(i, character.only = T)
  }
}

# 1. since our code was deleted, we will be using the files that were saved during development, but the code is unavailable
 
  # 1.1. file with dependent and independent variables in years 2008-2019 (only check_in is available for 2019)
data <- read.csv('data/data.csv', sep = ',')

  # 1.2. file with OSM IDs for all borough (was required for downloading the data from OSM and will be used to sort the observations)
boroughs_osm <- read.csv('data/boroughs_osm.csv', sep = ',')

  # 1.3. file with borough polygons (downloaded from OSM using osmdata package)
locs <- readRDS('data/locs.RDS')

  # 1.4. file with borough polygons joined with information from data file (locs + data)
locs_sp <- readRDS('data/locs_sp.RDS')


# 2. let's prepare data from modeling

  # 2.1. filter out 2019 since it only contains target variable
data_model <- data %>%
  filter(year != 2019)

  # 2.2. some variables do not change over time, and thus we should find only spatial lags for them
variables_constant <- c('bu', 'bur', 'br', 'dist', 'train')

  # 2.3. let's double check that these DO NOT change
data_model %>%
  dplyr::select(borough, year, variables_constant) %>%
  group_by(borough) %>%
  summarise_at(variables_constant, n_distinct) %>%
  filter_at(variables_constant, any_vars(. != 1))
    
  # 2.4. now let's select the variables that do change over time
variables_temporal <- c('check_in', 'income', 'kinder', 'unempl', 'pop_density', 'greenery', 'average_b_waw')

  # 2.5. let's double check that these DO change
data_model %>%
  dplyr::select(borough, year, variables_temporal) %>%
  group_by(borough) %>%
  summarise_at(variables_temporal, n_distinct) %>%
  filter_at(variables_temporal, any_vars(. == 1))
  # it seems that the value of greenery is not changing for some boroughs, but it is changing for most

  # 2.6. add osm_id and select columns for modeling
data_model <- data_model %>%
  merge(boroughs_osm, by='borough') %>%
  dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], variables_temporal)

  # 2.7. add lagged variables
    # 2.7.1. number of boroughs
n_osm_id <- length(unique(data_model$osm_id))
    
    # 2.7.2. temporal lag
data_model <- data_model %>%
  arrange(year, osm_id) %>%
  mutate_at(variables_temporal, .funs = list(ST1 = ~lag(., n_osm_id)))

    # 2.7.3 filter out 2008
data_model <- data_model %>%
  filter(year != 2008)

  # 2.8. create spatial weights matrix
cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
cont.listw <- nb2listw(cont.nb, style="W")

  # 2.9. replicate spatial weights matrix to account for multiple years (spatial structure is constant)
    # 2.9.1. number of years
n_year <- length(unique(data_model$year))

    # 2.9.2. create new object
cont.listw.yearly <- cont.listw

    # 2.9.3. replicate neighbours and weights by number of years
cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)

    # 2.9.4. assign the attributes from the original list
attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
attributes(cont.listw.yearly$weights) <- attributes(cont.listw$weights)
    
    # 2.9.5. replicate region IDs by number of years
attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
attributes(cont.listw.yearly$weights)$region.id <- rep(attributes(cont.listw.yearly$weights)$region.id, n_year)

  # 2.10. spatio-temporal lag
data_model <- data_model %>%
  mutate_at(
    paste(variables_temporal, 'ST1', sep='_'),
    .funs = ~lag.listw(cont.listw.yearly, .)
  )


# 3. Plots
  # 3.1. define plot function to compare 2 years
plot_metrics_compare <- function (var, year1, year2, title, caption, legend, breaks) {
  var <- enquo(var)
  ggplot() +
    geom_polygon(data = locs_sp[locs_sp$year %in% c(year1, year2),], aes(x=long, y=lat, group=group, fill=!!var), colour='black', size=0.5, alpha=0.9) +
    coord_sf() +
    facet_wrap(~year) +
    scale_fill_viridis(trans = "log1p",
                       breaks = breaks, labels = breaks
    ) +
    labs(
      caption = caption,
      title = title,
      fill = legend
    ) +
    theme_void() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(color="#000000", face="bold", size=16, hjust=0),
      plot.caption = element_text(color="#000000", face="bold", size=10, hjust=1),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
      strip.text.x = element_text(color="#505050", face="bold", size=12, hjust=0.5),
      legend.title = element_text(color="#505050", face="bold", size=12, hjust=1),
      legend.text = element_text(color="#505050", size=10, angle = 0),
      legend.position = 'bottom'
    ) +
    guides(
      fill = guide_colourbar(
        ticks.colour = "white",
        ticks.linewidth = 1.2,
        barwidth=15,
        label.position="bottom"))
}

  # 3.2. plot 4 most important metrics - 2008 vs 2018
plt_check_in <- plot_metrics_compare(check_in, 2008, 2018, 'a) migrants', 'Data Source: Polish Statistical Office', '# of migrants', c(1, 2, 5, 10, 25, 50, 100, 250, 750))

plt_pop_density <- plot_metrics_compare(pop_density, 2008, 2018, 'b) population density', 'Data Source: Polish Statistical Office', '', c(0.5, 1, 2, 3, 5, 7, 10, 15, 20, 30)) +
  labs(fill = expression(bold(paste('# of people / ', km^2))))

plt_dist <- plot_metrics_compare(dist, 2008, 2018, 'c) distance', 'Data Source: Google Maps', 'km', c(10, 12, 15, 20, 25, 30, 40, 50))

plt_income <- plot_metrics_compare(income, 2008, 2018, 'd) income per capita', 'Data Source: Polish Statistical Office', 'ratio', c(0.3, 0.5, 0.75, 1, 1.5, 2, 2.5, 3))

plt_comparison <- plot_grid(plt_check_in, plt_pop_density, plt_dist, plt_income, align = 'hv', greedy=F)
plt_comparison


# 3. Modelling

  # 3.1. formula
form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
  greenery + average_b_waw + check_in_ST1 + income_ST1 + kinder_ST1 + unempl_ST1 +
  pop_density_ST1 + greenery_ST1 + average_b_waw_ST1

  # 3.2. sac2 model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)

  # 3.3. sac3 model
model.sac3<-errorsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)




