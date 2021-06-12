# 0. Initial set-up
{
  # 0.1. clear env
rm(list = ls())

  # 0.2. load packages
requiredPackages <- c('tidyverse', 'haven', 'readxl', 'utf8', 'rjson', 'osmdata', 'sf', 'sp', 'broom', 'viridis', 'cowplot', 'reshape2', 'spatialreg',
                      'spdep', 'plm', 'splm', 'Metrics', 'stargazer', 'kableExtra', 'ggpattern', 'scales')
for (i in requiredPackages) {
  if (!require(i,character.only = T)) {
    install.packages(i, character.only = T)
  }
}
#remotes::install_github("coolbutuseless/ggpattern")

  # 0.3. define custom functions
r2 <- function (actual, res) {
  rss <- sum(res^2)
  tss <- sum((actual - mean(actual)) ^ 2)
  r2 <- 1 - rss/tss
  return (r2)
}

r2_adj <- function (actual, res, p = 29) {
  n <- 759
  r2_adj <- 1 - ((1 - r2(actual, res)) * (n - 1) / (n - p - 1))
  return (r2_adj)
}

rmse <- function (res) {
  return (sqrt(sum(res^2)/759))
}

# https://stackoverflow.com/questions/46186527/how-to-calculate-bic-and-aic-for-a-gmm-model-in-r-using-plm
AIC_adj <- function(mod, p){
  # Number of observations
  n.N   <- nrow(mod$model)
  # Residuals vector
  u.hat <- residuals(mod)
  # Variance estimation
  s.sq  <- log( (sum(u.hat^2)/(n.N)))
  # Number of parameters (incl. constant) + one additional for variance estimation
  p     <-  p
  
  # Note: minus sign cancels in log likelihood
  aic <- 2*p  +  n.N * (  log(2*pi) + s.sq  + 1 ) 
  
  return(aic)
}
}

# 1. Since our code was deleted, we will be using the files that were saved during development, but the code is unavailable
{
  # 1.1. file with dependent and independent variables in years 2008-2019 (only check_in is available for 2019)
data <- read.csv('data/data.csv', sep = ',')

  # 1.2. file with OSM IDs for all borough (was required for downloading the data from OSM and will be used to sort the observations)
boroughs_osm <- read.csv('data/boroughs_osm.csv', sep = ',')

  # 1.3. file with borough polygons (downloaded from OSM using osmdata package and transformed to correct CRS)
locs <- readRDS('data/locs.RDS')

  # 1.4. file with borough polygons joined with information from data file (locs + data)
locs_sp <- readRDS('data/locs_sp.RDS')

  # 1.5. file with buildings
bldgs <- readRDS('data/bldgs.RDS')

  # 1.6 file with roads
hghws <- readRDS('data/hghws.RDS')
}

# 2. Let's prepare data for modeling
{
  # 2.1. filter out 2019 since it only contains target variable
data_model <- data %>%
  filter(year != 2019)

  # 2.2. some variables do not change over time, and thus we should find only spatial lags for them
variables_constant <- c('bu', 'bur', 'br', 'dist', 'train')
    
  # 2.3. now let's select the variables that do change over time
variables_temporal <- c('check_in', 'income', 'kinder', 'unempl', 'pop_density', 'greenery', 'average_b_waw')

  # 2.4. convert binary variables to categorical
data_model <- data_model %>%
  mutate_at(c('bu', 'bur', 'br', 'train'), as.factor)

  # 2.4. add osm_id and select columns for modeling
data_model <- data_model %>%
  merge(boroughs_osm, by = 'borough') %>%
  dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], all_of(variables_temporal)) %>%
  arrange(osm_id, year)

  # 2.5. create spatial weights matrix
locs$osm_multipolygons <- arrange(locs$osm_multipolygons, rownames(locs$osm_multipolygons))

cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
cont.listw <- nb2listw(cont.nb, style="W")
}

# 3. Plots
{
  # 3.1 plot of boroughs with names
{
  # 3.1.1. preapre data with polygons' centroids to add labels to map
  locs_cent_sp <- st_transform(locs$osm_multipolygon, crs = 2180) %>%
    st_geometry() %>%
    st_centroid() %>%
    st_transform(., crs ="+proj=longlat +datum=WGS84") %>%
    as(., 'Spatial') %>%
    as.data.frame() %>%
    mutate (
      borough = locs$osm_multipolygons$borough,
      x = as.numeric(coords.x1),
      y = as.numeric(coords.x2)
    ) %>%
    dplyr::select(borough, x, y)
  
  # 3.1.2. for some boroughs, numbers will be used instead of names (for readability purposes)
  boroughs_numbers <- c('Nowy Dwór Mazowiecki', 'Piastów', 'Podkowa Leśna', 'Milanówek', 'Mińsk Mazowiecki', 'Mińsk Mazowiecki 2', 'Legionowo', 'Pruszków', 'Sulejówek')
  
  # 3.1.3. filter for boroughs where numbers will be shown
  locs_cent_sp_number <- locs_cent_sp[locs_cent_sp$borough %in% boroughs_numbers, ] %>%
    arrange(borough)
  
  # 3.1.4. add unique numbers to borough
  locs_cent_sp_number$number <- 1:nrow(locs_cent_sp_number)
  
  # 3.1.5. filter for boroughs where names will be shown
  locs_cent_sp_name <- locs_cent_sp[!locs_cent_sp$borough %in% boroughs_numbers, ]
  
  # 3.1.6. create data frame to add the names of borough denoted with numbers
  (max(locs_cent_sp_name$y) - min(locs_cent_sp_name$y)) / 9 # -0.0706
  
  map_legend <- data.frame(
    number = locs_cent_sp_number$number,
    borough = locs_cent_sp_number$borough,
    x = max(locs_cent_sp_name$x) + 0.15,
    y = seq(max(locs_cent_sp_name$y), min(locs_cent_sp_name$y), -0.0709)
  )
  
  # 3.1.7. add enters to names that have more than one word (for readability purposes)
  for (i in (1:nrow(locs_cent_sp_name))) {
    name <- locs_cent_sp_name[i, 'borough']
    if (grepl(' ', name) ||  grepl('-', name)) {
      name <- ifelse(grepl(' ', name), strsplit(name, ' '), strsplit(name, '-'))[[1]]
      name <- paste(name,  collapse = '\n')
      locs_cent_sp_name[i, 'borough'] <- name
    }
  }
  
  # 3.1.8. manually adjust locations to better fit the name
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Lesznowola', 'y'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Lesznowola', 'y'] + 0.01
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Ożarów\nMazowiecki', 'y'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Ożarów\nMazowiecki', 'y'] + 0.005
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Wiązowna', 'y'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Wiązowna', 'y'] + 0.01
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Wiązowna', 'x'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Wiązowna', 'x'] - 0.01
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Celestynów', 'x'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Celestynów', 'x'] - 0.01
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Pomiechówek', 'x'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Pomiechówek', 'x'] - 0.01
  locs_cent_sp_name[locs_cent_sp_name$borough == 'Pomiechówek', 'y'] <- locs_cent_sp_name[locs_cent_sp_name$borough == 'Pomiechówek', 'y'] - 0.014
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Mińsk Mazowiecki 2', 'y'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Mińsk Mazowiecki 2', 'y'] - 0.01
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Mińsk Mazowiecki 2', 'x'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Mińsk Mazowiecki 2', 'x'] + 0.045
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Mińsk Mazowiecki 1', 'y'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Mińsk Mazowiecki 1', 'y'] + 0.005
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Nowy Dwór Mazowiecki', 'y'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Nowy Dwór Mazowiecki', 'y'] - 0.011
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Nowy Dwór Mazowiecki', 'x'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Nowy Dwór Mazowiecki', 'x'] + 0.02
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Podkowa Leśna', 'y'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Podkowa Leśna', 'y'] - 0.007
  locs_cent_sp_number[locs_cent_sp_number$borough == 'Podkowa Leśna', 'x'] <- locs_cent_sp_number[locs_cent_sp_number$borough == 'Podkowa Leśna', 'x'] + 0.018
  
  # 3.1.9. plot
  map_names <- ggplot(locs_sp, aes(x=long, y=lat, group=group)) +
    # borough outlines and fill
    geom_polygon(color = 'black', fill = '#f9f9f9', size=1) +
    # number labels on the map
    geom_label(
      data = locs_cent_sp_number,
      mapping = aes(x=x, y=y, label = number),
      size = 2.5,
      color = 'black',
      fontface = 'bold',
      inherit.aes = F) +
    # number labels on the legend
    geom_label(
      data = map_legend,
      mapping = aes(x=x, y=y, label = number),
      size = 2.5,
      color = 'black',
      fontface = 'bold',
      inherit.aes = F) +
    # text labels on the legend
    geom_text(
      data = map_legend,
      mapping = aes(x=x + 0.03, y=y, label = borough),
      size = 2.5,
      color = 'black',
      fontface = 'bold',
      hjust = 0,
      inherit.aes = F) +
    # empty points to fit the legend's text labels
    geom_point(
      data = map_legend,
      mapping = aes(x=x + 0.15, y=y),
      size = 0,
      color = NA,
      inherit.aes = F) +
    # text labels on the map
    geom_text(
      data = locs_cent_sp_name,
      mapping = aes(x=x, y=y, label = borough),
      size = 2,
      color = 'black',
      fontface = 'bold',
      angle = 20,
      inherit.aes = F) +
    coord_fixed() +
    theme_void()
  
  # 3.1.10. save plot
  png('img/map_names.png', width = 3508, height = 1700, res=300)
  map_names
  dev.off()
}
  
  # 3.2. plot with highways and buildings
{
  # 3.2.1. large highway types
  hghws_tp_lrg <- c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')
  
  # 3.2.2. select only large highways
  hghws_lrg <- hghws$osm_lines[hghws$osm_lines$highway %in% hghws_tp_lrg,]
  
  # 3.3.3. function to plot boroughs_osm
  plt_loc <- function(borough = 'all', l=T, b=T, h=T, hl=T) {
    # create spatial object for loc, bldgs, hghws based on selection go borough
    if (borough=='all') {
      locs_sp <- as(locs$osm_multipolygon, 'Spatial')
      bldgs_sp <- as(bldgs$osm_polygons, 'Spatial')
      hghws_sp <- as(hghws$osm_lines, 'Spatial')
      hghws_lrg_sp <- as(hghws_lrg, 'Spatial')
    } else if (class(borough) == 'character') {
      locs_sp <- as(locs$osm_multipolygon[locs$osm_multipolygon$borough %in% borough,], 'Spatial')
      bldgs_sp <- as(bldgs$osm_polygons[bldgs$osm_polygons$borough %in% borough,], 'Spatial')
      hghws_sp <- as(hghws$osm_lines[hghws$osm_lines$borough %in% borough,], 'Spatial')
      hghws_lrg_sp <- as(hghws_lrg[hghws_lrg$borough %in% borough,], 'Spatial')
    } else if (class(borough) == 'numeric') {
      locs_sp <- as(locs$osm_multipolygon[borough,], 'Spatial')
      bldgs_sp <- as(bldgs$osm_polygons[borough,], 'Spatial')
      hghws_sp <- as(hghws$osm_lines[borough,], 'Spatial')
      hghws_lrg_sp <- as(hghws_lrg[borough,], 'Spatial')
    }
    
    legend <- c()
    if (l) {legend <- append(legend, c('border' = 'black'))}
    if (b) {legend <- append(legend, c('building' = '#482677'))}
    if (hl) {legend <- append(legend, c('large road' = '#1F968B'))}
    if (h) {legend <- append(legend, c('road' = '#73D055'))}
    
    # plot
    p <- ggplot() +
      {if (h) geom_line(data = hghws_sp, aes(x=long, y=lat, group=group, colour = 'road'), size=0.2)} +
      {if (hl) geom_line(data = hghws_lrg_sp, aes(x=long, y=lat, group=group, colour = 'large road'), size=0.4)} +
      {if (b) geom_polygon(data = bldgs_sp, aes(x=long, y=lat, group=group, colour = 'building'), fill = '#482677', size=0)} +
      {if (l) geom_polygon(data = locs_sp, aes(x=long, y=lat, group=group, colour = 'border'), fill=NA, size=1)} +
      labs(
        title = borough,
        color = NULL) +
      scale_color_manual(values = legend) +
      theme_void() +
      coord_fixed() +
      theme(
        plot.title = element_text(color="#000000", face="bold", size=16, hjust=0.5),
        legend.position = 'bottom',
        legend.text = element_text(size=10)
      ) +
      guides(color = guide_legend(
        keyheight = 1, 
        keywidth = 1,
        override.aes = list(fill = legend)
      ))
    
    print(p)
    
    return(p)
  }
  
  # save plot for Pruszków
  png('img/map_pruszkow.png', width = 3508, height = 2000, res=300)
  plt_loc('Pruszków')
  dev.off()
  
  # save plot for Konstancin Jeziorna
  png('img/map_konstancin.png', width = 3508, height = 2000, res=300)
  plt_loc('Konstancin-Jeziorna')
  dev.off()
  
  # save plot for Jaktorów
  png('img/map_jaktorow.png', width = 3508, height = 2000, res=300)
  plt_loc('Jaktorów')
  dev.off()
}
  
  # 3.3.  define plot function
plt_metric <- function (var, year, title, caption, legend, breaks) {
  var <- enquo(var)
  ggplot() +
    geom_polygon(data = locs_sp[locs_sp$year == year,], aes(x=long, y=lat, group=group, fill=!!var), colour='black', size=0.5, alpha=0.9) +
    coord_sf() +
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

  # 3.4. define plot function to compare 2 years
plt_compare <- function (var, year1, year2, title, caption, legend, breaks) {
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

  # 3.5. plot bu/bur/br
plt_bu_bur_bu <- locs_sp %>%
  mutate(
    b = case_when(
      bu == '1' ~ 'urban',
      bur == '1' ~ 'urban-rural',
      T ~ 'rural'
    )) %>%
  filter(
    year == 2018
  ) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=b), colour='black', size=0.5, alpha=0.9) +
  coord_sf() +
  scale_fill_viridis(discrete=T) +
  labs(
    caption = 'Data Source: Polish Statistical Office',
    title = 'a) type of borough',
    fill = 'type'
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
  )

  # 3.6. plots for time-invariant variables
plt_dist <- plt_metric(dist, 2018, 'c) distance', 'Data Source: Google Maps', 'km', c(10, 12, 15, 20, 25, 30, 40, 50))

plt_train <- locs_sp %>%
  mutate(
    train = case_when(
      train == '1' ~ 'presence of suburban train station',
      T ~ 'lack of suburban train station'
    )) %>%
  filter(
    year == 2018
  ) %>%
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=train), colour='black', size=0.5, alpha=0.9) +
  coord_sf() +
  scale_fill_viridis(discrete=T) +
  labs(
    caption = 'Data Source: Google Maps',
    title = 'b) suburban train station',
    fill = ''
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
  )


  # 3.7. combined plot for all variables
plt_check_in <- plt_compare(check_in, 2008, 2018, 'd) migrants', 'Data Source: Polish Statistical Office', '# of migrants', c(1, 2, 5, 10, 25, 50, 100, 250, 750))

plt_income <- plt_compare(income, 2008, 2018, 'e) income per capita', 'Data Source: own calculation based on data from Polish Statistical Office', 'ratio', c(0.3, 0.5, 0.75, 1, 1.5, 2, 2.5, 3))

plt_kinder <- plt_compare(kinder, 2008, 2018, 'f) number of kindergartens', 'Data Source: Polish Statistical Office', '# of kindergartens', c(1, 2, 5, 10, 25, 50, 75, 100))

plt_unempl <- plt_compare(unempl, 2008, 2018, 'g) unemployment rate', 'Data Source: Polish Statistical Office', 'ratio', seq(0, 0.1, 0.01))

plt_pop_density <- plt_compare(pop_density, 2008, 2018, 'h) population density', 'Data Source: Polish Statistical Office', '', c(0.5, 1, 2, 3, 5, 7, 10, 15, 20, 30)) +
  labs(fill = expression(bold(paste('# of people / ', km^2))))

plt_greenery <- plt_compare(greenery, 2008, 2018, 'i) area of green amenities (parks etc.)', 'Data Source: Polish Statistical Office', 'area in ha', c(2, 5, 10, 25, 50, 100, 250))

plt_average_b_waw <- plt_compare(average_b_waw, 2008, 2018, 'j) relative price of housing', 'Data Source: Polish Statistical Office & suburban county officies', 'ratio', c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))


png('img/maps_combined.png', width = 3508, height = 2*2480, res=300)
plot_grid(plt_bu_bur_bu, plt_dist, plt_train, plt_check_in, plt_income, plt_kinder, plt_unempl, plt_pop_density, plt_greenery, plt_average_b_waw, ncol = 2, greedy=F)
dev.off()
}

# 4. Modeling
{
  # 4.1. create formulas - form_all (all variables) & form_time (time-varying variables)
form_all <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density + greenery + average_b_waw
form_time <- check_in ~ income + kinder + unempl + pop_density + greenery + average_b_waw

  # 4.2 testing - performed on both formulas
bsjktest(form_all, data=data_model, listw=cont.listw, test="C.1") # H0 rejected -> 'spatial dependence in error terms, sub RE and serial corr.'
bsjktest(form_time, data=data_model, listw=cont.listw, test="J") # H0 rejected -> 'random effects or serial corr. spatial dependence in error terms'

bsktest(form_all, data=data_model, listw=cont.listw, test="LM1", standardize=TRUE) # H0 rejected -> Random effects
bsktest(form_time, data=data_model, listw=cont.listw, test="LM1", standardize=TRUE) # H0 rejected -> Random effects

bsktest(form_all, data=data_model, listw=cont.listw, test="LM2", standardize=TRUE) # H0 rejected -> Spatial autocorrelation
bsktest(form_time, data=data_model, listw=cont.listw, test="LM2", standardize=TRUE) # H0 rejected -> Spatial autocorrelation

sphtest(form_time, data=data_model, listw=cont.listw, spatial.model="error", method="ML", errors = 'BSK') # H0 rejected -> Fixed Effects
sphtest(form_time, data=data_model, listw=cont.listw, spatial.model="lag", method="ML", errors = 'BSK') # H0 rejected -> Fixed Effects
sphtest(form_time, data=data_model, listw=cont.listw, spatial.model="sarar", method="ML", errors = 'BSK') # H0 rejected -> Fixed Effects



  # 4.3. plm models - without spatial errors and coefficients

    # 4.3.1. FE for time
model_plm_1 <- plm(form_all, data=data_model, model="within", effect="time")
summary(model_plm_1)

    # 4.3.2 FE for boroughs
model_plm_2 <- plm(form_time, data=data_model, model="within", effect="individual")
summary(model_plm_2)

    # 4.3.3. FE for time & boroughs
model_plm_3 <- plm(form_time, data=data_model, model="within", effect="twoway")
summary(model_plm_3)

    # 4.3.4. pooling
model_plm_4 <- plm(form_all, data=data_model, model="pooling")
summary(model_plm_4)

  # 4.4. spml models - with spatial errors and coefficients

    # 4.4.1. FE for time with Baltagi errors and y lags
model_spml_1 <-spml(form_all, data=data_model, listw=cont.listw, model="within", spatial.error="b", lag=T, effect="time")
summary(model_spml_1)

    # 4.4.2. FE for time with Baltagi errors 
model_spml_2<-spml(form_all, data=data_model, listw=cont.listw, model="within", spatial.error="b", lag=F, effect="time")
summary(model_spml_2)

    # 4.4.3. FE for boroughs with Baltagi errors and y lags
model_spml_3<-spml(form_time, data=data_model, listw=cont.listw, model="within", spatial.error="b", lag=T, effect="individual")
summary(model_spml_3)

    # 4.4.4. FE for boroughs with Baltagi errors
model_spml_4<-spml(form_time, data=data_model, listw=cont.listw, model="within", spatial.error="b", lag=F, effect="individual")
summary(model_spml_4)

    # 4.4.5. FE for time & boroughs with Baltagi errors and y lags
model_spml_5<-spml(form_time, data=data_model, listw=cont.listw, model="within", spatial.error="b", lag=T, effect="twoway")
summary(model_spml_5)

    # 4.4.6. FE for time & boroughs with Baltagi errors
model_spml_6<-spml(form_time, data=data_model, listw=cont.listw, model="within", spatial.error="b", lag=F, effect="twoway")
summary(model_spml_6)

    # 4.4.7. pooling with Baltagi errors and y lags
model_spml_7<-spml(form_all, data=data_model, listw=cont.listw, model="pooling", spatial.error="b", lag=T)
summary(model_spml_7)

    # 4.4.8. pooling with Baltagi errors
model_spml_8<-spml(form_all, data=data_model, listw=cont.listw, model="pooling", spatial.error="b", lag=F)
summary(model_spml_8)

  # 4.5. models' statistics
{
  model_stats <- data.frame(
    model_name = rep('', 12),
    params = c(11 + 11, 7 + 69, 7 + 11 + 69, 11, 11 + 11 + 2, 11 + 11 + 1, 7 + 69 + 2, 7 + 69 + 1, 7 + 11 + 69 + 2, 7 + 11 + 69 + 1, 11 + 2, 11 + 1),
    formula = c('form_all', rep('form_time', 2), rep('form_all', 3), rep('form_time', 4), rep('form_all', 2)),
    type = c(rep('plm', 4), rep('spml', 8)),
    param_model = c(rep("within", 3), 'pooling', rep("within", 6), rep('pooling', 2)),
    param_spatial.error = c(rep('-', 4), rep("b", 8)),
    param_lag = c(rep('-', 4), rep(c('T', 'F'), 4)),
    param_effect = c('time', 'individual', 'twoway', '-', rep('time', 2), rep('individual', 2), rep('twoway', 2), rep('-', 2)),
    rmse = rep(0, 12),
    r2 = rep(0, 12),
    r2_adj = rep(0, 12),
    aic_adj = rep(0, 12)
  )
  
  i <- 1
  
  for (model in c(paste0('model_plm_', seq(1,4)), paste0('model_spml_', seq(1,8)))) {
    
    model_stats[i,'model_name'] <- model
    model_stats[i,'rmse'] <- rmse(get(model)$residuals)
    model_stats[i,'r2'] <- r2(data_model$check_in, get(model)$residuals)
    model_stats[i,'r2_adj'] <- r2_adj(data_model$check_in, get(model)$residuals, model_stats[i,'params'])
    model_stats[i,'aic_adj'] <- AIC_adj(get(model), model_stats[i,'params'])
    i <- i+1
  }
}

model_stats %>%
  arrange(desc(r2_adj)) %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "hover"))
# model_spml_5 has lowest RMSE and highest R2 Adjusted, but model_plm_3 has the lowest Adjusted AIC

  # 4.6. final model
    # 4.6.1. summary
summary(model_spml_5)

  # 4.6.2. impacts
impacts <- impacts(model_spml_5, listw = cont.listw, time = 11)

impacts <- data.frame(
  variable = attributes(impacts)$bnames,
  direct = round(impacts$res[1][[1]], 2),
  indirect = round(impacts$res[2][[1]], 2),
  total = round(impacts$res[3][[1]], 2)
)

impacts %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "responsive", "hover"))

  # 4.6.3. time effects
effects_time <- effects(model_spml_5)$TETable %>%
  as.data.frame()

  # 4.6.4. individual effects
effects_individual <- effects(model_spml_5)$SETable %>%
  as.data.frame() %>%
  mutate(
    borough = locs$osm_multipolygons$borough,
    osm_id = locs$osm_multipolygons$osm_id,
    effect_bin = factor(case_when(
      Estimate > -250 & Estimate <= 0 ~ '-250 - 0',
      Estimate > 0 & Estimate <= 250 ~ '0 - 250',
      Estimate > 250 & Estimate <= 500 ~ '250 - 500',
      Estimate > 500 & Estimate <= 1000 ~ '500 - 1000'),
      levels = c('-250 - 0', '0 - 250', '250 - 500', '500 - 1000')),
    effect_significant = ifelse(`Pr(>|t|)` <= 0.05, 'T', 'F')
  )

  # 4.6.5. map of individual effects
locs_sp <- locs_sp %>% 
  merge(., effects_individual, by.x='id', by.y='osm_id')

map_effects <- ggplot() +
  geom_polygon_pattern(data = locs_sp[locs_sp$effect_significant == 'F', ], aes(x=long, y=lat, group=group, fill=effect_bin), pattern = 'magick', colour='black', size=0.5, alpha=0.9) +
  geom_polygon(data = locs_sp[locs_sp$effect_significant == 'T', ], aes(x=long, y=lat, group=group, fill=effect_bin), colour='black', size=0.5, alpha=0.9) +
  coord_sf() +
  scale_fill_viridis(discrete=T) +
  #annotate(geom = 'text', label = '', x = 50, y = -2) + 
  labs(
    caption = 'Data Source: own calculation',
    title = 'Individual Effects',
    fill = 'dots represent insignificant effects (5%)'
  ) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(color="#000000", face="bold", size=16, hjust=0.5),
    plot.caption = element_text(color="#000000", face="bold", size=10, hjust=1),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    strip.text.x = element_text(color="#505050", face="bold", size=12, hjust=0.5),
    legend.title = element_text(color="#505050", size=12, hjust=0.5),
    legend.text = element_text(color="#505050", size=10, angle = 0),
    legend.position = 'bottom'
  ) + 
  guides(fill = guide_legend(title.position = "bottom"))

png('img/map_effects.png', width = 3508, height = 2480, res=300)
map_effects
dev.off()
}




