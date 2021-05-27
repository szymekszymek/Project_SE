df <- data.frame(x = c("a","b","c"), y = c(1,2,3))

ggplot(data = df) +
  geom_rect(data = df, aes(x = x, y=y), xmin = as.numeric(df$x[[2]]) - 0.3,
            xmax = as.numeric(df$x[[3]]) + 0.3,
            ymin = 0, ymax = 2)


merge(boroughs_osm, by='borough') %>%
  dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], variables_temporal)
# add lagged variables
# number of boroughs
n_osm_id <- length(unique(data_model$osm_id))
# temporal lag
data_model <- data_model %>%
  arrange(year, osm_id) %>%
  mutate_at(variables_temporal, .funs = list(T1 = ~lag(., n_osm_id)))
# filter out 2008
data_model <- data_model %>%
  filter(year != 2008)
# create spatial weights matrix
cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
cont.listw <- nb2listw(cont.nb, style="W")
# replicate spatial weights matrix to account for multiple years (spatial structure is constant)
# number of years
n_year <- length(unique(data_model$year))
# create new object
cont.listw.yearly <- cont.listw
# replicate neighbours and weights by number of years
cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
# assign the attributes from the original list
attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
attributes(cont.listw.yearly$weights) <- attributes(cont.listw$neighbours)
# replicate region IDs by number of years
attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
attributes(cont.listw.yearly$weights)$region.id <- rep(attributes(cont.listw.yearly$weights)$region.id, n_year)
# spatio-temporal lag
data_model <- data_model %>%
  mutate_at(
    paste(variables_temporal, 'T1', sep='_'),
    .funs = list(ST1 = ~lag.listw(cont.listw.yearly, .))
  )
write.csv2(data_model, 'data/data_model.csv')
#formula
form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
  greenery + average_b_waw + check_in_ST1 + income_ST1 + kinder_ST1 + unempl_ST1 +
  pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
# model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
data_model
# spatio-temporal lag
data_model <- data_model %>%
  mutate_at(
    paste(variables_temporal, 'T1', sep='_'),
    .funs = ~lag.listw(cont.listw.yearly, .)
  )
# 4. MODEL DATA PREPARATION
# filter out 2019 since it only contains target variable
data_model <- data %>%
  filter(year != 2019)
# some variables do not change over time, and thus we should find only spatial lags for those
variables_constant <- c('bu', 'bur', 'br', 'dist', 'train')
# let's double check that these do not change
data_model %>%
  dplyr::select(borough, year, variables_constant) %>%
  group_by(borough) %>%
  summarise_at(variables_constant, n_distinct) %>%
  filter_at(variables_constant, any_vars(. != 1))
# now let's select the variables that do change over time
variables_temporal <- c('check_in', 'income', 'kinder', 'unempl', 'pop_density', 'greenery', 'average_b_waw')
# let's double check that these do change
data_model %>%
  dplyr::select(borough, year, variables_temporal) %>%
  group_by(borough) %>%
  summarise_at(variables_temporal, n_distinct) %>%
  filter_at(variables_temporal, any_vars(. == 1))
# add osm_id and select columns for modeling
data_model <- data_model %>%
  merge(boroughs_osm, by='borough') %>%
  dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], variables_temporal)
# add lagged variables
# number of boroughs
n_osm_id <- length(unique(data_model$osm_id))
# temporal lag
data_model <- data_model %>%
  arrange(year, osm_id) %>%
  mutate_at(variables_temporal, .funs = list(ST1 = ~lag(., n_osm_id)))
# filter out 2008
data_model <- data_model %>%
  filter(year != 2008)
# create spatial weights matrix
cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
cont.listw <- nb2listw(cont.nb, style="W")
# replicate spatial weights matrix to account for multiple years (spatial structure is constant)
# number of years
n_year <- length(unique(data_model$year))
# create new object
cont.listw.yearly <- cont.listw
# replicate neighbours and weights by number of years
cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
# assign the attributes from the original list
attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
attributes(cont.listw.yearly$weights) <- attributes(cont.listw$neighbours)
# replicate region IDs by number of years
attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
attributes(cont.listw.yearly$weights)$region.id <- rep(attributes(cont.listw.yearly$weights)$region.id, n_year)
# spatio-temporal lag
data_model <- data_model %>%
  mutate_at(
    paste(variables_temporal, 'ST1', sep='_'),
    .funs = ~lag.listw(cont.listw.yearly, .)
  )
write.csv2(data_model, 'data/data_model.csv')
View(data_model)
#formula
form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
  greenery + average_b_waw + check_in_ST1 + income_ST1 + kinder_ST1 + unempl_ST1 +
  pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
# model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
cont.listw.yearly
attr(cont.listw.yearly$weights, "mode")
attr(cont.listw$weights, "mode")
attributes(cont.listw.yearly$neighbours)
# filter out 2019 since it only contains target variable
data_model <- data %>%
  filter(year != 2019)
# some variables do not change over time, and thus we should find only spatial lags for those
variables_constant <- c('bu', 'bur', 'br', 'dist', 'train')
# let's double check that these do not change
data_model %>%
  dplyr::select(borough, year, variables_constant) %>%
  group_by(borough) %>%
  summarise_at(variables_constant, n_distinct) %>%
  filter_at(variables_constant, any_vars(. != 1))
# now let's select the variables that do change over time
variables_temporal <- c('check_in', 'income', 'kinder', 'unempl', 'pop_density', 'greenery', 'average_b_waw')
# let's double check that these do change
data_model %>%
  dplyr::select(borough, year, variables_temporal) %>%
  group_by(borough) %>%
  summarise_at(variables_temporal, n_distinct) %>%
  filter_at(variables_temporal, any_vars(. == 1))
# add osm_id and select columns for modeling
data_model <- data_model %>%
  merge(boroughs_osm, by='borough') %>%
  dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], variables_temporal)
# add lagged variables
# number of boroughs
n_osm_id <- length(unique(data_model$osm_id))
# temporal lag
data_model <- data_model %>%
  arrange(year, osm_id) %>%
  mutate_at(variables_temporal, .funs = list(ST1 = ~lag(., n_osm_id)))
# filter out 2008
data_model <- data_model %>%
  filter(year != 2008)
# create spatial weights matrix
cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
cont.listw <- nb2listw(cont.nb, style="W")
# replicate spatial weights matrix to account for multiple years (spatial structure is constant)
# number of years
n_year <- length(unique(data_model$year))
# create new object
cont.listw.yearly <- cont.listw
# replicate neighbours and weights by number of years
cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
# assign the attributes from the original list
attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
attributes(cont.listw.yearly$weights) <- attributes(cont.listw$weights)
# replicate region IDs by number of years
attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
attributes(cont.listw.yearly$weights)$region.id <- rep(attributes(cont.listw.yearly$weights)$region.id, n_year)
# spatio-temporal lag
data_model <- data_model %>%
  mutate_at(
    paste(variables_temporal, 'ST1', sep='_'),
    .funs = ~lag.listw(cont.listw.yearly, .)
  )
write.csv2(data_model, 'data/data_model.csv')
#formula
form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
  greenery + average_b_waw + check_in_ST1 + income_ST1 + kinder_ST1 + unempl_ST1 +
  pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
# model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
data_model
# model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
#formula
form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
  greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
  pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
# model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
summary(model.sac2)
attr(cont.listw.yearly$weights, "mode")
model.sac3<-errorsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
data_model
#formula
form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
  greenery + average_b_waw
# model
model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
attributes(cont.listw.yearly$weights)
# replicate spatial weights matrix to account for multiple years (spatial structure is constant)
# number of years
n_year <- length(unique(data_model$year))
# create new object
cont.listw.yearly <- cont.listw
# replicate neighbours and weights by number of years
cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
attributes(cont.listw.yearly$weights)
# assign the attributes from the original list
attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
attributes(cont.listw.yearly$weights) <- attributes(cont.listw$weights)
attributes(cont.listw.yearly$weights)
attributes(cont.listw$weights)
# replicate spatial weights matrix to account for multiple years (spatial structure is constant)
# number of years
n_year <- length(unique(data_model$year))
# create new object
cont.listw.yearly <- cont.listw
# create spatial weights matrix
cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
cont.listw <- nb2listw(cont.nb, style="W")
attributes(cont.listw$weights
           attributes(cont.listw$weights)
           attributes(cont.listw$weights)
           attributes(cont.listw$weights)$comp
           length(attributes(cont.listw$weights)$comp)
           length(attributes(cont.listw$weights)$comp$d
                  length(attributes(cont.listw$weights)$comp$d)
                  length(attributes(cont.listw$weights)$comp$d)
                  # 4. MODEL DATA PREPARATION
                  # filter out 2019 since it only contains target variable
                  data_model <- data %>%
                    filter(year != 2019)
                  # some variables do not change over time, and thus we should find only spatial lags for those
                  variables_constant <- c('bu', 'bur', 'br', 'dist', 'train')
                  # let's double check that these do not change
                  data_model %>%
                    dplyr::select(borough, year, variables_constant) %>%
                    group_by(borough) %>%
                    summarise_at(variables_constant, n_distinct) %>%
                    filter_at(variables_constant, any_vars(. != 1))
                  # now let's select the variables that do change over time
                  variables_temporal <- c('check_in', 'income', 'kinder', 'unempl', 'pop_density', 'greenery', 'average_b_waw')
                  # let's double check that these do change
                  data_model %>%
                    dplyr::select(borough, year, variables_temporal) %>%
                    group_by(borough) %>%
                    summarise_at(variables_temporal, n_distinct) %>%
                    filter_at(variables_temporal, any_vars(. == 1))
                  # add osm_id and select columns for modeling
                  data_model <- data_model %>%
                    merge(boroughs_osm, by='borough') %>%
                    dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], variables_temporal)
                  # add lagged variables
                  # number of boroughs
                  n_osm_id <- length(unique(data_model$osm_id))
                  # temporal lag
                  data_model <- data_model %>%
                    arrange(year, osm_id) %>%
                    mutate_at(variables_temporal, .funs = list(ST1 = ~lag(., n_osm_id)))
                  # filter out 2008
                  data_model <- data_model %>%
                    filter(year != 2008)
                  # create spatial weights matrix
                  cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
                  cont.listw <- nb2listw(cont.nb, style="W")
                  # replicate spatial weights matrix to account for multiple years (spatial structure is constant)
                  # number of years
                  n_year <- length(unique(data_model$year))
                  # create new object
                  cont.listw.yearly <- cont.listw
                  # replicate neighbours and weights by number of years
                  cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
                  cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
                  # assign the attributes from the original list
                  attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
                  attributes(cont.listw.yearly$weights) <- attributes(cont.listw$weights)
                  # replicate attributes by number of years
                  attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
                  attributes(cont.listw.yearly$weights)$comp$d <- rep(attributes(cont.listw.yearly)$comp$d, n_year)
                  # spatio-temporal lag
                  data_model <- data_model %>%
                    mutate_at(
                      paste(variables_temporal, 'ST1', sep='_'),
                      .funs = ~lag.listw(cont.listw.yearly, .)
                    )
                  write.csv2(data_model, 'data/data_model.csv')
                  #formula
                  form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
                  # model
                  model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  attributes(cont.listw.yearly$weights)$comp$d
                  # create spatial weights matrix
                  cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
                  cont.listw <- nb2listw(cont.nb, style="W")
                  # replicate spatial weights matrix to account for multiple years (spatial structure is constant)
                  # number of years
                  n_year <- length(unique(data_model$year))
                  # create new object
                  cont.listw.yearly <- cont.listw
                  # replicate neighbours and weights by number of years
                  cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
                  cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
                  # assign the attributes from the original list
                  attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
                  attributes(cont.listw.yearly$weights) <- attributes(cont.listw$weights)
                  # replicate attributes by number of years
                  attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
                  attributes(cont.listw.yearly$weights)
                  attributes(cont.listw.yearly$weights)$comp$d
                  rep(attributes(cont.listw.yearly)$comp$d, n_year)
                  attributes(cont.listw.yearly)$comp$d
                  attributes(cont.listw.yearly$weights)$comp$d
                  rep(attributes(cont.listw.yearly$weights)$comp$d, n_year)
                  attributes(cont.listw.yearly$weights)$comp$d <- rep(attributes(cont.listw.yearly$weights)$comp$d, n_year)
                  # spatio-temporal lag
                  data_model <- data_model %>%
                    mutate_at(
                      paste(variables_temporal, 'ST1', sep='_'),
                      .funs = ~lag.listw(cont.listw.yearly, .)
                    )
                  # filter out 2019 since it only contains target variable
                  data_model <- data %>%
                    filter(year != 2019)
                  # some variables do not change over time, and thus we should find only spatial lags for those
                  variables_constant <- c('bu', 'bur', 'br', 'dist', 'train')
                  # let's double check that these do not change
                  data_model %>%
                    dplyr::select(borough, year, variables_constant) %>%
                    group_by(borough) %>%
                    summarise_at(variables_constant, n_distinct) %>%
                    filter_at(variables_constant, any_vars(. != 1))
                  # now let's select the variables that do change over time
                  variables_temporal <- c('check_in', 'income', 'kinder', 'unempl', 'pop_density', 'greenery', 'average_b_waw')
                  # let's double check that these do change
                  data_model %>%
                    dplyr::select(borough, year, variables_temporal) %>%
                    group_by(borough) %>%
                    summarise_at(variables_temporal, n_distinct) %>%
                    filter_at(variables_temporal, any_vars(. == 1))
                  # add osm_id and select columns for modeling
                  data_model <- data_model %>%
                    merge(boroughs_osm, by='borough') %>%
                    dplyr::select(osm_id, year, variables_constant[variables_constant!='br'], variables_temporal)
                  # add lagged variables
                  # number of boroughs
                  n_osm_id <- length(unique(data_model$osm_id))
                  # temporal lag
                  data_model <- data_model %>%
                    arrange(year, osm_id) %>%
                    mutate_at(variables_temporal, .funs = list(ST1 = ~lag(., n_osm_id)))
                  # filter out 2008
                  data_model <- data_model %>%
                    filter(year != 2008)
                  # create spatial weights matrix
                  cont.nb <- poly2nb(as(locs$osm_multipolygons, 'Spatial'))
                  cont.listw <- nb2listw(cont.nb, style="W")
                  # replicate spatial weights matrix to account for multiple years (spatial structure is constant)
                  # number of years
                  n_year <- length(unique(data_model$year))
                  # create new object
                  cont.listw.yearly <- cont.listw
                  # replicate neighbours and weights by number of years
                  cont.listw.yearly$neighbours <- rep(cont.listw.yearly$neighbours, n_year)
                  cont.listw.yearly$weights <- rep(cont.listw.yearly$weights, n_year)
                  # assign the attributes from the original list
                  attributes(cont.listw.yearly$neighbours) <- attributes(cont.listw$neighbours)
                  attributes(cont.listw.yearly$weights) <- attributes(cont.listw$weights)
                  # replicate attributes by number of years
                  attributes(cont.listw.yearly$neighbours)$region.id <- rep(attributes(cont.listw.yearly$neighbours)$region.id, n_year)
                  attributes(cont.listw.yearly$weights)$comp$d <- rep(attributes(cont.listw.yearly$weights)$comp$d, n_year)
                  # spatio-temporal lag
                  data_model <- data_model %>%
                    mutate_at(
                      paste(variables_temporal, 'ST1', sep='_'),
                      .funs = ~lag.listw(cont.listw.yearly, .)
                    )
                  write.csv2(data_model, 'data/data_model.csv')
                  #formula
                  form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
                  # model
                  model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  # model
                  model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  str(cont.listw.yearly)
                  str(cont.listw)
                  attributes(cont.listw.yearly)
                  attributes(cont.listw.yearly)$region.id <- rep(attributes(cont.listw.yearly)$region.id, n_year)
                  str(cont.listw.yearly)
                  str(cont.listw)
                  # formula
                  form <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
                  # model
                  model.sac2<-sacsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  summary(model.sac2)
                  model.sac3<-errorsarlm(form, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  summary(model.sac3)
                  # formula
                  form1 <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw
                  form2 <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
                  form3 <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1 + check_in_ST
                  model.ols<-lm(form1, data=sub, tol.solve=3e-30)
                  model.ols<-lm(form1, data=data_model, tol.solve=3e-30)
                  model.ols<-lm(form1, data=data_model)
                  summary(model.ols)
                  model.sac2<-sacsarlm(form2, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  summary(model.sac2)
                  model.sac3<-errorsarlm(form3, data=data_model, cont.listw.yearly, tol.solve=3e-30)
                  summary(model.sac3)
                  AIC(model.ols)
                  AIC(model.sac3)
                  AIC(model.ols)
                  AIC(model.sac2)
                  AIC(model.sac3)
                  summary(model.sac2)
                  summary(model.sac3)
                  model.sac2<-sacsarlm(form2, data=data_model, cont.listw.yearly, tol.solve=3e-30 , type = 'sacmixed')
                  summary(model.sac2)
                  # formula
                  form_basic <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw
                  # formula
                  form_basic <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw
                  form_st <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
                  form_st_durbin <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1 + check_in_ST
                  form_durbin <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + check_in_ST
                  # formula without lags
                  form_basic <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw
                  # formula with spatio-temporal lags
                  form_st <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1
                  # formula with spatio-temporal lags & durbin component
                  form_st_durbin <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + income_ST1 + kinder_ST1 + unempl_ST1 +
                    pop_density_ST1 + greenery_ST1 + average_b_waw_ST1 + check_in_ST
                  # formula with durbin component
                  form_durbin <- check_in ~ bu + bur + dist + train + income + kinder + unempl + pop_density +
                    greenery + average_b_waw + check_in_ST
                  # GNS
                  model.gns<-sacsarlm(form_basic, data=data_model, cont.listw.yearly, tol.solve=3e-30 , type = 'sacmixed')
                  summary(model.gns)
                  # define plot function to compare 2 years
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
                  # plot 4 most important merics - 2008 vs 2018
                  plt_check_in <- plot_metrics_compare(check_in, 2008, 2018, 'a) migrants', 'Data Source: Polish Statistical Office', '# of migrants', c(1, 2, 5, 10, 25, 50, 100, 250, 750))
                  
# list and load/install packages
requiredPackages <- c('tidyverse', 'haven', 'readxl', 'utf8', 'rjson', 'osmdata', 'sf', 'sp', 'broom', 'viridis', 'cowplot', 'reshape2', 'spatialreg', 'spdep')
for (i in requiredPackages) {
  if (!require(i,character.only = T)) {
    install.packages(i, character.only = T)
  }
}

plt_check_in
plt_pop_density <- plot_metrics_compare(pop_density, 2008, 2018, 'b) population density', 'Data Source: Polish Statistical Office', '', c(0.5, 1, 2, 3, 5, 7, 10, 15, 20, 30)) +
  labs(fill = expression(bold(paste('# of people / ', km^2))))
plt_dist <- plot_metrics_compare(dist, 2008, 2018, 'c) distance', 'Data Source: Google Maps', 'km', c(10, 12, 15, 20, 25, 30, 40, 50))
plt_income <- plot_metrics_compare(income, 2008, 2018, 'd) income per capita', 'Data Source: Polish Statistical Office', 'ratio', c(0.3, 0.5, 0.75, 1, 1.5, 2, 2.5, 3))
plt_comparison
                  