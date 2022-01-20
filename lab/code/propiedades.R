library(readxl)
library(dplyr)
library(sf)
library(tmap)
library(spgwr)
library(GGally)
library(tidymodels)
library(vip)
propie <- read_xlsx("lab/data/data.xlsx")
summary(propie)
propie <- filter(propie, !is.na(latitud) & longitud > -80 & longitud < -70 & latitud < -34) %>%
  # st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
  mutate(bien_casa = if_else(tipo_bien == "Casa", 1, 0))
map <- tm_shape(propie) +
  tm_dots(col = "tot_constr_m2")
tmap_leaflet(map)
##############darle centroide comuna
#################análisis
ggpairs(select_if(st_drop_geometry(propie), is.numeric))
##############probar modelo lineal
set.seed(123)
dt_split <- initial_split(propie, p = 3/4, strata = comuna)
dt_training <- training(dt_split)
dt_testing <- testing(dt_split)
#model
lm_model <- linear_reg() %>% 
  set_engine('lm') %>% # adds lm implementation of linear regression
  set_mode('regression')
lm_fit <- lm_model %>%
  fit(total_uf ~ tot_constr_m2 + tot_terreno_m2 + num_estacionam, 
      data = dt_training)
 summary(lm_fit$fit)
vip(lm_fit) 
glance(lm_fit)
m1 <- rand_forest(trees = 3000) %>%
  set_engine("ranger") %>% 
  set_mode("regression")
m2 <- linear_reg() %>%
  set_engine('lm') %>%
  set_mode('regression')
#set recipe ----
m_recipe <- recipe(total_uf ~ tot_constr_m2 + tot_terreno_m2 + num_estacionam, data = dt_training) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())
#create workflow ----
m_wkf <- workflow() %>%
  add_model(m1) %>%
  add_recipe(m_recipe)
m_wkf <- workflow() %>%
  add_model(m2) %>%
  add_recipe(m_recipe)

#fit model ----
set.seed(234)
m_fit <- m_wkf %>%
  fit(dt_training)
summary(m_fit$fit$fit$fit)
# vip(m_fit)
# glance(m_fit)
#prediction ----
m_train_pred <- predict(m_fit, dt_training) %>%
  bind_cols(dt_training)
ggplot(aes(.pred), data = m_train_pred) +
  geom_histogram()
select(m_train_pred, total_uf, .pred) %>%
  ggplot(aes(.pred, total_uf)) +
  geom_point() + 
  geom_smooth(formula= y~x)

m_test_pred <- predict(m_fit, dt_testing) %>%
  bind_cols(dt_testing)
ggplot(aes(.pred), data = m_test_pred) +
  geom_histogram()
select(m_test_pred, total_uf, .pred) %>%
  ggplot(aes(.pred, total_uf)) +
  geom_point() + 
  geom_smooth(formula= y~x)
####################aplicar técnicas de vaidación cruzada
######################modelo elástico
############################################GWR
propie2 <- filter(propie, !is.na(latitud) & longitud > -80 & longitud < -70 & latitud < -34) %>%
  st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>%
  as_Spatial()
#bandwidth
bwG <- gwr.sel(total_uf ~ tot_constr_m2 + tot_terreno_m2 + num_estacionam, 
               data = propie2, 
               gweight = gwr.Gauss,
               verbose = FALSE)
#model
gwrG <- gwr(total_uf ~ tot_constr_m2 + tot_terreno_m2 + num_estacionam, 
            data = propie2, 
            bandwidth = bwG,
            gweight = gwr.Gauss, 
            hatmatrix = TRUE)

results<-as.data.frame(gwrG$SDF)
head(results)

propie2$coeffpred <- results$pred
propie2$coeffconst <- results$tot_constr_m2
propie2$coeffterr <- results$tot_terreno_m2
propie2$coeffest <- results$num_estacionam

propie2 <- st_as_sf(propie2)

map <- tm_shape(propie2, name = "pred") +
  tm_dots(col = "coeffpred") +
  tm_shape(propie2, name = "const" ) +
  tm_dots(col = "coeffconst") +
  tm_shape(propie2, name = "terr" ) +
  tm_dots(col = "coeffterr") +
  tm_shape(propie2, name = "est" ) +
  tm_dots(col = "coeffest")
  
tmap_leaflet(map)