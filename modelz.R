library(cluster)
library(xgboost)
library(lubridate)
library(tidyverse)
library(FeatureHashing)
transactions <- read_csv("~/Documents/Kaggle/zillow/data/train_2016_v2.csv")

temp_dist <- read_csv("~/Documents/Kaggle/zillow/data/properties_2016.csv")

temp_dist <- temp_dist %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% dplyr::rename(
  id_parcel = parcelid,
  date = transactiondate
)

temp_dist <- temp_dist %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))





detect_outliers <- function(dat) {
  if(nrow(dat) <50) {
    dat$is_outlier <- 0
    dat$mahal_dist <- 0
    return(dat)
  }
  if((var(dat$latitude, na.rm = TRUE) + var(dat$longitude, na.rm = TRUE)) < 50) {
    dat$is_outlier <- 0
    dat$mahal_dist <- 0
    return(dat)
  }
  lat_and_lon <- cbind(dat$latitude,dat$longitude)
  S <- cov(lat_and_lon)
  mu <- colMeans(lat_and_lon)
  dists <- mahalanobis(lat_and_lon, center = mu, cov = S)  
  sd_mh <- sd(dists)
  avg_mh <- mean(dists)
  ub <- avg_mh + 3*sd_mh
  dat$mahal_dist <- dists
  dat$is_outlier <- (abs(dists) > ub)
  dat
}

new_dat <- temp_dist %>% 
  mutate(region_zip = ifelse(is.na(region_zip), 0, region_zip)) %>% 
  split(.$region_zip) %>% 
  map(detect_outliers) %>% 
  do.call('rbind', .)




# reg95982 <- new_dat %>% inner_join(transactions, by = "id_parcel") %>% filter(region_zip == 95982)  
#   
# lat_and_lon <- cbind(reg95982$latitude,reg95982$longitude)
# S <- cov(lat_and_lon)
# mu <- colMeans(lat_and_lon)
# dists <- mahalanobis(lat_and_lon, center = mu, cov = S)  
# mean(dists)
# max(dists)
# min(dists)


# has a shed or not
# new_dat %>% mutate(has_shed = !is.na(area_shed)*1,
#                    has_patio = !is.na(area_patio)*1,
#                    has_fireplace = !is.na(flag_fireplace)*1,
#                    has_pool = !is.na(num_pool)*1,
#                    pool_type = as.factor(case_when(!is.na(pooltypeid10) ~ "spa or hot tub",
#                                          !is.na(pooltypeid2) ~ "pool and tub",
#                                          !is.na(pooltypeid7) ~ "pool only",
#                                          TRUE ~ "nothing")),
#                    tax_delinquency = 1*tax_delinquency)
# 

tmp[is.na(tmp)] <- 0

na_to_zero <- function(x){
  x[is.na(x)] <- 0
  x
}

new_dat <- new_dat %>% mutate(needs_pred = 1)
transactions <- transactions %>% mutate(is_train =1)

complete_obs <- right_join(transactions, new_dat, by = "id_parcel")

labels <- complete_obs$logerror
datez <- complete_obs$date
ids <- complete_obs$id_parcel
complete_obs$row_number <- 1:nrow(complete_obs)
has_label <- complete_obs %>% 
  filter(!is.na(logerror)) %>% pull(row_number)

complete_obs <- complete_obs %>% select(-date)
complete_obs[is.na(complete_obs)] <- 0
removed_feats <- complete_obs %>% 
  mutate_at(vars(architectural_style, pooltypeid10,
                 pooltypeid2,zoning_landuse_county,
                 material, zoning_property, fips), as.factor) %>% 
  select(-latitude, - longitude, -id_parcel,
         -tax_delinquency_year, - rawcensustractandblock,
         -censustractandblock, -zoning_property, -region_neighbor, -region_zip) 



removed_feats %>% mutate_if(is.factor, as.numeric) %>% str
 
train_inds <- sample(seq_len(nrow(removed_feats)), size = nrow(removed_feats) * 0.5)

hashed_df <- hashed.model.matrix( logerror ~ . -1, data = as.data.frame(removed_feats), hash.size = 2^17)

train_hashed <- hashed_df[has_label,]
# val_hashed <- hashed_df[-train_inds,]
train_label <- labels[has_label]
# val_label <- labels[-train_inds]




dtrain <- xgb.DMatrix(train_hashed, label = train_label)
dval <- xgb.DMatrix(val_hashed, label = val_label)




param <- list(objective = "reg:linear", 
              eval_metric = "rmse",
              eta = 0.1,
              gamma = 1,
              subsample = 0.7,
              colsample_bytree = 0.7,
              min_child_weight = 0,
              max_depth = 14)


set.seed(120)

m2 <- xgb.train(data = dtrain,
                param,
                nrounds = 50,
                watchlist = list(val = dval,model = dtrain),
                nthread=4)



xgb_grid_1 <- expand.grid(eta = c(0.1),
                        max_depth = c(2,10),
                        gamma= c(1,0.1),
                        colsample_bytree = 0.7,
                        subsample = 0.7,
                        min_child_weight = c(0,5)
)


paramFrame <- split(xgb_grid_1, 1:nrow(xgb_grid_1))


xgb.error <- function(err_func) {
  return(function(preds, dtrain) {
    labels <- xgboost::getinfo(dtrain, "label")
    if (length(unique(labels))<3) {
      preds <- thresh(preds)
    }
    err <- err_func(labels, preds)
    return(list(metric = "error", value = err))
  })
}
mae <- function(actual, preds) {
  mean(abs(actual - preds))
}


print_params <- function(paramz) paste0(paste0(names(paramz),"=", paramz), collapse = ", ")
models <- map(paramFrame, function(paramz) {
  cat("Training using,", print_params(paramz), "\n")
  return(xgb.train(data=dtrain,
                   param=paramz,
                   nthread = 4,
                   nrounds = 150,
                   objective = "reg:linear",
                   eval_metric = xgb.error(mae),
                   watchlist = list(eval = dval, 
                                    model = dtrain)))
})

val_preds <- map(models, function(model) {
  predict(model, dval)
})


map(val_preds, function(preds) { 
  mae(val_label, preds)
  })

sample_sub <- read_csv("~/Documents/Kaggle/zillow/sample_submission.csv", n_max = 10)
temp_dist %>% nrow

preds <- data.frame(id_parcel = complete_obs$id_parcel[-has_label],
               logerror = predict(models[[5]], xgb.DMatrix(hashed_df[-has_label,])))
out_errors <- bind_rows(preds, transactions %>% select(id_parcel, logerror))

unique_preds <- out_errors %>% group_by(id_parcel) %>%
  dplyr::summarise(logerror = mean(logerror)) %>% 
  transmute(ParcelId = id_parcel,
            `201610` = logerror,
            `201611` = logerror,
            `201612` = logerror,
            `201710` = logerror,
            `201711` = logerror,
            `201712` = logerror)
write_csv(unique_preds, "~/Documents/Kaggle/zillow/preds/first_try.csv")










