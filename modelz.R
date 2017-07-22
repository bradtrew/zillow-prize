library(xgboost)
library(tidyverse)
library(lubridate)
transactions <- read_csv("~/Documents/Kaggle/zillow/data/train_2016_v2.csv")

properties <- read_csv("~/Documents/Kaggle/zillow/data/properties_2016.csv")

properties <- properties %>% rename(
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
transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

transactions %>% mutate(month = as.factor(month(date))) %>% lm(logerror~month, data = .) %>% 
  summary


properties %>% 
  inner_join(transactions) %>% 
  nrow

properties %>%
  inner_join(transactions) %>% 
  group_by(zoning_property) %>% 
  summarise(n=n(),
            var = var(logerror),
            mean = mean(logerror)) %>%
  arrange(desc(n), desc(var)) %>% View


properties %>% inner_join(transactions) %>% group_by(region_zip) %>% 
  summarise(n()) %>% View


library(leaflet)

properties %>% filter(region_zip==97319) %>% inner_join(transactions) %>% 
  summarise(max(logerror), min(logerror), mean(logerror))
arrange(longitude) %>%
  ggplot(aes(y= latitude, x=longitude, color=logerror)) + geom_point() +
  scale_color_continuous(limits = c())


properties %>% filter(region_zip==97328) %>% inner_join(transactions) %>% 
  ggplot(aes(y= latitude, x=longitude, color=logerror)) + geom_point() +
  scale_color_continuous(limits = c())


96987
properties %>% filter(region_zip==97319) %>%  inner_join(transactions) %>% .[668,] %>%  pull(logerror)



properties %>% filter(region_zip==97328) %>% inner_join(transactions) %>%  .[83,] %>% 
  pull(logerror)
pull(latitude) %>% which.min
%>%  pull(latitude) %>% which.min


properties %>% filter(region_zip==97328) %>% 
  inner_join(transactions) %>% summarise(sd(logerror))





# distance from centre of zip/n'hood/ - flag if more than 2 stds of average - mahalanobis


library(cluster)
?mahalanobis

prop97328 <- properties %>% filter(region_zip==97328)  %>% inner_join(transactions)

S <- matrix(c(var(prop97328$latitude),
              cov(prop97328$latitude,prop97328$longitude),
              cov(prop97328$latitude,prop97328$longitude),
              var(prop97328$longitude)
), nrow = 2, byrow = TRUE)
mu <- c(mean(prop97328$latitude), mean(prop97328$longitude))


detect_outliers <- function(dat) {
  if(nrow(dat) <10) {
    dat$is_outlier <- FALSE
    dat$mahal_dist <- rep(NA,nrow(dat))
    return(dat)
  }
  if((var(dat$latitude) + var(dat$longitude)) < 10){
    dat$is_outlier <- FALSE
    dat$mahal_dist <- rep(NA,nrow(dat))
    return(dat)
  }
  S <- matrix(c(var(dat$latitude),
                cov(dat$latitude,  dat$longitude),
                cov(dat$latitude, dat$longitude),
                var(dat$longitude)
  ), nrow = 2, byrow = TRUE)
  mu <- c(mean(dat$latitude), mean(dat$longitude))
  dists <- mahalanobis(cbind(dat$latitude, dat$longitude), center = mu, cov = S)  
  sd_mh <- sd(dists)
  avg_mh <- mean(dists)
  ub <- avg_mh + 3*sd_mh
  dat$is_outlier <- (abs(dists) > ub)
  dat$mahal_dist <- dists
  dat
}

props_with_outliers <- properties %>% 
  split(.$region_zip) %>% 
  map(detect_outliers) %>% 
  do.call('rbind', .)


props_with_outliers %>% inner_join(transactions) %>% 
  filter(is_outlier==1) %>% 
  summarise(mean(logerror), median(logerror), mean(abs(logerror)), n())








