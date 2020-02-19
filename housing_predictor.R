#2/7/20

#LOAD IN PACKAGES
{
  install.packages("dplyr")
  install.packages("plyr")
  install.packages("ggplot2")
  install.packages("magrittr")
  install.packages("reshape")
  install.packages("lubridate")
  install.packages("forecast")
  install.packages("revgeo")
  install.packages("xgboost")
  install.packages("Ckmeans.1d.dp")
  install.packages("DiagrammeR")
  
  require(dplyr)
  require(plyr)
  require(ggplot2)
  require(magrittr)
  require(reshape)
  require(lubridate)
  require(forecast)
  require(revgeo)
  require(xgboost)
  require(Ckmeans.1d.dp)
  require(DiagrammeR)
}

#LOAD AND CREATE DATAFRAMES
{
  test_data_raw = read.csv("test.csv")
  train_data_raw = read.csv("train.csv")
  glimpse(test_data_raw)
  glimpse(train_data_raw)
}

#DATA WRANGLING
{
  test_data_working = test_data_raw %>% mutate(
    TransDate = as.Date(as.character(TransDate), format = "%m/%d/%Y"),
    BuiltYear = as.Date(ISOdate(BuiltYear, 1, 1)),
    ViewType = as.factor(ViewType),
    Latitude = Latitude / 1000000, 
    Longitude = Longitude / 1000000,
    
    #new columns
    propertyAgeYears = as.numeric(TransDate - BuiltYear) / 365
    
    #reverse transformation of long/lat took too long to run
    # temp_revgeo = revgeo(longitude = Longitude, latitude = Latitude, provider = 'photon', output = 'frame'),
    # housenumber = temp_revgeo$housenumber,
    # street = temp_revgeo$street,
    # city = temp_revgeo$city,
    # state = temp_revgeo$state,
    # zip = temp_revgeo$zip,
    # country = temp_revgeo$country
  
  ) %>% mutate(
    temp_revgeo = NULL #get rid of our temp variable
  )
  
  train_data_working = train_data_raw %>% mutate(
    TransDate = as.Date(as.character(TransDate), format = "%m/%d/%Y"),
    BuiltYear = as.Date(ISOdate(BuiltYear, 1, 1)),
    ViewType = as.factor(ViewType),
    Latitude = Latitude / 1000000, 
    Longitude = Longitude / 1000000,
    
    #new columns
    propertyAgeYears = as.numeric(TransDate - BuiltYear) / 365
    
    #reverse transformation of long/lat took too long to run
    # temp_revgeo = revgeo(longitude = Longitude, latitude = Latitude, provider = 'photon', output = 'frame'),
    # housenumber = temp_revgeo$housenumber,
    # street = temp_revgeo$street,
    # city = temp_revgeo$city,
    # state = temp_revgeo$state,
    # zip = temp_revgeo$zip,
    # country = temp_revgeo$country
  ) %>% mutate(
    temp_revgeo = NULL #get rid of our temp variable
  )
}

#TRANFORMATION OF LONG/LAT INTO CLUSTERS
{
  data_coord = rbind(test_data_working[,c("Longitude","Latitude")], train_data_working[,c("Longitude","Latitude")])
  sum_squares = c()
  for(i in 1:100){
    print(i)
    km_model = kmeans(data_coord, centers = i)  
    ss = km_model$tot.withinss #within-cluster sum of squares
    sum_squares = c(sum_squares, ss)
  }
  qplot(1:100, sum_squares, xlab = "Number of Clusters", 
        ylab = "Total Sum Square Distance to Center") #elbow method shows we should have about 10 clusters
  km_model_optimal = kmeans(data_coord, centers = 10)
  
  clusters = km_model_optimal$cluster
  test_data_working$long_lat_cluster = clusters[1:nrow(test_data_working)]
  train_data_working$long_lat_cluster = clusters[(nrow(test_data_working)+1):length(clusters)]
  
  qplot(data_coord$Longitude, data_coord$Latitude, col = clusters)
  ggplot(data = data_coord, aes(Longitude, Latitude)) +
    geom_point(aes(col = as.factor(clusters))) + 
    scale_color_brewer(palette = "Paired") +
    labs(title = 'Clusters of Regions in King County, Seattle (k=10)')
  }

#INITIAL TRAIN DATA EXPLORATION
{
  glimpse(train_data_working)
  colnames(train_data_working)
  
  #PropertyID
  qplot(train_data_working$PropertyID)
  summary(train_data_working$PropertyID)
  nrow(train_data_working) - length(unique(train_data_working$PropertyID)) #every row is unique property id
  nrow(test_data_working) - length(unique(test_data_working$PropertyID)) #every row is unique property id
  
  #SaleDollarCnt (Independent variable)
  qplot(train_data_working$SaleDollarCnt/1000000, bins = 50, xlab = 'Price ($ Millions)', ylab = 'Count', main = 'Distribution of Prices')
  qplot(log(train_data_working$SaleDollarCnt), bins = 50, xlab = 'ln(Price)', ylab = 'Count', main = 'Distribution of ln(Price)') #resembles normal distribution
  summary(train_data_working$SaleDollarCnt)
  sd(train_data_working$SaleDollarCnt)
  summary(log(train_data_working$SaleDollarCnt))
  
  #TransDate
  summary(train_data_working$TransDate)
  qplot(train_data_working$TransDate, log(train_data_working$SaleDollarCnt), alpha = .1) #checking for autocorrelation/ time trends
  
  summary(test_data_working$TransDate)
  qplot(test_data_working$TransDate, log(train_data_working$SaleDollarCnt), alpha = .1) #checking for autocorrelation/ time trends
  
  #censusblockgroup
  summary(train_data_working$censusblockgroup)
  length(unique(train_data_working$censusblockgroup))
  
  #ZoneCodeCountry
  summary(train_data_working$ZoneCodeCounty)
  
  #Usecode
  summary(train_data_working$Usecode)
  length(unique(train_data_working$Usecode))
  
  #BedroomCnt
  qplot(train_data_working$BedroomCnt)
  summary(train_data_working$BedroomCnt)
  qplot(train_data_working$BedroomCnt, log(train_data_working$SaleDollarCnt))
  
  #BathroomCnt
  qplot(train_data_working$BathroomCnt)
  summary(train_data_working$BathroomCnt)
  qplot(train_data_working$BathroomCnt, log(train_data_working$SaleDollarCnt))
  
  #FinishedSquareFeet
  qplot(train_data_working$FinishedSquareFeet)
  summary(train_data_working$FinishedSquareFeet)
  qplot(train_data_working$FinishedSquareFeet, log(train_data_working$SaleDollarCnt), 
        xlab = 'Finished Square Feet (Area in Feet)', ylab = 'lnPrice')
  
  #GarageSquareFeet
  qplot(train_data_working$GarageSquareFeet)
  summary(train_data_working$GarageSquareFeet)
  qplot(train_data_working$GarageSquareFeet, log(train_data_working$SaleDollarCnt))
  
  #LotSizeSquareFeet
  qplot(train_data_working$LotSizeSquareFeet)
  summary(train_data_working$LotSizeSquareFeet)
  qplot(train_data_working$LotSizeSquareFeet, log(train_data_working$SaleDollarCnt))
  
  #StoryCnt
  qplot(train_data_working$StoryCnt)
  summary(train_data_working$StoryCnt)
  qplot(train_data_working$StoryCnt, log(train_data_working$SaleDollarCnt))
  
  #BuiltYear
  qplot(train_data_working$BuiltYear)
  summary(train_data_working$BuiltYear)
  qplot(train_data_working$BuiltYear, log(train_data_working$SaleDollarCnt))
  
  #ViewType
  qplot(train_data_working$ViewType)
  summary(train_data_working$ViewType)
  
  #Latitude/Longitude
  ggplot(data = train_data_working, aes(Longitude, Latitude)) + #FIGURE 1
    geom_point(aes(color = log(SaleDollarCnt)), alpha = .3) +
    scale_color_gradientn(colours = rev(rainbow(5)))+
    labs(col = "ln(Price)", title = "Housing Prices in King County, Washington")
  summary(train_data_working$Latitude)
  
  #street
  summary(train_data_working$street)
  
  #city
  qplot(train_data_working$city)
  summary(train_data_working$city)
  
  #state
  qplot(train_data_working$state)
  summary(train_data_working$state)
  
  #zip
  summary(train_data_working$zip)
  
  #country
  summary(train_data_working$country)
  
  #BGMedHomeValue
  qplot(train_data_working$BGMedHomeValue)
  summary(train_data_working$BGMedHomeValue)
  qplot(train_data_working$BGMedHomeValue/1000000, log(train_data_working$SaleDollarCnt), 
        xlab = 'Median home value in block group ($ Millions)', ylab = 'lnPrice')
  
  #BGMedRent
  qplot(train_data_working$BGMedRent)
  summary(train_data_working$BGMedRent)
  qplot(train_data_working$BGMedRent, log(train_data_working$SaleDollarCnt))
  
  #BGMedYearBuilt
  qplot(train_data_working$BGMedYearBuilt)
  summary(train_data_working$BGMedYearBuilt)
  
  #BGPctOwn
  qplot(train_data_working$BGPctOwn)
  summary(train_data_working$BGPctOwn)
  qplot(train_data_working$BGPctOwn, log(train_data_working$SaleDollarCnt))
  
  #BGPctVacant
  qplot(train_data_working$BGPctVacant)
  summary(train_data_working$BGPctVacant)
  qplot(train_data_working$BGPctVacant, log(train_data_working$SaleDollarCnt))
  
  #BGMedIncome
  qplot(train_data_working$BGMedIncome)
  summary(train_data_working$BGMedIncome)
  qplot(train_data_working$BGMedIncome, log(train_data_working$SaleDollarCnt))
  
  #BGPctKids
  qplot(train_data_working$BGPctKids)
  summary(train_data_working$BGPctKids)
  qplot(train_data_working$BGPctKids, log(train_data_working$SaleDollarCnt))
  
  #BGMedAge
  qplot(train_data_working$BGMedAge)
  summary(train_data_working$BGMedAge)
  qplot(train_data_working$BGMedAge, log(train_data_working$SaleDollarCnt))
  
  #propertyAgeYears
  qplot(train_data_working$propertyAgeYears)
  summary(train_data_working$propertyAgeYears)
  qplot(train_data_working$propertyAgeYears, log(train_data_working$SaleDollarCnt))
}

#EVAL METRIC FUNCTIONS
{
  calc_aape <- function(predicted, actual){
    return(mean(abs(predicted-actual) / actual))
  }
  calc_mape <- function(predicted, actual){
    return(median(abs(predicted-actual) / actual))
  } 
  calc_error_sd <- function(predicted, actual){
    return(sd(abs(predicted-actual) / actual))
  } 
}

#XGBOOST MODEL BUILDING
{
  #PREPARE MODEL DATA
  {
    train_data_model = train_data_working %>% mutate(
      PropertyID = NULL,
      censusblockgroup = NULL,
      Usecode = NULL,
      Latitude = NULL,
      Longitude = NULL,
      logSaleDollarCount = log(SaleDollarCnt),
      SaleDollarCnt = NULL
    ) 
    
    test_data_model = test_data_working %>% mutate(
      PropertyID = NULL,
      censusblockgroup = NULL,
      Usecode = NULL,
      Latitude = NULL,
      Longitude = NULL,
      logSaleDollarCount = log(SaleDollarCnt),
      SaleDollarCnt = NULL
    )
    index = 1:nrow(train_data_model)
    validation_split_indices = sample(index, trunc(length(index) / 4)) #take a random fourth of our test set as validation set
    validation_data_model = train_data_model[validation_split_indices,]
    train_subset_data_model = train_data_model[-validation_split_indices,]
    
    dtrain_df_full = list(x = data.matrix(train_data_model %>% select(-logSaleDollarCount)),
                            y = train_data_model$logSaleDollarCount)    
    dtrain_df = list(x = data.matrix(train_data_model[-validation_split_indices,] %>% select(-logSaleDollarCount)),
                            y = train_data_model$logSaleDollarCount[-validation_split_indices])
    dvalid_df = list(x = data.matrix(validation_data_model %>% select(-logSaleDollarCount)),
                            y = validation_data_model$logSaleDollarCount)
    dtest_df = list(x = data.matrix(test_data_model %>% select(-logSaleDollarCount)),
                            y = as.numeric(test_data_model$logSaleDollarCount))

    dtrain_full = xgb.DMatrix(data = dtrain_df_full$x, label = dtrain_df_full$y)
    dtrain = xgb.DMatrix(data = dtrain_df$x, label = dtrain_df$y)
    dvalidation = xgb.DMatrix(data = dvalid_df$x, label = dvalid_df$y)
    dtest = xgb.DMatrix(data = dtest_df$x, label = dtest_df$y)
    watchlist = list(train = dtrain, test = dvalidation)
  }
  
  #BUILD MODEL FROM OPTIMAL PARAMS
  {
    #CALIBRATE NROUNDS HYPERPARAM
    mape_temp = c()
    for(i in seq(from = 1, to = 121, by = 10)){
      working_model = xgb.train(params = list(), nrounds = i, dtrain, watchlist)
      pred_validation = predict(working_model, dvalidation)
      mape_temp = c(mape_temp,calc_mape(exp(pred_validation), exp(validation_data_model$logSaleDollarCount)))
    }
    qplot(seq(from=1, to = 121, by = 10), mape_temp, geom = 'line')
    
    nrounds_optimal = 50 #point in which mape basically stops decreasing
    working_model = xgb.train(params = list(), nrounds = nrounds_optimal, dtrain, watchlist)
  }
  
  #PREDICT USING TRAINED XGBOOST MODEL
  {
    pred_train = predict(working_model, dtrain_full)
    pred_validation = predict(working_model, dvalidation)
    
    qplot(exp(pred_train), exp(train_data_model$logSaleDollarCount))
    qplot(exp(pred_validation), exp(validation_data_model$logSaleDollarCount))
  }
  
  #EVAL METRICS
  {
    aape_xgboost = calc_aape(exp(pred_validation), exp(validation_data_model$logSaleDollarCount))
    mape_xgboost = calc_mape(exp(pred_validation), exp(validation_data_model$logSaleDollarCount))
    error_sd_xgboost = calc_error_sd(exp(pred_validation), exp(validation_data_model$logSaleDollarCount))
    cat("AAPE: ", aape_xgboost, "| MAPE: ", mape_xgboost, "| Error SD: ", error_sd_xgboost)
  }
  
  #PLOT FEATURE IMPORTANCE
  {
    importance = xgb.importance(feature_names = dimnames(dtrain_df$x)[[2]], model = working_model)
    list('model' = working_model, 'importance' = importance)
    
    xgb.ggplot.importance(
      importance_matrix = importance,
      top_n = 10,
      measure = NULL,
      rel_to_first = FALSE
    )
    # xgb.plot.tree(model = working_model, trees = 1)
  }
  
  #OUTPUT PREDICTION FOR TEST SET
  {
    pred_xgboost_testset = round(exp(predict(working_model, dtest)))
    qplot(pred_xgboost_testset, bins = 50)
    test_data_working$pred = pred_xgboost_testset
    output = test_data_working[,c("PropertyID", "pred")]
    write.csv(output, file = "predictions.csv", row.names = FALSE)
  }
}

#REGRESSION MODELS
{
  validation_data_model = train_data_model[validation_split_indices,]
  train_subset_data_model = train_data_model[-validation_split_indices,]
  
  #BGMedHomeValue
  #ln(SaleDollarCount) = beta * BGMedHomeValue + mu + error
  {
    qplot(train_data_working$BGMedHomeValue, log(train_data_working$SaleDollarCnt))
    
    x_temp = train_data_model$BGMedHomeValue 
    x_temp[is.na(x_temp)] = 0 #replace all NAs with 0
    
    y_temp = train_data_model$logSaleDollarCount
    y_temp[is.na(y_temp)] = 0
    
    lm_bgMedHomeValue = lm(y_temp ~ x_temp)
    summary(lm_bgMedHomeValue)
    
    pred_temp = as.data.frame(validation_data_model$BGMedHomeValue)
    pred_temp[is.na(pred_temp)] = 0
    
    pred_lm_bgMedHomeValue = predict(lm_bgMedHomeValue, pred_temp)
    summary(pred_lm_bgMedHomeValue)
    
    aape_lm_mhv = calc_aape(exp(pred_lm_bgMedHomeValue), exp(validation_data_model$logSaleDollarCount))
    mape_lm_mhv = calc_mape(exp(pred_lm_bgMedHomeValue), exp(validation_data_model$logSaleDollarCount))
    error_sd_lm_mhv = calc_error_sd(exp(pred_lm_bgMedHomeValue), exp(validation_data_model$logSaleDollarCount))
    cat("AAPE: ", aape_lm_mhv, "| MAPE: ", mape_lm_mhv, "| Error SD: " , error_sd_lm_mhv)
  }
  
  #FinishedSquareFeet
  #ln(SaleDollarCount) = beta * FinishedSquareFeet + mu + error
  {
    qplot(train_data_working$FinishedSquareFeet, log(train_data_working$SaleDollarCnt))
    
    x_temp = train_data_model$FinishedSquareFeet
    x_temp[is.na(x_temp)] = 0 #replace all NAs with 0
    
    y_temp = train_data_model$logSaleDollarCount
    y_temp[is.na(y_temp)] = 0
    
    lm_FinishedSquareFeet = lm(y_temp ~ x_temp)
    summary(lm_FinishedSquareFeet)
    
    pred_temp = as.data.frame(validation_data_model$FinishedSquareFeet)
    pred_temp[is.na(pred_temp)] = 0
    
    pred_lm_FinishedSquareFeet = predict(lm_FinishedSquareFeet, pred_temp)
    summary(pred_lm_FinishedSquareFeet)
    
    aape_lm_fsf = calc_aape(exp(pred_lm_FinishedSquareFeet), exp(validation_data_model$logSaleDollarCount))
    mape_lm_fsf = calc_mape(exp(pred_lm_FinishedSquareFeet), exp(validation_data_model$logSaleDollarCount))
    error_sd_lm_fsf = calc_error_sd(exp(pred_lm_FinishedSquareFeet), exp(validation_data_model$logSaleDollarCount))
    cat("AAPE: ", aape_lm_fsf, "| MAPE: ", mape_lm_fsf, "| Error SD: " , error_sd_lm_fsf)
  }
  
}

