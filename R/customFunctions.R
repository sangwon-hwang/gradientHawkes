# Ptproc 학습
learning <- function(data_frame_obj, begin_index, end_index, pstart, method_name, hessian_bool) {
  training_time <- data_frame_obj$time[begin_index:end_index] # training_time <- cbind(x_j_cl_org$time[1:2874], x_j_cl_LSTM$time[2875:3403])[1:3403] 
  training_time <- sort(training_time)
  temp_ptproc <- ptproc(pts = training_time, cond.int = gradient.hawkes.cond.int, params = pstart) # pts는 관측이터
  condition(temp_ptproc) <- penalty(code = NULL, condition = quote(any(params < 0)))
  temp_ptproc <- ptproc.fit(temp_ptproc, optim.control = list(trace = 2), method = method_name, alpha = 1e+5, hessian = hessian_bool)
  return(temp_ptproc)
}


cif <- function(ptproc, event_time_set, begin_index, end_index) {
  temp_ptproc <- ptproc(pts = event_time_set, cond.int = gradient.hawkes.cond.int, params = ptproc$params) # pts는 관측이터
  condition(temp_ptproc) <- penalty(code = NULL, condition = quote(any(params < 0)))
  temp_domain <- seq(event_time_set[begin_index], event_time_set[end_index], len=(end_index-begin_index+1))
  temp_evalCIF <- evalCIF(temp_ptproc, xpts = temp_domain)  
  return(list(temp_domain, temp_evalCIF))
}


# 시간 Format 변형
timeTransform <- function(dataFrame) {
  dataFrame$time <- as.POSIXct(dataFrame$time, origin="1970-01-01")
  dataFrame
}


# aggregation: breakUnit별 합산
aggregation <- function(dataFrame, breakUnit) {
  dataFrameRe <- aggregate(dataFrame$magnitude ~ cut( dataFrame$time, breaks = breakUnit), dataFrame, sum)
  dataFrameRe
}


# 기울기 (gradient descent)
gradient_gamma <- function(dataframe001, dataframe002) {
  
  mse <- function(x, df01, df02){
    return(sum((df01$magnitude[1:90] - x*df02$magnitude[1:90])^2)/90)}  
  
  minn <- optimize(mse, interval = c(-100,100), tol = 0.01, dataframe001, dataframe002) # interval은 수렴범위
  return(minn$minimum)
}


# Evaluations
forecast_MSE <- function(df01, df02) {
  mean((df01 - df02)^2)
}  


forecast_MAE <- function(df01, df02) {
  mean(abs(df01 - df02))
}


forecast_accuracy <- function(df01, df02) {
  
  error_rate <- abs(df01 - df02)/(df01)
  temp_accuracy <- rep(1, times = 30) - error_rate
  temp_list <- which(0>temp_accuracy)
  n <- length(temp_list)
  
  for(i in 1:n){
    temp_index <- temp_list[i]
    temp_accuracy[temp_index] <- 0
  }
  
  temp_accuracy
}


forecast_score <- function(df01, df02, a1, a2) {
  distances <- df02 - df01 # estimated values - true values
  
  a1 <- a1
  a2 <- a2
  
  a1_list <- which(0>distances)
  a2_list <- which(0<=distances)
  
  a1_n <- length(a1_list)
  a2_n <- length(a2_list)
  
  exp_list_a1 <- c(rep(0,30))
  exp_list_a2 <- c(rep(0,30))
  
  if (a1_n != 0){
    for(i in 1:a1_n){
      temp_index <- a1_list[i]
      temp_distance <- distances[temp_index]
      exp_list_a1[i] <- exp(-temp_distance/a1) - 1
    } }
  
  if (a2_n != 0){
    for(i in 1:a2_n){
      temp_index <- a2_list[i]
      temp_distance <- distances[temp_index]
      exp_list_a2[i] <- exp(temp_distance/a2) - 1
    } }
  
  return( list(sum(exp_list_a1), sum(exp_list_a2)))
  
}

getDistance <- function(beg, end, timeVector) {
  tempBeg <- head(which(beg <= timeVector), n=1)   # 1
  tempEnd <- tail(which(end > timeVector), n=1) # 2873  
  return(list(tempBeg, tempEnd))
}


getConcatenated <- function(list1, list2, df1, df2) {
  
  tempBeg1 <- toString(list1[1])
  tempEnd1 <- toString(list1[2])
  
  tempBeg2 <- toString(list2[1])
  tempEnd2 <- toString(list2[2])
  
  c(df1[tempBeg1:tempEnd1], df2[tempBeg2:tempEnd2])
}


add_up <- function(dt_frame) { # by min
  dt_frame$time <- as.POSIXct(dt_frame$time,  origin="1970-01-01")
  
  dt_frame <- aggregate(
    x = dt_frame$magnitude, 
    by = list(dt_frame$time), 
    FUN = sum
  )
  
  dt_frame <- dt_frame[,-3] # Group.1 magnitude time, 마지막   time drop
  names(dt_frame) <- c("time", "magnitude")
  dt_frame
}


timeTransform <- function(dataFrame) {
  dataFrame$time <- as.POSIXct(dataFrame$time,  origin="1970-01-01")
  dataFrame
}


aggregation <- function(dataFrame, breakUnit) {
  dataFrameRe <- aggregate(dataFrame$magnitude ~ cut( dataFrame$time, breaks = breakUnit), dataFrame, sum)
  dataFrameRe
}

