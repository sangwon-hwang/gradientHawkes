  
  # install.packages('ggplot2')
  library(ggplot2)
  library(dplyr)
  
  # source
  ls <- list.files('./R')
  mainIndex <- which(ls == "main.R")
  evaluationIndex <- which(ls == "evaluation.R")
  scoreIndex <- which(ls == "scoreGraph.R")
  MAEIndex <- which(ls == "MAEGraph.R")
  
  ls <- ls[-c(mainIndex, evaluationIndex, scoreIndex, MAEIndex)]
  
  for(i in 1:length(ls)){
    sourcePath <- paste0("./R/", ls[i])
    print(sourcePath)
    source(sourcePath)
  }

	# data load
	# decrease
	options(digits=12)
	tempDir = './data/criteo_cl_times_dec.csv'
	observed_dec <- read.csv(file=tempDir) # click 
	# tempDir = './data/cv_times_1029.csv'
	# observed_dec <- read.csv(file=tempDir) # conversion
	tempDir = './data/0817LSTM_CL_Pred_LB02_epoch30_RMSE7.25.csv' 
	LSTM_dec <- read.csv(file=tempDir); 	names(LSTM_dec) <- c("index","time") # prediction
	
	# increase
	tempDir = './data//criteo_cl_times_inc.csv'
	observed_inc <- read.csv(file=tempDir) # click
	# tempDir = './data//cv_times_2nd_1029.csv'
	# observed_inc <- read.csv(file=tempDir) # conversion
	tempDir = './data//0815LSTM_CL_Pred_LB01_epoch20_RMSE3.47.csv' 
	LSTM_inc <- read.csv(file=tempDir); 	names(LSTM_inc) <- c("index","time") # prediction
	
	# decrease/learning: [260820, 260820+5400) = [260820, 266220)
	# decrease/prediction: [266220, 266220+1800) = [266220, 268020) 
	decLearn <- getDistance(260820, 266220, observed_dec$time)
	decPredict <- getDistance(266220, 268020, LSTM_dec$time)
	
	# increase/learning:  [355800, 355800+5400) = [355800, 361200)
	# decrease/prediction:  [361200, 361200+1800) = [361200, 363000)
	incLearn <- getDistance(355800, 361200, observed_inc$time)
	incPredict <- getDistance(361200, 363000, LSTM_inc$time)

	# combined data
	decPTS <- getConcatenated(decLearn, decPredict, observed_dec$time, LSTM_dec$time)
	incPTS <- getConcatenated(incLearn, incPredict, observed_inc$time, LSTM_inc$time)
	
	# main
	# hyper parameter setup
	pstart <- c(mu = 0.062850512533, C = 0.013627776356, a = 0.011434188950, c=1.1)
	
	# optimization 
	# 'arg' should be one of “Nelder-Mead”, “BFGS”, “CG”, “L-BFGS-B”, “SANN”, “Brent”
	NM_dec <- learning(observed_dec, as.numeric(unlist(decLearn[1])), as.numeric(unlist(decLearn[2])), pstart, "Nelder-Mead", TRUE)
	CG_dec <- learning(observed_dec, as.numeric(unlist(decLearn[1])), as.numeric(unlist(decLearn[2])), pstart, "CG", FALSE)
	BFGS_dec <- learning(observed_dec, as.numeric(unlist(decLearn[1])), as.numeric(unlist(decLearn[2])), pstart, "BFGS", FALSE)
	SANN_dec <- learning(observed_dec, as.numeric(unlist(decLearn[1])), as.numeric(unlist(decLearn[2])), pstart, "SANN", TRUE)
	
	NM_inc <- learning(observed_inc, as.numeric(unlist(incLearn[1])), as.numeric(unlist(incLearn[2])), pstart, "Nelder-Mead", FALSE)
	CG_inc <- learning(observed_inc, as.numeric(unlist(incLearn[1])), as.numeric(unlist(incLearn[2])), pstart, "CG", FALSE)
	BFGS_inc <- learning(observed_inc, as.numeric(unlist(incLearn[1])), as.numeric(unlist(incLearn[2])), pstart, "BFGS", FALSE)
	SANN_inc <- learning(observed_inc, as.numeric(unlist(incLearn[1])), as.numeric(unlist(incLearn[2])), pstart, "SANN", TRUE)
	
	# Evaluation Residuals of Gradient Hawkes Model
	NM_dec_res <- residuals(NM_dec, type = "approx", K = 100)
	CG_dec_res <- residuals(CG_dec, type = "approx", K = 100)
	BFGS_dec_res <- residuals(BFGS_dec, type = "approx", K = 100)
	SANN_dec_res <- residuals(SANN_dec, type = "approx", K = 100)
	
	NM_inc_res <- residuals(NM_inc, type = "approx", K = 100)
	CG_inc_res <- residuals(CG_inc, type = "approx", K = 100)
	BFGS_inc_res <- residuals(BFGS_inc, type = "approx", K = 100)
	SANN_inc_res <- residuals(SANN_inc, type = "approx", K = 1200)
	
	# logSurv
	logSurv(NM_dec_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	logSurv(CG_dec_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	logSurv(BFGS_dec_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	logSurv(SANN_dec_res, col = rgb(0.192, 0.192, 0.192, 0.13))

	logSurv(NM_inc_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	logSurv(CG_inc_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	logSurv(BFGS_inc_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	logSurv(SANN_inc_res, col = rgb(0.192, 0.192, 0.192, 0.13))
	
	# sampling
	sim <- ptproc.sim(SANN_inc, M = 5) # 학습이 가장 잘된 증가구간 SANN으로 t_i 샘플링 (Thinning 방식) / ptproc.sim(NM_dec, M = 5)
	simEnd <- tail(which(362996.47>=sim), n = 1) # 관측 데이터에서 예측 구간의 마지막 t_n의 값 362996.47 # 2085
	sim[1:simEnd] # 샘플링 된 이벤트 타임중 비교가능한 t_i의 개수 2085개
	sim[1:100] # 228.997709678
	thinning_compare <- (observed_dec$time[2874:2973] - observed_dec$time[2873])
	qqx_thinning <- (sim[1:100] - thinning_compare) # 해당 구간 [91:120]의 이벤트 시간 오차의 분산
	qqx_lstm <- (LSTM_dec$time[2876:2975] - observed_dec$time[2874:2973])
	
	thinning = data.frame(x = 1:40, y = qqx_thinning[1:40]); colnames(thinning) <- c('time','residual')
	lstm = data.frame(x = 1:40, y = qqx_lstm[1:40]); colnames(lstm) <- c('time','residual')

	# sampling graph
	temp_ggplot <- ggplot() + 
	  geom_point(data = thinning, aes(x = time, y = residual, color = "c1", shape = "s1"), size=2)  +
	  geom_point(data = lstm, aes(x = time, y = residual, color = "c2", shape = "s2"), size=1.5)  +
	  scale_colour_manual(name = "sampling method",
	                      label= c("Thinning sampling","LSTM sampling"),
	                      values = c(c1 = "#F8966D", c2 = "#00BFC4")) +   
	  scale_shape_manual(name = "sampling method", 
	                     label= c("Thinning sampling","LSTM sampling"),
	                     values = c(s1 = 4, s2 = 1))
  	xlab('Time') +
	  ylab('Residual') +
	  theme_light()
	
	  temp_ggplot + 
	  theme_bw() +
	  xlab('Time') +
	  ylab('Residual') +
	  theme(legend.title = element_text(face="bold", color="black", size=10),
	        legend.text = element_text(face="bold", color="black", size=10)
	  ) 
	
	# CIF 그리기
	NM_dec_cif <- cif(NM_dec, decPTS, 1, length(decPTS))
	CG_dec_cif <- cif(CG_dec, decPTS, 1, length(decPTS))
	BFGS_dec_cif <- cif(BFGS_dec, decPTS, 1, length(decPTS))
	SANN_dec_cif <- cif(SANN_dec, decPTS, 1, length(decPTS))
	
	NM_inc_cif <- cif(NM_inc, incPTS, 1, length(incPTS)) 
	CG_inc_cif <- cif(CG_inc, incPTS, 1, length(incPTS))
	BFGS_inc_cif <- cif(BFGS_inc, incPTS, 1, length(incPTS))
	SANN_inc_cif <- cif(SANN_inc, incPTS, 1, length(incPTS))

	# 데이터프레임설정
	originDec <- getDistance(260820, 268020, observed_dec$time)
	decBeg <- as.numeric(unlist(originDec[1]))
	decEnd <- as.numeric(unlist(originDec[2]))
	observed_dec_df <- data.frame(time=observed_dec$time[decBeg:decEnd], magnitude=observed_dec$magnitude[decBeg:decEnd])
	
	originInc <- getDistance(355800, 363000, observed_inc$time)
	incBeg <- as.numeric(unlist(originInc[1]))
	incEnd <- as.numeric(unlist(originInc[2]))
	observed_inc_df <- data.frame(time=observed_inc$time[incBeg:incEnd], magnitude=observed_inc$magnitude[incBeg:incEnd])

	NM_dec_df <- data.frame(time=NM_dec_cif[1], magnitude=NM_dec_cif[2]); 	colnames(NM_dec_df) <- c("time","magnitude")
	CG_dec_df <- data.frame(time=CG_dec_cif[1], magnitude=CG_dec_cif[2]); 	colnames(CG_dec_df) <- c("time","magnitude")
	BFGS_dec_df <- data.frame(time=BFGS_dec_cif[1], magnitude=BFGS_dec_cif[2]); 	colnames(BFGS_dec_df) <- c("time","magnitude")
	SANN_dec_df <- data.frame(time=SANN_dec_cif[1], magnitude=SANN_dec_cif[2]); 	colnames(SANN_dec_df) <- c("time","magnitude")
  
	NM_inc_df <- data.frame(time=NM_inc_cif[1], magnitude=NM_inc_cif[2]); 	colnames(NM_inc_df) <- c("time","magnitude")
	CG_inc_df <- data.frame(time=CG_inc_cif[1], magnitude=CG_inc_cif[2]); 	colnames(CG_inc_df) <- c("time","magnitude")
	BFGS_inc_df <- data.frame(time=BFGS_inc_cif[1], magnitude=BFGS_inc_cif[2]); 	colnames(BFGS_inc_df) <- c("time","magnitude")
	SANN_inc_df <- data.frame(time=SANN_inc_cif[1], magnitude=SANN_inc_cif[2]); 	colnames(SANN_inc_df) <- c("time","magnitude")

	# Time transform  # 예외처리추가필요
	observed_dec_df <- timeTransform(observed_dec_df)
	observed_inc_df <- timeTransform(observed_inc_df)
	
	NM_dec_df <- timeTransform(NM_dec_df)
	CG_dec_df <- timeTransform(CG_dec_df)
	BFGS_dec_df <- timeTransform(BFGS_dec_df)
	SANN_dec_df <- timeTransform(SANN_dec_df)
	
	NM_inc_df <- timeTransform(NM_inc_df)         	
	CG_inc_df <- timeTransform(CG_inc_df)         
	BFGS_inc_df <- timeTransform(BFGS_inc_df)         
	SANN_inc_df <- timeTransform(SANN_inc_df)         
	
	# aggregation by time-unit, by min  # 예외처리추가필요
	observed_dec_df <- aggregation(observed_dec_df,"1 min"); 	colnames(observed_dec_df) <- c("time","magnitude")	
	observed_inc_df <- aggregation(observed_inc_df,"1 min"); 	colnames(observed_inc_df) <- c("time","magnitude")	

	NM_dec_df <- aggregation(NM_dec_df,"1 min"); 	colnames(NM_dec_df) <- c("time","magnitude")	
	CG_dec_df <- aggregation(CG_dec_df,"1 min"); 	colnames(CG_dec_df) <- c("time","magnitude")
	BFGS_dec_df <- aggregation(BFGS_dec_df,"1 min"); 	colnames(BFGS_dec_df) <- c("time","magnitude")
	SANN_dec_df <- aggregation(SANN_dec_df,"1 min"); 	colnames(SANN_dec_df) <- c("time","magnitude")

	NM_inc_df <- aggregation(NM_inc_df,"1 min"); 	colnames(NM_inc_df) <- c("time","magnitude")
	CG_inc_df <- aggregation(CG_inc_df,"1 min"); 	colnames(CG_inc_df) <- c("time","magnitude")
	BFGS_inc_df <- aggregation(BFGS_inc_df,"1 min"); 	colnames(BFGS_inc_df) <- c("time","magnitude")
	SANN_inc_df <- aggregation(SANN_inc_df,"1 min"); 	colnames(SANN_inc_df) <- c("time","magnitude")
	
	# 기울기 최적화
	NM_dec_df$magnitude <- NM_dec_df$magnitude * gradient_gamma(observed_dec_df, NM_dec_df)  
	CG_dec_df$magnitude <- CG_dec_df$magnitude * gradient_gamma(observed_dec_df, CG_dec_df)  
	BFGS_dec_df$magnitude <- BFGS_dec_df$magnitude * gradient_gamma(observed_dec_df, BFGS_dec_df)  
	SANN_dec_df$magnitude <- SANN_dec_df$magnitude * gradient_gamma(observed_dec_df, SANN_dec_df)  
	
	NM_inc_df$magnitude <- NM_inc_df$magnitude * gradient_gamma(observed_inc_df, NM_inc_df)  
	CG_inc_df$magnitude <- CG_inc_df$magnitude * gradient_gamma(observed_inc_df, CG_inc_df)  
	BFGS_inc_df$magnitude <- BFGS_inc_df$magnitude * gradient_gamma(observed_inc_df, BFGS_inc_df)  
	SANN_inc_df$magnitude <- SANN_inc_df$magnitude * gradient_gamma(observed_inc_df, SANN_inc_df)  
  
	
	NM_dec_df_origin <- read.csv(file='./data/NM_dec_df.csv')

	tmep_ggplot <- ggplot() + 
	  geom_line(aes(x = 1:120, y = NM_dec_df_origin$magnitude, color = "c1", linetype="lt1"), size = 0.6)  +
	  geom_line(aes(x = 1:120, y = NM_dec_df$magnitude, color = "c2", linetype="lt1"), size = 0.3) +
	  scale_colour_manual(name = "section",
	                      label = c("observed", "fitted", "forecast"),
	                      values = c(c1 = "#F8966D", c2 = "#00BA38", c3="#619CFF")) +   
	  scale_linetype_manual(name = "section", 
	                        label = c("observed", "fitted", "forecast"),
	                        values = c(lt1 = "twodash", lt2 = "solid", lt3 = "solid")) +
	  xlab('Time') +
	  ylab('') +
	  theme_light() +
	  theme(legend.title = element_blank(),
	        legend.text = element_text(face="bold", color="black", size=10),
	        legend.position = c(0.12, 0.88)) 
	
	tmep_ggplot
	
	
	NM_dec_df_origin$magnitude - NM_dec_df$magnitude
	
	# To write a file 
	# write.csv(observed_dec_df, './data/observed_dec_df.csv', row.names=TRUE)
	# write.csv(observed_inc_df, './data/observed_inc_df.csv', row.names=TRUE)
	# 	
	# write.csv(NM_dec_df, './data/NM_dec_df.csv', row.names=TRUE)
	# write.csv(CG_dec_df, './data/CG_dec_df.csv', row.names=TRUE)
	# write.csv(BFGS_dec_df, './data/BFGS_dec_df.csv', row.names=TRUE)
	# write.csv(SANN_dec_df, './data/SANN_dec_df.csv', row.names=TRUE)
	# 
	# write.csv(NM_inc_df, './data/NM_inc_df.csv', row.names=TRUE)
	# write.csv(CG_inc_df, './data/CG_inc_df.csv', row.names=TRUE)
	# write.csv(BFGS_inc_df, './data/BFGS_inc_df.csv', row.names=TRUE)
	# write.csv(SANN_inc_df, './data/SANN_inc_df.csv', row.names=TRUE)
	
