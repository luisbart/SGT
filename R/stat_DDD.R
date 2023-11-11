#' @title Compute snow gamma parameters per elevation zone
#' @author Luis Barreiro
#' @description Clip raster to polygon. It uses a shapefile (or sf) as input, locate the extent on the raster catalog, crops the raster, then clip
#' @param station station number, finishin with ".0". F eg "7.23.0"
#' @param dtm_res dtm resolution in meter for accessing the right folder of results (1/10)
#' @param path_DDD_R path to DDD output using SG_R
#' @param path_DDD_T path to DDD output using SG_T
#' @param path_MODIS path to Modis files with SCA observations
#' @param cloud maximum cloud cover alq loud for using Modis SCA (default=30)
#' @returns A table with all results, efficiency creiteria, biases and whether they are significant or not. Q_KGE_cal and Q_KGE_val are the efficiency criteria Kling-Gupta Efficiency (KGE) (Gupta et al. (2009) and Kling et al. (2012)) for calibration and validation dataset respectively, and for SG_R (_R) and SG_T (_T). Q_NSE_cal and Q_NSE_val are the Nash-Sutcliffe efficiency (NSE) criterion (Nash & Sutcliffe, 1970) for calibration and validation. Q_bias_cal and Q_bias_val are the bias between observed and estimated streamflow. Qp_bias_R and Qp_bias_T are the (maximum annual peak flow for spring season) bias obs/estimated for SG_R and SG_T. SWE_bias_cal and SWE_bias_val are the biases between SG_R and SG_T. SCA_avg is the average SCA for the melting season for mMODIS (_modis), SG_R (_R) and SG_T (_T). SCA_bias_R and SCA_bias_T are the SCA bias obs/estimated for SG_R and SC_T. For all bias calculations, the symbol (*) marks when it is significant (sig. level 5%)

stat_DDD <- function(station, dtm_res, path_DDD_R, path_DDD_T, path_MODIS, cloud = 30){
  colnames1 <- c("yr","mnt","day","hr","meanprecip","meantemp","q","QRD","middelsca","snomag","M-totdef","totdef",
                 "sm","ea","outxoutbog","outglac","m_r_onglac+outglac","lyrs","Qmm","smbog","eabog","boglyrs","qmm_state",
                 "wcs","PotEvap","MSW","rad*fac","MLT*fac","MLA*fac","MSH*fac","MLE*fac","MALB","midGLAC","midGLAC1","mCl")


  colnames2 <- c("yr","mnt","day","hr","meanprecip","meantemp","q","QRD","middelsca","snomag","M-totdef","totdef",
                 "sm","ea","outxoutbog","outglac","m_r_onglac+outglac","lyrs","Qmm","smbog","eabog","boglyrs","qmm_state",
                 "wcs","PotEvap","MSW","rad*fac","MLT*fac","MLA*fac","MSH*fac","MLE*fac","MALB","midGLAC","midGLAC1","mCl",
                 "sca[1]","sca[2]","sca[3]","sca[4]","sca[5]","sca[6]","sca[7]","sca[8]","sca[9]","sca[10]","swe_h[1]",
                 "swe_h[2]","swe_h[3]","swe_h[4]","swe_h[5]","swe_h[6]","swe_h[7]","swe_h[8]","swe_h[9]","swe_h[10]","alfa[1]",
                 "alfa[2]","alfa[3]","alfa[4]","alfa[5]","alfa[6]","alfa[7]","alfa[8]","alfa[9]","alfa[10]","ny[1]","ny[2]",
                 "ny[3]","ny[4]","ny[5]","ny[6]","ny[7]","ny[8]","ny[9]","ny[10]")



  #### Assess streamflow ####
  dataOld1 = data.frame(as.matrix(read.csv(file.path(path_DDD_R, paste("simres_kal_EB_", station, ".csv", sep="")), header=FALSE, stringsAsFactors=FALSE, sep=";")))
  dataOld1 <- dataOld1[-c(1:5),]
  colnames(dataOld1) <- colnames1

  dataCal1 = data.frame(as.matrix(read.csv(file.path(path_DDD_T, paste(dtm_res,"m/kal/simres_kal_EB_", station, ".csv", sep="")), header=FALSE, stringsAsFactors=FALSE, sep=";")))
  dataCal1 <- dataCal1[-c(1:5),]
  colnames(dataCal1) <- colnames2

  dataOld2 = data.frame(as.matrix(read.csv(file.path(path_DDD_R, paste("simres_val_EB_", station, ".csv", sep="")), header=FALSE, stringsAsFactors=FALSE, sep=";")))
  dataOld2 <- dataOld2[-c(1:5),]
  colnames(dataOld2) <- colnames1

  dataCal2 = data.frame(as.matrix(read.csv(file.path(path_DDD_T, paste(dtm_res,"m/kal/simres_val_EB_", station, ".csv", sep="")), header=FALSE, stringsAsFactors=FALSE, sep=";")))
  dataCal2 <- dataCal2[-c(1:5),]
  colnames(dataCal2) <- colnames2

  dataOld1 <- dataOld1[dataOld1$yr != 1999,]
  dataOld2 <- dataOld2[dataOld2$yr != 1989,]
  dataCal1 <- dataCal1[dataCal1$yr != 1999,]
  dataCal2 <- dataCal2[dataCal2$yr != 1989,]

  dataCal1 <- within(dataCal1, {
    Date <- as.Date(paste(yr, mnt, day, sep = "-"), format = "%Y-%m-%d")
  })

  dataOld1 <- within(dataOld1, {
    Date <- as.Date(paste(yr, mnt, day, sep = "-"), format = "%Y-%m-%d")
  })

  dataCal2 <- within(dataCal2, {
    Date <- as.Date(paste(yr, mnt, day, sep = "-"), format = "%Y-%m-%d")
  })

  dataOld2 <- within(dataOld2, {
    Date <- as.Date(paste(yr, mnt, day, sep = "-"), format = "%Y-%m-%d")
  })

  ###########################
  #Check p-value

  # SG_T
  # Perform a t-test to compare the two data sets
  t_test_result <- t.test(dataCal1$q, dataCal1$QRD)
  t_test_result2 <- t.test(dataCal2$q, dataCal2$QRD)

  # Extract the p-value from the test result
  p_value <- t_test_result$p.value
  p_value2 <- t_test_result2$p.value

  # Set the significance level (alpha)
  alpha <- 0.05

  # Compare the p-value to the significance level
  if (p_value <= alpha) {
    Q_sig_cal_T <- "*"
  } else {
    Q_sig_cal_T <- ""
  }

  if (p_value2 <= alpha) {
    Q_sig_val_T <- "*"
  } else {
    Q_sig_val_T <- ""
  }

  Q_KGE_cal_T <- cbind(KGE(dataCal1$QRD, dataCal1$q), "")
  Q_KGE_val_T <- cbind( KGE(dataCal2$QRD, dataCal2$q), "")
  Q_NSE_cal_T <- cbind(NSE(dataCal1$QRD, dataCal1$q), "")
  Q_NSE_val_T <- cbind(NSE(dataCal2$QRD, dataCal2$q), "")
  Q_bias_cal_T <- cbind(mean(dataCal1$QRD) / mean(dataCal1$q), Q_sig_cal_T)
  Q_bias_val_T <- cbind(mean(dataCal2$QRD) / mean(dataCal2$q), Q_sig_val_T)

  #SG_R
  # Perform a t-test to compare the two data sets
  t_test_result <- t.test(dataOld1$q, dataOld1$QRD)
  t_test_result2 <- t.test(dataOld2$q, dataOld2$QRD)

  # Extract the p-value from the test result
  p_value <- t_test_result$p.value
  p_value2 <- t_test_result2$p.value

  # Set the significance level (alpha)
  alpha <- 0.05

  # Compare the p-value to the significance level
  if (p_value <= alpha) {
    Q_sig_cal_R <- "*"
  } else {
    Q_sig_cal_R <- ""
  }

  if (p_value2 <= alpha) {
    Q_sig_val_R <- "*"
  } else {
    Q_sig_val_R <- ""
  }

  Q_KGE_cal_R <- cbind(KGE(dataCal1$QRD, dataCal1$q), "")
  Q_KGE_val_R <- cbind( KGE(dataCal2$QRD, dataCal2$q), "")
  Q_NSE_cal_R <- cbind(NSE(dataCal1$QRD, dataCal1$q), "")
  Q_NSE_val_R <- cbind(NSE(dataCal2$QRD, dataCal2$q), "")
  Q_bias_cal_R <- cbind(mean(dataCal1$QRD) / mean(dataCal1$q), Q_sig_cal_R)
  Q_bias_val_R <- cbind(mean(dataCal2$QRD) / mean(dataCal2$q), Q_sig_val_R)



  #### Assess yearly spring peak flow ####
  #Merge kal and val
  dataCal <- rbind(dataCal1, dataCal2)
  dataOld <- rbind(dataOld1, dataOld2)

  #select dates with MODIS data
  dataCal <- dataCal %>%
    filter(!yr %in% c(1989, 1999))

  dataOld <- dataOld %>%
    filter(!yr %in% c(1989, 1999))

  #Checking Peaks per Year
  observed_data <- dataCal[, c("yr", "q", "Date")]
  sim_Old <- dataOld[, c("yr", "QRD", "Date")]
  sim_New <- dataCal[, c("yr", "QRD", "Date")]

  # Extracting annual maximum series (peak flow per spring season)
  extract_ams <- function(time_series, field) {
    time_series %>%
      mutate(Year = format(Date, "%Y")) %>%
      group_by(Year) %>%
      filter({{field}} == max({{field}})) %>%
      select(Year, max_q = {{field}}, Date)
  }


  ams_obs <- extract_ams(observed_data,q)
  ams_old <- extract_ams(sim_Old,QRD)
  ams_new <- extract_ams(sim_New,QRD)

  # Perform a t-test to compare the two data sets
  t_test_resultOld <- t.test(ams_obs$max_q, ams_old$max_q)
  t_test_resultNew <- t.test(ams_obs$max_q, ams_new$max_q)

  # Extract the p-value from the test result
  p_valueOld <- t_test_resultOld$p.value
  p_valueNew <- t_test_resultNew$p.value

  # Compare the p-value to the significance level
  if (p_valueOld <= alpha) {
    Qp_sig_T <- "*"
  } else {
    Qp_sig_T <- ""
  }

  if (p_valueNew <= alpha) {
    Qp_sig_R <- "*"
  } else {
    Qp_sig_R <- ""
  }

  Qp_bias_T <- cbind(mean(ams_obs$max_q) / mean(ams_old$max_q), Qp_sig_R)
  Qp_bias_R <- cbind(mean(ams_obs$max_q) / mean(ams_new$max_q), Qp_sig_T)

  #### Assess SWE: bias between SG_R and SG_T (no observation values) ####

  # Perform a t-test to compare the two data sets
  t_test_result <- t.test(dataOld1$snomag, dataCal1$snomag)
  t_test_result2 <- t.test(dataOld2$snomag, dataCal2$snomag)
  # Extract the p-value from the test result
  p_value <- t_test_result$p.value
  p_value2 <- t_test_result2$p.value

  # Set the significance level (alpha)
  alpha <- 0.05

  # Compare the p-value to the significance level
  if (p_value <= alpha) {
    SWE_sig_cal <- "*"
  } else {
    SWE_sig_cal <- ""
  }

  if (p_value2 <= alpha) {
    SWE_sig_val <- "*"
  } else {
    SWE_sig_val <- ""
  }


  SWE_bias_cal <- cbind(mean(dataCal1$snomag) / mean(dataOld1$snomag), SWE_sig_cal)
  SWE_bias_val <- cbind(mean(dataCal2$snomag) / mean(dataOld2$snomag), SWE_sig_val)



  #### Assess SCA ####

  #Import Modis SCA coverage
  Modis = data.frame(as.matrix(read.table(file.path(paste(path_MODIS, "/", station, "_SCA_2001_2015.txt", sep="")), header=FALSE, stringsAsFactors=FALSE, sep="")))
  #Replace -9999 for NA
  Modis[, 2:ncol(Modis)] <- lapply(Modis[, 2:ncol(Modis)], as.double)
  Modis[Modis == -9999] <- NA
  colnames(Modis) <- c("Date","avgSCA","skyer","scah1","scah2","scah3","scah4","scah5","scah6","scah7","scah8","scah9","scah10")
  Modis$Date <- as.Date(Modis$Date, format = "%Y.%m.%d")
  Modis <- Modis[,c(1,2,3)]

  #Import SCA results from DDD
  dataCal <- dataCal[,c(76, 9)]
  dataCal[2:2] <- dataCal[2:2] *100
  colnames(dataCal) <- c("Date", "midSCA_new")

  dataOld <- dataOld[,c(36, 9)]
  dataOld[2:2] <- dataOld[2:2] *100
  colnames(dataOld) <- c("Date", "midSCA_old")

  #Merge
  DDD <- merge(dataCal, dataOld, by="Date", all=F)
  SCA <- Modis[complete.cases(Modis[,2]), ] #remove rows with SCA from MODIS missing
  SCA <- merge(SCA,DDD, by="Date", all=F)
  SCA <- SCA[SCA$skyer<cloud | is.na(SCA$skyer),]


  # Perform a t-test to compare the two data sets
  t_test_resultOld <- t.test(SCA$avgSCA, SCA$midSCA_old)
  t_test_resultNew <- t.test(SCA$avgSCA, SCA$midSCA_new)

  # Extract the p-value from the test result
  p_valueOld <- t_test_resultOld$p.value
  p_valueNew <- t_test_resultNew$p.value

  # Set the significance level (alpha)
  alpha <- 0.05

  # Compare the p-value to the significance level
  if (p_valueOld <= alpha) {
    SCA_sig_R <- "*"
  } else {
    SCA_sig_R <- ""
  }

  if (p_valueNew <= alpha) {
    SCA_sig_T <- "*"
  } else {
    SCA_sig_T <- ""
  }

  SCA_avg_Modis <- cbind(mean(SCA$avgSCA),"")
  SCA_avg_R <- cbind(mean(DDD$midSCA_old),"")
  SCA_avg_T <- cbind(mean(DDD$midSCA_new),"")

  SCA_bias_T <- cbind(mean(SCA$avgSCA) / mean(SCA$midSCA_new), SCA_sig_R)
  SCA_bias_R <- cbind(mean(SCA$avgSCA) / mean(SCA$midSCA_old), SCA_sig_T)

  #### export results in a table####
  results <- as.data.frame(rbind(Q_KGE_cal_R, Q_KGE_val_R, Q_NSE_cal_R, Q_NSE_val_R, Q_bias_cal_R, Q_bias_val_R, Q_KGE_cal_T,
                                 Q_KGE_val_T, Q_NSE_cal_T, Q_NSE_val_T, Q_bias_cal_T, Q_bias_val_T, Qp_bias_R, Qp_bias_T,
                                 SWE_bias_cal, SWE_bias_val, SCA_avg_Modis, SCA_avg_R, SCA_bias_R, SCA_avg_T, SCA_bias_T))
  rownames(results) <- c("Q_KGE_cal_R", "Q_KGE_val_R", "Q_NSE_cal_R", "Q_NSE_val_R", "Q_bias_cal_R", "Q_bias_val_R", "Q_KGE_cal_T",
                         "Q_KGE_val_T", "Q_NSE_cal_T", "Q_NSE_val_T", "Q_bias_cal_T", "Q_bias_val_T", "Qp_bias_R", "Qp_bias_T",
                         "SWE_bias_cal", "SWE_bias_val", "SCA_avg_Modis", "SCA_avg_R", "SCA_bias_R", "SCA_avg_T", "SCA_bias_T")
  colnames(results) <- c("par", "signif")

  return(results)

}



