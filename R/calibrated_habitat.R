calibrated_habitat <- function(year, month){
  habitat <- get_habitat(year, month)
#Calibration Changes 
  vect2<-c(1.4416309,1.9379344,1.3706987,1.6449355,1.4556516,0.5481166,0.7098337,0.7279391,0.8681320,
           1.3761102,1.0039699,1.9759963,1.7591008,1.4374917,0.8327904,0.6907140,1.0503587,1.3019928,
           1.1983915)

  habitat$inchannel[6] <- habitat$inchannel[6] * vect2[6]
  habitat$inchannel[10] <- habitat$inchannel[10] * vect2[7]
  habitat$inchannel[12] <- habitat$inchannel[12] * vect2[8]
  habitat$inchannel[16] <- habitat$inchannel[16] * vect2[9]
  habitat$inchannel[19] <- habitat$inchannel[19] * vect2[10]
  habitat$inchannel[20] <- habitat$inchannel[20] * vect2[11]
  habitat$inchannel[21] <- habitat$inchannel[21] * vect2[12]
  habitat$inchannel[24] <- habitat$inchannel[24] * vect2[13]
  habitat$inchannel[2] <- habitat$inchannel[2] * vect2[14]
  habitat$inchannel[7] <- habitat$inchannel[7] * vect2[15]
  

  habitat$sutter <- habitat$sutter * vect2[16]
  habitat$yolo <- habitat$yolo * vect2[17]
  

  habitat$north_delta <- habitat$north_delta * vect2[18]
  habitat$south_delta <- habitat$south_delta * vect2[19]
  
  return(habitat)
}



