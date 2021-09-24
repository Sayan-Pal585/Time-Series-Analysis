############################################################################
############################################################################
# CODE: BIVARIATES
# AUTHOR: DEEPESH SINGH - GAC BANGALORE
# DATE: 18 APR 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
#Set working directory

setwd("O:\\DATA\\Argentina\\Data")

############################################################################
#Read input file

in_frame <-read.csv(file.choose(),sep = ",", header = T, stringsAsFactors = F)

############################################################################
######################      CODE STARTS HERE           #####################
############################################################################

in_frame$TimeFrame <- as.Date(in_frame$Month, "%m-%d-%Y")
in_frame$Month <- in_frame$TimeFrame
in_frame$TimeFrame <- NULL
no_col <- ncol(in_frame) - 2
no_rows <- ceiling(no_col/2)

############################################################################
#Plotting Bivariates and writing to PDF

pdf("EDA_Plots.pdf", width = 16 , height = 10, title = "EDA Plots for data")
par(mfrow=c(2,2))
for(i in 3:(ncol(in_frame))){
  
  par(mar = c(5,4,4,5)+.1)
  plot(in_frame[,1], in_frame[,2], col = "black", type = "l", ylab = names(in_frame[2]), xlab = "Year")
  par(new = T)
  plot(in_frame[,1], in_frame[,i], col = "red", type = "l", xaxt = "n", yaxt = "n",
       xlab = "", ylab = "")
  axis(4)
  mtext(names(in_frame[i]), side = 4, line = 3)
  corr <- round(cor(in_frame[,2], in_frame[,i], method = "pearson", use = "pairwise.complete.obs"), digits = 2)
  color <- ifelse(abs(corr) > 0.3, "green", "gray")
  lngd <- paste0("Corr: ",corr)
  legend("topleft", legend = c(lngd), bty = "0", text.col = color)
  
}

dev.off()

############################################################################
######################      CODE ENDS HERE             #####################
############################################################################
