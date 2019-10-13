#!/usr/bin/env Rscript


##################################################################-
##                                                              ##
##--------------------------------------------------------------##
########----| CALCULATE ENERGY STORED IN OAT STEMS |----##########-
##--------------------------------------------------------------##
##                                                              ##
##################################################################-




##################################################################-
##                                                              ##
##--------------------------------------------------------------##
###################----|   REQUIREMENTS  |----####################-
##--------------------------------------------------------------##
##################################################################-
##
##  1. Script (.R)
##
##  2. Input directory named "INdir" (case sentitive), containing 
##     heights and data from the stalk meter.
##    2.1. Heights in centimeters (.csv) with two columns named 
##         "PLOT" and "HEIGHT" (upper case), respectively.
##    2.2. Files (.TXT) from stalk meter. They are expected to 
##         have 4 columns named "PLOT", "TIME", "LOAD", and "ANGLE".
##
##  3. Output directory named "OUTdir" (case sentitive), where 
##     the script will write the results.
##
##--------------------------------------------------------------##
##                                                              ##
##################################################################-







##--------------------------------------------------------------##
#################------|   CODE BEGINS  |------##################-
##--------------------------------------------------------------##





# Define files' folders
mydir <- dirname(rstudioapi::getSourceEditorContext()$path)
OUTdir <- paste0(mydir, "/OUTdir/")
INdir <- paste0(mydir, "/INdir/")

# Make a folder for graphs
dir.create(paste0(OUTdir, "GRAPHS"))

# List all the files (usually "LOG***.TXT")
FilesList <- list.files(path = INdir, pattern = glob2rx("LOG*.TXT"))

# Read the listed files (change it if files are .csv)
myListedFiles <- lapply(paste0(INdir, FilesList), read.table, header = TRUE)
# myfiles <- Reduce(rbind, myListedFiles)

# Read HEIGHTS and define column names
heights <- read.csv(file = paste0(INdir, "HEIGHTS.csv"), header = TRUE)
colnames(heights) <- c("PLOT", "HEIGHT")




# Define calibration parameters
# To convert arbitrary LOAD values to Newtons (N) using regression line for 2019
m <- 1.475      # SLOPE of Calibration Curve
b <- 0.1404     # INTERCEPT of Calibration Curve





# Create empty file to store results
mysummary <- NULL

# Iteration
for (i in 1:length(myListedFiles)) {
  
  # Current plot
  myplot <- myListedFiles[[i]]
  
  # Combine plot with heights
  myplot <- merge(myplot, heights)
  
  # Convert arbitrary LOAD units to Newtons
  myplot$LOAD_N <- m * myplot$LOAD + b
  
  # Calculate DISPLACEMENT (l)
  myplot$DISPLACEMENT <- myplot$HEIGHT * cos(myplot$ANGLE * pi / 180)
  
  # Calculate ENERGY (l * F)
  myplot$ENERGY <- myplot$DISPLACEMENT * myplot$LOAD_N
  
  # Remove negative LOAD_N values
  myplot <- myplot[which(myplot$LOAD_N > 0), ]
  
  # Define Bend and UnBend
  UpTo45 <- max( which(myplot$ANGLE < 46 ) )  
  myBend <- myplot[1:UpTo45, ]
  myUnBend <- myplot[-which(myBend$TIME %in% myplot$TIME), ]
  
  # Estimate ENERGY stored in stems (E)
  E_Bend <- sum(myBend$E)
  E_UnBend <- sum(myUnBend$E)
  
  # Estimate POWER (E / time)
  POWER_Bend <- E_Bend / (max(myBend$TIME) - min(myBend$TIME))
  POWER_UnBend <- E_UnBend / (max(myUnBend$TIME) - max(myBend$TIME))
  
  # Calculate Deltas
  DELTA_ENERGY <- (E_Bend - E_UnBend) / 100   # To convert to ...
  DELTA_POWER <- POWER_Bend - POWER_UnBend
  
  
  ##--------------------------------------------------------------##
  ##---------------|  Plot Displacement Vs Load  |----------------##
  
  # Open a pdf file
  pdf(file = paste0(OUTdir, "GRAPHS/", myplot$PLOT[1], ".pdf"), 
      width = 2, height = 5, pointsize = 2)
  
  # Create three plots in one column
  par(mfrow=c(3,1))
  
  plot(myplot$DISPLACEMENT, myplot$LOAD_N, 
       main = paste0(myplot$PLOT[1]), 
       xlab="Displacement (cm)",
       ylab="Load (N)", type = "l")
  
  plot(myBend$DISPLACEMENT, myBend$LOAD_N, 
       main = paste0(myplot$PLOT[1], " Bending"), 
       xlab="Displacement (cm)",
       ylab="Load (N)", type = "l")
  
  plot(myUnBend$DISPLACEMENT, myUnBend$LOAD_N, 
       main = paste0(myplot$PLOT[1], " Unbending"), 
       xlab="Displacement (cm)",
       ylab="Load (N)", type = "l")
  
  # Close the pdf file
  dev.off()
  
  ##--------------------------------------------------------------##
  
  
  ##--------------------------------------------------------------##
  ##---------------------|  Write results  |----------------------##
  
  # Create output file  
  myfile <- NULL
  myfile$plot <- as.data.frame(myplot$PLOT[1])
  myfile$height <- as.data.frame(mean(myplot$HEIGHT))
  myfile$load_45 <- as.data.frame(myplot[UpTo45, c("LOAD_N")])
  myfile$displacement_45 <- as.data.frame(myplot[UpTo45, c("DISPLACEMENT")])
  myfile$delta_energy <- as.data.frame(DELTA_ENERGY)
  myfile$delta_power <- data.frame(DELTA_POWER)
  
  # Combine variables
  myfile <- Reduce(cbind, myfile)
  colnames(myfile) <- c("PLOT", "HEIGHT", "FORCE_45", 
                        "DISPLACEMENT_45", "DELTA_ENERGY", "DELTA_POWER")
  
  ##--------------------------------------------------------------##
  
  
  # Add current file to summary
  mysummary[[i]] <- myfile
  
}




# Define FINAL_file
FINAL_file <- Reduce(rbind, mysummary)

# Write file in output directory
write.table( FINAL_file, file = paste0(OUTdir, "FINAL_FILE.txt"), row.names = FALSE )





##--------------------------------------------------------------##
###################----|   CODE ENDS   |----########@############-
##--------------------------------------------------------------##






##################################################################-
##                                                              ##
## ----------------------  Written by ------------------------- ##
############ ----|  Joan Manuel Barreto Ortiz |---- ##############-
##--------------------------------------------------------------##
#######----| jbarreto@umn.edu | jmbarretoo@gmail.com |----########-
##--------------------------------------------------------------##
##                                                              ##
##################################################################-


