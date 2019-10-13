#!/usr/bin/env Rscript


##############################################################-
############---- SIMULATE STALK METER DATA ----##############-
##############################################################-


########---------------------------------------#########-
########---| By: Joan Manuel Barreto Ortiz |----########-
######| jbarreto@umn.edu | jmbarretoo@gmail.com |#######-
########---------------------------------------#########-




########## INFORMATION ############-
##
## Simulated data for the new Stalk Meter
## Two files. Heights file and AngLoad file.
## FILE ONE --> [ <<One obs per plot>> , c(PLOT, HEIGHT(cm)) ]
## FILE TWO --> [ <<Multiple obss per plot>> , c(PLOT, TIME(sec), LOAD_raw, ANGLE(degree)) ]
##
##
## LOAD_raw is in arbitrary units. Convert to Newtons
## Calculate DISPLACEMENT (l) = HEIGHT * cos(ANGLE)







######################################-
#######----***** BEGIN *****----#######-
######################################-


# Command to locate file's path
mydir <- dirname(rstudioapi::getSourceEditorContext()$path)
OUTdir <- paste0(mydir, "/OUTdir/")
INdir <- paste0(mydir, "/INdir/")


# To get reproducible numbers
set.seed(022490)


# Simulate HEIGHTS file in centimeters for 250 plots, where HEIGHT is the point where the the plants were pushed (midpoint)
# Let's assume there are short and long plants in general, and I'll have two different normal distributions centered at 100 and 70. It might not be necessary.



#######################################################-
############***| FILE ONE ---> HEIGHTS |***############-
#######################################################-


# How many plots?
nplots <- 250

# Generate fake plot names
PLOTNAMES <- as.factor(paste0(rep("LOG00", length.out = nplots), 
                   sprintf('%0.3d', 1:nplots), ".TXT"))

# Generate simulated heights
HEIGHTS <- as.data.frame( round( rnorm(n = length(PLOTNAMES), 
                               mean = c(100, 70), sd = 12), 0 ) )
# Create first file --> HEIGHTS
HEIGHTS <- cbind(PLOTNAMES, HEIGHTS)
colnames(HEIGHTS) <- c("PLOT", "HEIGHT")

# Write HEIGHTS file
write.csv( HEIGHTS, file = paste0(INdir, "HEIGHTS.csv"), row.names = FALSE )


#######################################################-
############***| FILE TWO ----> ANGLES |***############-
#######################################################-

# Variables in files
myvariables <- c("PLOT", "TIME", "LOAD", "ANGLE")


# Generate multiple files
for (i in 1:length(PLOTNAMES)) {
  
  # Reps per plot (on average, how many data points per plot?)
  myreps <- round( runif(n = 1, min = 120, max = 150) , 0)
  
  # Arbitrary Load (This might have a logarithmic/exponential distribution...?)
  ### First term: variable with log values from 1 to 55 percentile rep;
  ### Second term: adds variation across plots;
  ### Third term: includes some negative values
  myload1 <- round( sort( log(1:c(myreps*0.55)) * rnorm(1,20,5) + rnorm(1,-5,2) ), 2 )
  myload2 <- round( sort( runif(n = myreps*0.45, min = min(myload1), max = max(myload1)), 
                   decreasing = TRUE), 2 )

  # Angle from 95 to 45 (with outliers)
  reps90_45 <- length(myload1) - length(myload1[which(myload1 < 0)])
  myangleOut1 <- round( sort( runif(n = length(myload1) - reps90_45, min = 90, max = 95),
                             decreasing = TRUE ), 1 )
  myangle1 <- round( sort( runif(n = reps90_45, min = 44, max = 90),
                           decreasing = TRUE ), 1 )
  # Angle from <45 to 100
  
  reps45_90 <- length(myload2) - length(myload2[which(myload2 < 0)])
  myangleOut2 <- round( sort( runif(n = length(myload2) - reps45_90, min = 90, max = 95),
                              decreasing = TRUE ), 1 )
  myangle2 <- round( sort( runif(n = reps45_90, min = 44, max = 90)), 1 )
  
  # Redefine number of reps
  myreps <- length(myangleOut1) + length(myangle1) + length(myangle2) + length(myangleOut2)
  
  # Time in seconds increasing by 1
  mytime <- seq(from = 1, to = myreps, by = 1)
  
  
  # Create data with variables 
  myfile <- NULL
  myfile$plot <- as.data.frame(rep(PLOTNAMES[i], times = myreps))
  myfile$mytime <- as.data.frame(mytime)
  myfile$load <- as.data.frame(c(myload1, myload2))
  myfile$angle <- data.frame(c(myangleOut1,myangle1, myangle2,myangleOut2))
  
  # Combine variables
  myfile <- Reduce(cbind, myfile)
  colnames(myfile) <- myvariables
  
  # Write file in output directory
  write.table( myfile, file = paste0(INdir, PLOTNAMES[i]), row.names = FALSE )
  
  
}







######################################-
#####----***** THE END *****----#####-
######################################-
