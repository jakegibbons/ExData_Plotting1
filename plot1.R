 
# The assumption is that if a zip file exists with the correct name locally
# then the data in the file is correct.

getDataFileName <- function() {
    # variables
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    localZipFile <- "household_power_consumption.zip"
    localFile <- "household_power_consumption.txt"
    
    # Check for file. If it is not present then download and unzip data file.
    if (!file.exists(localFile)) {
        # Linux
        #download.file(fileURL
        #              , destfile=localZipFile
        #              , method="curl")
        # Windows
        download.file(fileURL
                      , destfile=localZipFile)
        # If there was a clean download then unzip specifying the data file.
        if (file.exists(localZipFile)){
          unzip(localZipFile
                , files = localFile)
        }
    }
#    else {
#      # Data Exists
#      ## testing only
#      # writeLines("File exists.\n")
#    }
#    # return the local file name
    localFile
}

getDataSet <- function() {
    
    tempDataFile <- "tempHousePowerCons.csv"
    
    # Using data from the dates 2007-02-01 and 2007-02-02
    
    # If we did not previously create a temporary data file then 
    if(!file.exists(tempDataFile)) {
        # Call getDataFileName
        message("getting new data.")
        hPowCons <- read.table(getDataFileName()
                               , sep = ";"
                               , header=T
                               , na.strings = "?"
                               , as.is = T)
        hPowCons$Date = as.Date(hPowCons$Date
                                , format = "%d/%m/%Y")
        hPowConsSub <- subset(hPowCons
                              , subset=(Date >= "2007-02-01" & Date <= "2007-02-02"))
    
        # Clean up a little
        rm(hPowCons)
        
        # Write the data to file for use later.
        write.csv(hPowConsSub
                  , tempDataFile)
    }
    else
    {
        # Later. Use data previously written (assume no updates).
        hPowConsSub <- read.csv(tempDataFile)
    }
    
    return(hPowConsSub)
    
}

runPlot <- function() {
    
    # Get the data
    powerData <- getDataSet()
    #print(summary(powerData))

    # plot 1: Histogram

    # Open png device
    png(filename = "plot1.png", width = 480, height = 480, units = "px")
    
    # Run plot

    hist(powerData$Global_active_power
         , main="Global Active Power"
         , xlab="Global Active Power (kilowatts)"
         , ylab="Frequency", col="Red")

    # Close device
    dev.off()
}

runPlot()