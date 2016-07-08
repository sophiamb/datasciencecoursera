## directory: location of the csv files
## pollutant: pollutant to summarise
## id: monitor ID numbers to include
pollutantmean <- function(directory,pollutant,id=1:332){
  #Get files from directory
  files<-dir(directory)[id]
  
  #Get results from files
  results=c()
  for(filename in files)
  {
    filecontents = read.table(paste(directory,"/",filename,sep=""),header=TRUE,sep=",")
    results=c(results,filecontents[pollutant][!is.na(filecontents[pollutant])])
  }
  
  #Return the mean
  return(mean(results))
}