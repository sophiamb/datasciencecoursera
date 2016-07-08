completecasesbyfileid <- function (id)
{
  # file name, assume csv file with number padded to 3 (eg:001.csv)
  filename = str_c(str_pad(id,3,"left","0"),".csv")
  
  #read the table
  filecontents = read.table(str_c(directory,"/",filename),header=TRUE,sep=",")
  
  #return  count of complete cases
  return (sum(complete.cases(filecontents),na.rm=TRUE))
}


complete <- function(directory,id=1:332)
{
  # create data frame, with ids and an empty column for results
  df = data.frame(id=id,nobs=NA)
  
  # populate results 
  df$nobs = lapply(df$id, completecasesbyfileid)
  
  # return completed dataframe
  return(df)
}