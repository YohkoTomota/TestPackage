#This is test for building package.
#'Maccor Status was check
#'@importFrom  stringr str_detect
#'@importFrom  dplyr %>%
#'@importFrom  dplyr filter
#'@importFrom  dplyr mutate
#'@importFrom  dplyr select
#'@import data.table
#'@param FP Folder path of maccor ascii files
#'@export
#'
#'
MaccorStatus <- function(FP){
  #Check maccor ASCII files which modified in last 2 hours
  #Arg:  FP  : folder path which has Maccor ASCII rawdata

  MaccorStatus <- data.frame()

  for (i  in c("BASF","BASF2","BASF3")){
    print(i)
    setwd(paste0(FP,i))
    filestatus<- data.frame(file.info(dir(pattern = "\\.[0-9][0-9][0-9]$")))

    filestatus <- filestatus %>%  mutate(filename = rownames(filestatus)) %>%
      select(filename,mtime,size)%>%
      filter(mtime > Sys.time() - 3600*3)

    MaccorStatus <-rbind(MaccorStatus,filestatus)
  }
  ch <- function(x){
    substr(x, nchar(x)-2, nchar(x))
  }
  blk <- function(x){
    sub("_.*","",x)
  }
  cell <- function(x){
    substr(x,nchar(blk(x))+2,nchar(x)-4)
  }

  MaccorStatus$mtime <- format(MaccorStatus$mtime, "%H:%M")
  MaccorStatus$size <- round(MaccorStatus$size/1000000,2)
  MaccorStatus <- MaccorStatus %>%
    mutate(blk =blk(filename), channel = as.character(ch(filename)), CellNo =cell(filename)) %>%
    select(blk,channel,CellNo, mtime, size)
  colnames(MaccorStatus)<-c("blk","Channel","CellNo","Modified time","Size[Mb]")

  return(MaccorStatus)
}

