iskin <- function(data,kinmat,thresh=0.24) {
  inds <- data[,cbind(match(FocalID,rownames(A)),
                match(PartnerID,colnames(A)))]
  return(A[inds]>thresh)
}

samesex <- function(data,demo) {
  return(demo[match(data$FocalID,Focal_ID),SEX] == demo[match(data$PartnerID,Focal_ID),SEX])
}

addmodifier <- function(data,func,modname=as.character(substitute(func)),...) {
  modcontent <- func(data,...)
  data[!is.na(modcontent),BehaviorModifier:=paste0(BehaviorModifier,";",
                                 modname,"(",modcontent[!is.na(modcontent)],")")]
}

countkin <- function(ID,kinmat,thresh=0.25) {
  kinnames <- colnames(kinmat)
  tmp <- (kinmat>=thresh)[match(ID,kinnames),match(ID,kinnames)]
  return(data.table(ID=colnames(tmp),kincount=colSums(tmp)-1))
}

partnerrank <- function(data,rank) {
  focrank <- rank[data,ORD_RANK,on=c(ID="FocalID",GROUP="Group",YEAR="Year"),mult="first"]
  partrank <- rank[data,ORD_RANK,on=c(ID="PartnerID",GROUP="Group",YEAR="Year"),mult="first"]
  
  comp <- rep(NA,length(focrank))
  comp[focrank>partrank] <- "lower"
  comp[focrank<partrank] <- "higher"
  comp[focrank==partrank] <- "similar"
  
  return(comp)
}




censifiles <- paste0("~/Dropbox/Cayo Census/",c("2013/2013.June_animalels in cs-orig matriline June 2013.xls",
                                                "2014/2014.AUG_animalels in cs-orig matriline August 2014.xls",
                                                "2015/2015.June.animalels in cs-orig matriline June 2015.xls",
                                                "2016/2016 April.animalels in cs-orig matriline abril 2016.xls"))
censiyear <- c("2013","2014","2015","2016")

censi <- lapply(censifiles,read_excel)
for (i in 1:length(censi)) censi[[i]]$YEAR <- censiyear[i]
censi <- do.call(rbind,censi) %>% as.data.table()
censi <- censi[paste0(GROUP,YEAR) %in% bdat[,paste0(Group,Year)]]

kincounts <- censi[str_length(animal_id2)==3,countkin(animal_id2,A,0.25),by=.(YEAR,GROUP)]