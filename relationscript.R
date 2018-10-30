iskin <- function(data,kinmat,thresh=0.5) {
  inds <- data[,cbind(match(FocalID,rownames(A)),
                match(PartnerID,colnames(A)))]
  return(A[inds]>=thresh)
}

samesex <- function(data,demo) {
  return(demo[match(data$FocalID,Focal_ID),SEX] == demo[match(data$PartnerID,Focal_ID),SEX])
}

addmodifier <- function(data,func,modname=as.character(substitute(func)),...) {
  modcontent <- func(data,...)
  data[!is.na(modcontent),BehaviorModifier:=paste0(BehaviorModifier,";",
                                 modname,"(",modcontent[!is.na(modcontent)],")")]
}

countkin <- function(ID,kinmat,thresh=0.5) {
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





