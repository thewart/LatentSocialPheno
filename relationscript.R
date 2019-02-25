iskin <- function(data,kinmat,thresh=0.24) {
  inds <- data[,cbind(match(FocalID,rownames(A)),
                match(PartnerID,colnames(A)))]
  return(A[inds]>=thresh)
}

samesex <- function(data,demo) {
  return(demo[match(data$FocalID,Focal_ID),SEX] == demo[match(data$PartnerID,Focal_ID),SEX])
}

addmodifier <- function(data,func,modname=as.character(substitute(func)),...) {
  modcontent <- func(data,...)
  modcontent[bdat[,PartnerID!=""] & is.na(modcontent)] <- "NA"
  data[!is.na(modcontent),BehaviorModifier:=paste0(BehaviorModifier,";",
                                 modname,"(",modcontent[!is.na(modcontent)],")")]
}

countkin <- function(ID,kinmat,sex,thresh=0.24) {
  sexmatch <- (((sex=="f")-0.5)*2) %>% tcrossprod()
  kinnames <- colnames(kinmat)
  tmp <- (kinmat>=thresh)[match(ID,kinnames),match(ID,kinnames)]
  return(data.table(ID=colnames(tmp),
                    kin_samesex=colSums(tmp*(sexmatch==1))-1,
                    kin_diffsex=colSums(tmp*(sexmatch==-1))))
}

partnerrank <- function(data,rank) {
  focrank <- rank[data,ORD_RANK,on=c(ID="FocalID",GROUP="Group",YEAR="Year"),mult="first"]
  partrank <- rank[data,ORD_RANK,on=c(ID="PartnerID",GROUP="Group",YEAR="Year"),mult="first"]
  
  comp <- rep(NA,length(focrank))
  comp[focrank>=partrank] <- "lower"
  comp[focrank<partrank] <- "higher"
  comp[focrank=="H" & partrank=="H"] <- "higher"
  # comp[focrank>partrank] <- "lower"
  # comp[focrank==partrank] <- "similar"
  
  return(comp)
}