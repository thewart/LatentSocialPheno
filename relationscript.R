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

countkin <- function(ID,kinmat,thresh) {
  kinnames <- colnames(kinmat)
  tmp <- (kinmat>thresh)[match(ID,kinnames),match(ID,kinnames)]
  return(colSums(tmp)-1)
}
