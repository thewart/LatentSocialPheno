derepeat <- function(behav) {
  n <- length(behav)
  x <- !logical(n)
  if (n>1) {
    for (i in 2:n) 
      if (behav[i-1]==behav[i]) x[i] <- F
  }
  return(x)
}

eventslices <- function(behav,basebehav) 
  behav[sapply(behav,function(x) str_detect(x,pattern = paste0("^",basebehav,"(:|$)")) %>% any)]

addupbehaviors <- function(behav,dat,adultsonly=T) {
  underage <- c("HUMAN","INFANT","JUVENILE","NO ANIMAL","UNKNOWN","UNKOWN","JUVENIL")
  dat[,Observation:=factor(Observation)]
  
  pt <- dat[(Behavior %in% behav) & !(PartnerID %in% underage),
            if (Duration[1]>0) as.integer(sum(Duration))
            else .N,
            by=.(Observation,Behavior)]
  
  pt <- dcast(pt,Observation ~ Behavior,fill=0,drop=F)
  return(pt)
}

scanprep <- function(dat) {
  scandat <- dat[Behavior=="scan",.(In2mCode,
                          N2m=sapply(RelativeEventTime,function(x) abs(x-c(0,300,600)) %>% which.min)),
       by=.(FocalID,Observation)]
  scandat[,In2mPart:=!(In2mCode %in% c("In 2m? (8)","In 2m? (0)"))]
  out <- scandat[,.(ScanProx=length(unique(N2m[In2mPart])),ScanProxInd=length(unique(In2mCode[In2mPart]))),by=Observation]
  
  #scannless observations
  noscan <- dat[,("scan" %in% Behavior),by=.(Observation)][V1==F,Observation]
  if (length(noscan)>0) out <- rbind(out,data.table(Observation=noscan,ScanProx=NA,ScanProxInd=NA))
  return(out)
}

#divide behaviors in ptetho based on matching to modifiers
eventsplit <- function(behav,targmod,ptetho) {
  targmod <- str_replace_all(targmod," ","")
  nb <- nrow(ptetho)
  npte <- copy(ptetho)
  for (i in 1:nb) {
    ibeh <- ptetho$behavior[i]
    behmatch <- str_detect(behav,paste0("^",ibeh,"(:|$)"))
    if (is.na(ptetho$modifier[i]) | all(!behmatch)) {
      next
    } else {
      mod <- str_split(ptetho$modifier[i],";",2)[[1]]
      imod <- mod[1]
      npte$modifier[i] <- mod[2]

      newb <- string_attach(behav[behmatch],targmod[behmatch],imod)
      behav[behmatch] <- newb
    }
  }
  
  if ( is.na(npte$modifier) %>% all() ) {
    return(behav)
  } else {
    return(eventsplit(behav,targmod,npte))
  }
}

string_attach <- function(base,string,pattern) {
  guh <- str_extract(string,pattern)
  notna <- !is.na(guh)
  out <- base
  out[notna] <- paste0(out[notna],":",guh[notna])
  return(out)
}
                             
collectfocal <- function(bdat,ptetho=NULL,stetho=NULL,nsec=NA,group,fixyear=T,state.maxsec=630)
{
  if (is.null(stetho))
    stetho <- defaultstate()
  
  if (is.null(ptetho))   
    ptetho <- defaultpoint2()
  
  if (is.character(bdat)) bdat <- readfocfiles(bdat,group)
  
  #construct state data
  ss <- sapply(stetho,length)
  Xs <- stetho[,statprep(behavior,dat = bdat,nsec = nsec, maxsec = state.maxsec),by=state] %>% 
    dcast(Observation ~ behavior,value.var="value") %>% setcolorder(c("Observation",stetho$behavior))

  #construct count data
  if (is.list(ptetho)) {
    bdat[,Behavior:=eventsplit(.BY[[1]],ptetho,BehaviorModifier,.N),by=Behavior]
    ptetho <- ptetho$behavior
  }
  Xc <- countprep(ptetho,bdat)
  
  #construct proximity data
  Xp <- scanprep(bdat)
  
  X <- merge(Xc,Xs) %>% merge(Xp)
  l <- length(X)
  X <- merge(X,unique(bdat[,cbind(Observation,FocalID,Observer,Year,Group)]),by="Observation")
  setcolorder(X,c(1,(l+1):(l+4),2:l))
  setkey(X,"FocalID")
  return(X)
}

readfocfiles <- function(files,group,minobs=11,maxsec=630,fixyear=T) {
  bdat <- list()
  for (i in 1:length(files)) {
    bdat[[i]] <- fread(files[i])
    bdat[[i]] <- copy(bdat[[i]][!overtime & !BadObs])
    bdat[[i]] <- bdat[[i]][FocalID %in% bdat[[i]][,length(unique(Observation)),by=FocalID][V1>=minobs,FocalID]]
    if (fixyear) bdat[[i]]$Year <- bdat[[i]][,table(Year)] %>% which.max() %>% names()
    bdat[[i]]$Group <- group[i]
    bdat[[i]] <- bdat[[i]][RelativeEventTime<maxsec]
    bdat[[i]][,Duration:=pmin(Duration,630-RelativeEventTime)]
    
  }
  return(do.call(rbind,bdat))
}
