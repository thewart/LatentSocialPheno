getage <- function(ID,year)
{
  require(xlsx)
  census <- read.xlsx2("~/Dropbox/Cayo Census/2011.11.Nov_Animales in CS ALL.xls",1,colIndex = c(1,3))
  YOB <- as.numeric(sapply(census$DOB[match(ID,census$animal_id2)],format,"%Y"))
  age <- as.integer(year) - YOB
  
  return(age)
}

getsex <- function(ID)
{
  require(xlsx)
  census <- read.xlsx2("~/Dropbox/Cayo Census/2011.11.Nov_Animales in CS ALL.xls",1,colIndex = c(1,6))
  sex <- census$SEX[match(ID,census$animal_id2)]
  return(sex)
}

getgroup <- function(ID)
{
  require(xlsx)
  census <- read.xlsx2("~/Dropbox/Cayo Census/2011.11.Nov_Animales in CS ALL.xls",
                       1,colIndex = c(1,7),colClasses = rep("character",2))
  sex <- census$GROUP[match(ID,census$animal_id2)]
  return(sex)
}