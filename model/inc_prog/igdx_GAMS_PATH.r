library(gdata)
library(gdxrrw)
MacOS <- gdata::startsWith(Sys.getenv("R_HOME"), "/")
if(MacOS){
  igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")
  print("MacOS or linux")
}else{
  print("not MacOS or linux")
}