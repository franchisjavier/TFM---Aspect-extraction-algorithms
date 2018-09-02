

LeerIndividual =function(directorio,texto){
  scan(paste(directorio,"/",texto,sep=""),what="character",sep="\n")
}


Review=LeerIndividual("datasets","vader_amazon.txt")

library(coreNLP)
initCoreNLP()


#Este script divide el archivo en varias partes y luego las junta para evitar
#grandes cargas de memoria en el ordenador

opinionest = annotateString(Review[1:100])
opiniones = opinionest$token
depend = opinionest$basicDep



floor(length(Review)/100)

for(i in 1:(floor(length(Review)/100)-1)){
  
  cienopinionest = annotateString(Review[(100*i+1):(100*i+100)])
  cienopiniones = cienopinionest$token
  ciendepend = cienopinionest$basicDep
  
  cienopiniones$sentence=cienopiniones$sentence+opiniones$sentence[length(opiniones$sentence)]
  ciendepend$sentence=ciendepend$sentence+opiniones$sentence[length(opiniones$sentence)]
  
  opiniones=rbind(opiniones,cienopiniones)
  depend=rbind(depend,ciendepend)
  
  print(i)
}


if(length(Review)%%100>0){
  
  cienopinionest = annotateString(Review[(100*floor(length(Review))+1):(100*floor(length(Review))+length(Review)%%100)])
  cienopiniones = cienopinionest$token
  ciendepend = cienopinionest$basicDep
  
  cienopiniones$sentence=cienopiniones$sentence+opiniones$sentence[length(opiniones$sentence)]
  ciendepend$sentence=ciendepend$sentence+opiniones$sentence[length(opiniones$sentence)]
  
  opiniones=rbind(opiniones,cienopiniones)
  depend=rbind(depend,ciendepend)
}


summary(opiniones)
str(opiniones)

summary(depend)
str(depend)


###############################################

