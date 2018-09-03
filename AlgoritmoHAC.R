
# Implementación de HAC (High Adjective Count) creado por Eirinaki y su equipo:

# Eirinaki, M., Pisal, S., & Singh, J. (2012). Feature-based opinion mining and
# ranking. Journal of Computer and System Sciences, 78(4), 1175-1184.

# Para usarlo hay que ejecutar el script entero y llamar a la función: algorithmHAC(opiniones, umbral)
# opiniones: aquí se introduce la salida de NLP
# umbral: ocurrencias junto a opinion word a partir de la cual se tiene en cuenta como aspecto

library(tm)

EsNombre= function(palabra){(palabra=="NN" | palabra=="NNP" | palabra=="NNS" | palabra=="NNPS")}
EsAdjetivo= function(palabra){(palabra=="JJ" | palabra=="JJR" | palabra=="JJS")}

#Esta función filtra y se queda con los nombres y los adjetivos
opinionesSNA=function(opiniones){
  
  numeroopiniones=opiniones$sentence[nrow(opiniones)]
  opiniones[EsNombre(opiniones$POS)|EsAdjetivo(opiniones$POS),]
  
}

PurgaListaAspectos=function(Lista){
  
  #Quita las urls
  Lista=Lista[!sapply(Lista, function(x) grepl('http[:]',x))]
  Lista=Lista[!sapply(Lista, function(x) grepl('[.]com',x))]
  Lista=Lista[!sapply(Lista, function(x) grepl('www[.]',x))]
  #Quita los que tengan dos asteriscos o más seguidos en el aspecto
  Lista=Lista[!grepl('[*]+[*]',Lista)]
  #Quita los que tengan dos puntos o más seguidos en el aspecto
  Lista=Lista[!grepl('[.]+[.]',Lista)]
  #Quita los que tengan menos de 2 caracteres
  Lista=Lista[!sapply(Lista,nchar)<2]
  #Quita los NA
  Lista=Lista[!is.na(Lista)]
  
  Lista
}

#Algoritmo
algorithmHAC=function(opiniones,umbral){
  
  opiniones2=opinionesSNA(opiniones)
  
  candidatos=levels(as.factor(opiniones2$lemma[which(EsNombre(opiniones2$POS))]))
  contador=rep(0,length(candidatos))
  
  adjetivos=opiniones2[which(EsAdjetivo(opiniones2$POS)),]
  
  for(i in 1:nrow(adjetivos)){
    numerofrase=adjetivos$sentence[i]
    idadjetivo=as.numeric(adjetivos$id[i])
    candidatosfrase=opiniones2[(opiniones2$sentence==numerofrase&EsNombre(opiniones2$POS)),]
    if(nrow(candidatosfrase)>0){
      diferencias=abs(as.numeric(candidatosfrase$id)-idadjetivo)
      
      nombre=candidatosfrase$lemma[order(diferencias)[1]]
      
      contador[which(nombre==candidatos)]=contador[which(nombre==candidatos)]+1
    }
    
  }
  
  #El umbral indica a partir de cuántos se considera aspecto
  aspectos=candidatos[which(contador>umbral)]
  
  Aspectos=aspectos[!aspectos %in% stopwords()]
  PurgaListaAspectos(Aspectos)
}