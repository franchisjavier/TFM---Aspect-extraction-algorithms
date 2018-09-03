
# Implementación del algoritmo de Hu & Liu que aparece en:

# Hu, M., & Liu, B. (2004, August). Mining and summarizing customer reviews.
# In Proceedings of the tenth ACM SIGKDD international conference on Knowledge
# discovery and data mining (pp. 168-177). ACM.

# Para usarlo hay que ejecutar el script entero y llamar a la función: algorithmHL(opiniones, soporte)
# opiniones: aquí se introduce la salida de NLP
# soporte: frecuencia a partir de la cual se tiene en cuenta como aspecto, 
#   en Hu & Liu se estableció 0.01

library(tm)
library(arules)

# Funciones para usar con el POS del coreNLP
EsNombre= function(palabra){(palabra=="NN" | palabra=="NNP" | palabra=="NNS" | palabra=="NNPS")}
EsAdjetivo= function(palabra){(palabra=="JJ" | palabra=="JJR" | palabra=="JJS")}

# Esta función saca los bigrams de una frase
BigramTokenizer <- function(x) 
{unlist(lapply(ngrams(strsplit(x," ")[[1]], 2), paste, collapse = " "), 
        use.names = FALSE)}
# Esta función saca los trigrams de una frase
TrigramTokenizer <- function(x) 
{unlist(lapply(ngrams(strsplit(x," ")[[1]], 3), paste, collapse = " "), 
        use.names = FALSE)}

#Función que saca candidatos de bi y tri

BITRI= function(Rev,candidatosUNI){
  
  #Fase de quitar las stopwords
  Prueba=sapply(Rev,function (x) strsplit(x," "))
  for(j in 1:length(Prueba)){
    palabrasparada=which(is.na(match(Prueba[[j]],stopwords("SMART")))==FALSE)
    if(length(palabrasparada)>1)
      Prueba[[j]]=Prueba[[j]][-palabrasparada]
  }
  
  #Fase de crear las reglas de tamaño 2 y 3
  reglas=apriori(Prueba,
                 parameter = list(supp = 0.1, minlen=2 ,maxlen=3,target = "frequent itemsets"))
  apri=labels(reglas)
  apri=gsub('[{]','', apri)
  apri=gsub('[}]','', apri)
  apri=gsub('[,]',' ', apri)
  apri=gsub('[.]','', apri)
  apri=gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", apri, perl=TRUE)
  
  candidatosBITRI=apri[sort(unlist(sapply(candidatosUNI,function (x) grep(x,apri)),use.names = FALSE))]
  candidatosBITRI=unique(candidatosBITRI)
  
}

inside= function(palabras,frase){
  
  tam=length(palabras)
  h=FALSE
  
  if(tam==1){
    #Si solo es una palabra se comprueba fácilmente si está en la frase
    h=any(palabras==frase)
  }
  
  if(tam==2){
    #Comprueba si algún índice de la primera palabra está cerca de algún índice de la segunda
    ids1=which(palabras[1]==frase)
    ids2=which(palabras[2]==frase)
    if(length(ids1)>0 & length(ids2)>0) 
      h=any(abs(sapply(ids1,function (x) x-ids2))<=2)
  }
  
  if(tam==3){
    #Comprueba si algún índice de la primera palabra está cerca de algún índice de la segunda
    ids1=which(palabras[1]==frase)
    ids2=which(palabras[2]==frase)
    ids3=which(palabras[3]==frase)
    if(length(ids1)>0 & length(ids2)>0 & length(ids3)>0) {
      a=any(abs(sapply(ids1,function (x) x-ids2))<=2)
      b=any(abs(sapply(ids2,function (x) x-ids3))<=2)
      c=any(abs(sapply(ids1,function (x) x-ids3))<=4)
      h=all(a,b,c)
    }
  }
  
  if(tam==4){
    #Comprueba si algún índice de la primera palabra está cerca de algún índice de la segunda
    ids1=which(palabras[1]==frase)
    ids2=which(palabras[2]==frase)
    ids3=which(palabras[3]==frase)
    ids4=which(palabras[4]==frase)
    if(length(ids1)>0 & length(ids2)>0 & length(ids3)>0 & length(ids4)>0) {
      a=any(abs(sapply(ids1,function (x) x-ids2))<=2)
      b=any(abs(sapply(ids2,function (x) x-ids3))<=2)
      c=any(abs(sapply(ids1,function (x) x-ids3))<=4)
      d=any(abs(sapply(ids3,function (x) x-ids4))<=2)
      e=any(abs(sapply(ids2,function (x) x-ids4))<=4)
      f=any(abs(sapply(ids1,function (x) x-ids4))<=6)
      h=all(a,b,c,d,e,f)
    }
  }
  
  h
}

#Poda
Poda= function(candidatosUNI,candidatosBITRI,opiniones){
  
  #Se establece el parametro por el cual se pasa la poda de compacidad
  poda1=0.1
  #Parametro poda de redundancia
  poda2=0.2
  
  filas=nrow(opiniones)
  num=opiniones[filas,]$sentence
  
  candidatosBITRI=strsplit(candidatosBITRI," ")
  longitudes=sapply(1:length(candidatosBITRI),function(x)length(candidatosBITRI[[x]]))
  candidatosBI=candidatosBITRI[which(longitudes==2)]
  candidatosTRI=candidatosBITRI[which(longitudes==3)]
  
  #Poda compacidad
  killbi=NULL
  for(i in 1:length(candidatosBI)){
    lista1=unique(opiniones$sentence[which(opiniones$token==candidatosBI[[i]][1])])
    lista2=unique(opiniones$sentence[which(opiniones$token==candidatosBI[[i]][2])])
    listatotal=lista1[which(match(lista1,lista2,nomatch = 0)>0)]
    
    #Cuenta las apariciones del candidato
    contador=length(which(sapply(listatotal,function(x) 
    {inside(candidatosBI[[i]],opiniones[opiniones$sentence==x,]$token)}
    )))
    
    if(contador<poda1*length(listatotal)){
      killbi=c(killbi,i)
    }
    else{
      #Poda de redundancia
      if((length(lista1)-contador)/length(lista1)<poda2){
        redundant=which(candidatosUNI==candidatosBI[[i]][1])
        if(length(redundant)==1) candidatosUNI=candidatosUNI[-redundant]
      }
      if((length(lista2)-contador)/length(lista2)<poda2){
        redundant=which(candidatosUNI==candidatosBI[[i]][2])
        if(length(redundant)==1) candidatosUNI=candidatosUNI[-redundant]
      }
    }
  }
  
  candidatosBI=candidatosBI[-killbi]
  #
  
  killtri=NULL
  for(i in 1:length(candidatosTRI)){
    lista1=unique(opiniones$sentence[which(opiniones$token==candidatosTRI[[i]][1])])
    lista2=unique(opiniones$sentence[which(opiniones$token==candidatosTRI[[i]][2])])
    lista3=unique(opiniones$sentence[which(opiniones$token==candidatosTRI[[i]][3])])
    lista4=lista1[which(match(lista1,lista2,nomatch = 0)>0)]
    lista5=lista1[which(match(lista1,lista3,nomatch = 0)>0)]
    lista6=lista1[which(match(lista2,lista3,nomatch = 0)>0)]
    listatotal=lista3[which(match(lista3,lista4,nomatch = 0)>0)]
    
    #Cuenta las apariciones del candidato
    contador=length(which(sapply(listatotal,function(x) 
    {inside(candidatosTRI[[i]],opiniones[opiniones$sentence==x,]$token)}
    )))
    
    if(contador<poda1*length(listatotal)){
      killtri=c(killtri,i)
    }
    else{
      #Poda de redundancia
      if((length(lista4)-contador)/length(lista4)<poda2){
        redundant=which(sapply(1:length(candidatosBI), function (x)
        {inside(c(candidatosBI[[x]][1],candidatosBI[[x]][2]),c(candidatosTRI[[i]][1],candidatosTRI[[i]][2]))}
        ))
        if(length(redundant)==1) candidatosBI=candidatosBI[-redundant]
      }
      if((length(lista5)-contador)/length(lista5)<poda2){
        redundant=which(sapply(1:length(candidatosBI), function (x)
        {inside(c(candidatosBI[[x]][1],candidatosBI[[x]][3]),c(candidatosTRI[[i]][1],candidatosTRI[[i]][3]))}
        ))
        if(length(redundant)==1) candidatosBI=candidatosBI[-redundant]
      }
      if((length(lista6)-contador)/length(lista6)<poda2){
        redundant=which(sapply(1:length(candidatosBI), function (x)
        {inside(c(candidatosBI[[x]][2],candidatosBI[[x]][3]),c(candidatosTRI[[i]][2],candidatosTRI[[i]][3]))}
        ))
        if(length(redundant)==1) candidatosBI=candidatosBI[-redundant]
      }
    }
  }
  
  candidatosTRI=candidatosTRI[-killtri]
  c(candidatosUNI,candidatosBI,candidatosTRI)

}

OpinionWords=function(frecuentes,opiniones){
  
  longitudes=sapply(1:length(frecuentes),function(x)length(frecuentes[[x]]))
  uni=frecuentes[which(longitudes==1)]
  listaopinion=NULL
  
  for(i in 1:length(uni)){
    lista=unique(opiniones$sentence[opiniones$token==uni[[i]]])
    for(j in 1:length(lista)){
      frase=opiniones[opiniones$sentence==lista[j],]
      ids=which(frase$token==uni[i])
      
      ads=which(EsAdjetivo(frase$POS))
      if(length(ads)>0){
        for(m in 1:length(ids)){
          for(n in 1:length(ads)){
            if(abs(ids[m]-ads[n]<3)){
              listaopinion=unique(c(listaopinion,frase$lemma[ads[n]]))
            }
            
          }
          
        }
        
      }
      
    }
    
  }
  
  listaopinion
}

InfrequentWords=function(frecuentes,listaopinion,opiniones){
  
  listainfrecuentes=NULL
  
  opinionesconfreq=NULL
  for(freq in 1:length(frecuentes)){
    iter=unique(opiniones$sentence[opiniones$lemma %in% frecuentes2[[2]]])
    opinionesconfreq=unique(c(opinionesconfreq,iter))
  }
  
  for(i in 1:length(listaopinion)){
    
    lista=unique(opiniones$sentence[opiniones$lemma==listaopinion[[i]]])
    lista=lista[!lista %in% opinionesconfreq]
    if(length(lista)>1){
      for(j in 1:length(lista)){
        frase=opiniones[opiniones$sentence==lista[j],]
        ids=which(frase$lemma==listaopinion[i])
        
        ads=which(EsNombre(frase$POS))
        if(length(ads)>0&length(ids)>0){
          
          for(m in 1:length(ids)){
            for(n in 1:length(ads)){
              if(!any(is.na(ids[m]))&!any(is.na(ads[n]))){
                if(abs(ids[m]-ads[n]<3)){
                  listainfrecuentes=unique(c(listainfrecuentes,frase$lemma[ads[n]]))
                }
              }
            }
          }
        }
      }
    }
  }
  
  listainfrecuentes
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


#Algoritmo de Hu & Liu
algorithmHL=function(opiniones, soporte){
  
  numeroopiniones=opiniones$sentence[nrow(opiniones)]
  
  candidatosUNI=names(which(summary(as.factor(opiniones
                        [which(EsNombre(opiniones$POS)),]$lemma))>numeroopiniones*soporte))
  
  if(candidatosUNI[length(candidatosUNI)]=="(Other)") candidatosUNI=candidatosUNI[-length(candidatosUNI)]
  
  
  candidatosBITRI=BITRI(Review[1:20],candidatosUNI)
  
  candidatosBITRI=NULL
  frecuentes2=candidatosUNI
  
  if(length(candidatosBITRI)>0){
    frecuentes2=Poda(candidatosUNI,candidatosBITRI,opiniones)
  }
  
  #Infrecuentes
  listaopinion=OpinionWords(frecuentes2,opiniones)
  infrecuentes=InfrequentWords(frecuentes2,listaopinion ,opiniones)
  
  for(i in 1:length(frecuentes2)){
    if(length(frecuentes2[[i]])==2)
      frecuentes2[[i]]=paste(frecuentes2[[i]][1],frecuentes2[[i]][2])
    if(length(frecuentes2[[i]])==3)
      frecuentes2[[i]]=paste(frecuentes2[[i]][1],frecuentes2[[i]][2],frecuentes2[[i]][3])
    
  }
  
  frecuentes2=unlist(frecuentes2)
  aspectos=unique(c(frecuentes2,infrecuentes))
  Aspectos=aspectos[!aspectos %in% stopwords()]
  PurgaListaAspectos(Aspectos)

}
