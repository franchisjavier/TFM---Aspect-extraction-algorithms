
# Implementación del algoritmo creado por Poria y su equipo en:

# Poria, S., Cambria, E., Ku, L. W., Gui, C., & Gelbukh, A. (2014, August). A
# rule-based approach to aspect extraction from product reviews. In Proceedings
# of the second workshop on natural language processing for social media (SocialNLP)
# (pp. 28-37)

# Necesita cargar el archivo totalWN.txt el cual contiene una lista de IACS

# Para usarlo hay que ejecutar el script entero y llamar a la función: algorithmReglas(opiniones, depend)
# opiniones: aquí se introduce la salida de NLP
# depend: aquí se introduce la salida de NLP

library(tm)
library(lexicon)

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

# Funciones para usar con el POS del coreNLP
EsNombre= function(palabra){(palabra=="NN" | palabra=="NNP" | palabra=="NNS" | palabra=="NNPS")}
EsAdjetivo= function(palabra){(palabra=="JJ" | palabra=="JJR" | palabra=="JJS")}
EsAdverbio= function(palabra){(palabra=="RB" | palabra=="RBR" | palabra=="RBS" | palabra=="WRB")}
EsVerbo= function(palabra){(palabra=="VB" | palabra=="VBD" | palabra=="VBG" | palabra=="VBN"
                            | palabra=="VBP" | palabra=="VBZ")}
EsPreposicion= function(palabra){(palabra=="IN")}
EsModificador=function(tipo){(tipo=="amod" | tipo=="appos" | tipo=="advcl" | tipo=="det" 
                              | tipo=="predet" | tipo=="preconj" | tipo=="vmod" | tipo=="mwe"
                              | tipo=="mark" | tipo=="advmod" | tipo=="neg" | tipo=="rcmod" 
                              | tipo=="quantmod" | tipo=="nn" | tipo=="npadvmod" | tipo=="tmod"
                              | tipo=="num" | tipo=="number" | tipo=="prep" | tipo=="poss" 
                              | tipo=="possessive" | tipo=="prt" | tipo=="mod" | tipo=="nmod"  )}
EsModificadorAA=function(tipo){(tipo=="amod" | tipo=="advcl" |  tipo=="advmod" |  tipo=="npadvmod" )}
EsAuxiliar=function(tipo){(tipo=="aux")}
EsDobj=function(tipo){(tipo=="dobj")}
EsOpenClCl=function(tipo){(tipo=="xcomp")}
EsCopula=function(tipo){(tipo=="cop")}
EsCConj=function(tipo){(tipo=="cc"|tipo=="conj")}
EsCompuesto=function(tipo){(tipo=="compound")}
EsPrep=function(tipo){(tipo=="case")}
EsNSubject=function(tipo){( tipo=="nsubj" | tipo=="nsubjpass")}


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
algorithmReglas=function(opiniones,depend){
  
  senticnet=hash_sentiment_senticnet
  IACS=scan("IACS/totalWN.txt",what="character",sep="\n")
  
  numeroopiniones=opiniones$sentence[nrow(opiniones)]
  
  aspectList=NULL
  
  for(i in 1:numeroopiniones){
    
    frase=opiniones[opiniones$sentence==i,]
    dep=depend[depend$sentence==i,]
    iteraspectList=NULL
    
    if(any(EsNSubject(dep$type))){ 
      tIds=dep[which(EsNSubject(dep$type)),]$governorIdx
      hIds=dep[which(EsNSubject(dep$type)),]$dependentIdx
      
      for(j in 1:length(tIds)){
        tword=frase[which(frase$id==tIds[j]),]$lemma
        hword=frase[which(frase$id==hIds[j]),]$lemma
        
        #Rule 1
        if(any(EsModificadorAA(dep[which(tIds[j]==dep$dependentIdx|tIds[j]==dep$governorIdx),]$type))){
          nId=dep[which(dep$governorIdx==tIds[j] & EsModificadorAA(dep$type)),]$dependentIdx
          if (length(nId)<1) 
            nId=dep[which(dep$dependentIdx==tIds[j] & EsModificadorAA(dep$type)),]$governorIdx
          activador=FALSE
          for(k in 1:length(nId)){
            if(inside(frase[frase$id==nId[k],]$lemma,senticnet))
              activador=TRUE
          }
          if(activador)
            iteraspectList=c(iteraspectList,tword)
        }
        
        #Rule2  
        if(!any(EsAuxiliar(dep$type))){

          if(any(EsModificadorAA(dep[which(tIds[j]==dep$dependentIdx|tIds[j]==dep$governorIdx),]$type))){
            iteraspectList=c(iteraspectList,tword,hword)
          }
          
          if(any(EsDobj(dep[which(tIds[j]==dep$dependentIdx|tIds[j]==dep$governorIdx),]$type))){
            nId=dep[which(dep$governorIdx==tIds[j] & EsDobj(dep$type)),]$dependentIdx
            if (length(nId)<1) 
              nId=dep[which(dep$dependentIdx==tIds[j] & EsDobj(dep$type)),]$governorIdx
            for(k in 1:length(nId)){
              nword=frase[frase$id==nId[k],]$lemma

              if(EsNombre(frase[frase$id==nId[k],]$POS)&!inside(nword,senticnet))
                iteraspectList=c(iteraspectList,nword)
 
              if(EsNombre(frase[frase$id==nId[k],]$POS)&inside(nword,senticnet)){
                iteraspectList=c(iteraspectList,nword)
                nId2=dep[which(dep$governorIdx==nId[k] & EsNombre(frase$POS)),]$dependentIdx
                if (length(nId2)<1) 
                  nId2=dep[which(dep$dependentIdx==nId[k] & EsNombre(frase$POS)),]$governorIdx
                iteraspectList=c(iteraspectList,frase$lemma[as.integer(nId2)])
              }
            }
          }
          
          if(any(EsOpenClCl(dep[which(tIds[j]==dep$dependentIdx|tIds[j]==dep$governorIdx),]$type))){
            t1Id=dep[which(dep$governorIdx==tIds[j] & EsOpenClCl(dep$type)),]$dependentIdx
            if (length(t1Id)<1) 
              t1Id=dep[which(dep$dependentIdx==tIds[j] & EsOpenClCl(dep$type)),]$governorIdx
            for (k in 1:length(t1Id)){
              t1word=frase[frase$id==t1Id[k],]$lemma
              if(inside(t1word,senticnet)&inside(tword,senticnet))
                iteraspectList=c(iteraspectList,paste(tword,t1word))
              t2Id=dep[which(dep$governorIdx==t1Id[k] & EsNombre(frase$POS)),]$dependentIdx
              if (length(t2Id)<1) 
                t2Id=dep[which(dep$dependentIdx==t1Id[k] & EsNombre(frase$POS)),]$governorIdx
              iteraspectList=c(iteraspectList,frase$lemma[as.integer(t2Id)])
            }
          }
        }
        
        #Rule3, Rule 4 & Rule 5
        if(any(EsCopula(dep[which(tIds[j]==dep$dependentIdx|tIds[j]==dep$governorIdx),]$type))){
          nId=dep[dep$governorIdx==tIds[j] & EsCopula(dep$type),]$dependentIdx
          if (length(nId)<1) 
            nId=dep[dep$dependentIdx==tIds[j] & EsCopula(dep$type),]$governorIdx
          if(inside(frase[frase$id==nId,]$lemma,IACS)) 
            iteraspectList=c(iteraspectList,frase[frase$id==nId,]$lemma) 
          if(EsNombre(frase[which(frase$id==hIds[j]),]$POS))
            iteraspectList=c(iteraspectList,hword)

          for(k in 1:length(nId)){
            nword=frase[frase$id==nId[k],]$lemma
            nId2=dep[dep$governorIdx==nId[k] & EsVerbo(frase$POS),]$dependentIdx
            if (length(nId2)<1) 
              nId2=dep[dep$dependentIdx==nId[k] & EsVerbo(frase$POS),]$governorIdx
            if(inside(frase[frase$id==nId2,]$lemma,IACS)&inside(nword,IACS)){
              n2word=frase[frase$id==nId2[k],]$lemma
              iteraspectList=c(iteraspectList,nword,n2word)
            }
          }
        }
      }
    }
    else{ 
      if(any(EsOpenClCl(dep$type))){
        
        tIds=dep[which(EsOpenClCl(dep$type)),]$governorIdx
        hIds=dep[which(EsOpenClCl(dep$type)),]$dependentIdx
        
        for(j in 1:length(tIds)){
          
          tword=frase[which(frase$id==tIds[j]),]$lemma
          hword=frase[which(frase$id==hIds[j]),]$lemma
          if(EsAdjetivo(frase[frase$id==hIds[j],]$POS)|EsAdverbio(frase[frase$id==hIds[j],]$POS)){
            if(inside(hword,IACS)){
              iteraspectList=c(iteraspectList,hword)
            }
          }
        }
      }
      
      if(any(EsPrep(dep$type))){
        
        tIds=dep[which(EsPrep(dep$type)),]$governorIdx
        hIds=dep[which(EsPrep(dep$type)),]$dependentIdx
        
        for(j in 1:length(tIds)){
          
          tword=frase[which(frase$id==tIds[j]),]$lemma
          if(EsPreposicion(frase[frase$id==hIds[j],]$POS)|EsNombre(frase[frase$id==tIds[j],]$POS)){
            iteraspectList=c(iteraspectList,tword)
          }
        }
      }
      
      if(any(EsDobj(dep$type))){
        
        tIds=dep[which(EsDobj(dep$type)),]$governorIdx
        hIds=dep[which(EsDobj(dep$type)),]$dependentIdx
        
        for(j in 1:length(tIds)){
          tword=frase[which(frase$id==tIds[j]),]$lemma
          iteraspectList=c(iteraspectList,tword)
        }
      }
    }
    
    #Extra rules
    iteraspectList=unique(iteraspectList)
    
    for(iaspect in 1:length(iteraspectList)){
      
      hId=frase[frase$lemma==iteraspectList[iaspect],]$id
      
      if(length(hId)>0){
        
        for(k in 1:length(hId)){
          tId=dep[EsCConj(dep$type)&dep$dependentIdx==hId[k],]$governorIdx
          
          if(length(tId)>0){
            for(h in 1:length(tId)){
              tword=frase[frase$id==tId[h],]$lemma
              iteraspectList=c(iteraspectList,tword)
              
            }
          }
        }
      }
    }

    for(iaspect in 1:length(iteraspectList)){
      
      hId=frase[frase$lemma==iteraspectList[iaspect],]$id
      
      if(length(hId)>0){
        
        for(k in 1:length(hId)){
          tId=dep[EsCompuesto(dep$type)&dep$dependentIdx==hId[k],]$governorIdx
          
          if(length(tId)>0 ){
            if(!any(is.na(tId))){
              for(h in 1:length(tId)){
                if(EsNombre(frase[frase$id==hId[k],]$POS)&EsNombre(frase[frase$id==tId[h],]$POS)){
                  
                  hword=frase[frase$id==hId[k],]$lemma
                  tword=frase[frase$id==tId[h],]$lemma
                  iteraspectList[iaspect]=paste(hword,tword)
                }
              }
            }
          }
        }
      }
    }

    aspectList=unique(c(aspectList,iteraspectList))
    
  }
  
  AspectosReglas=aspectList[!aspectList %in% stopwords()]
  PurgaListaAspectos(AspectosReglas)

}