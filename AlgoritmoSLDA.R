
# Implementación del algoritmo SLDA creado por Yohan Jo y Alice Oh en:

# Jo, Y., & Oh, A. H. (2011, February). Aspect and sentiment unification model
# for online review analysis. In Proceedings of the fourth ACM international
# conference on Web search and data mining (pp. 815-824). ACM.

# Para usarlo hay que ejecutar el script entero y llamar a la función: algorithmSLDA(opiniones, depend, apha, beta)
# opiniones: aquí se introduce la salida de NLP
# depend: aquí se introduce la salida de NLP
# alpha y beta son los parámetros así llamados en el paper.

library(tm)

calculateThetaSLDA= function(matrixSDT,sumDST,alpha,sumAlpha){
  
  numDocs=nrow(matrixSDT)
  numTopics=ncol(matrixSDT)
  
  Theta=array(0,dim=c(numDocs,numTopics)) 
  
  for(j in 1:numDocs){
    for(k in 1:numTopics) Theta[j,k]=(matrixSDT[j,k]+alpha)/(sumDST[j]+sumAlpha)
  }
  
  Theta
  
}

calculatePhiSLDA= function(matrixSWT,sumSTW,beta,sumBeta){
  
  numWords=nrow(matrixSWT)
  numTopics=ncol(matrixSWT)
  
  Phi=array(0,dim=c(numWords,numTopics)) 
  
  for(j in 1:numWords){
    for(k in 1:numTopics) Phi[j,k]=(matrixSWT[j,k]+beta)/(sumSTW[k]+sumBeta)
  }
  
  Phi
  
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
algorithmSLDA=function(opiniones,depend, alpha, beta){
  
  
  numIterations=100  #Son 100 en el paper original
  numTopics=10
  numDocuments=1
  numProbWords=50 #Número de palabras probables
  
  wordList=NULL
  for(d in 1:numDocuments){
    
    #opiniones=documentos[d]
    wordList=unique(c(opiniones$lemma,wordList))
    
  }
  
  numUniqueWords = length(wordList)
  
  sumAlpha = alpha*numTopics
  sumBeta=beta*(numUniqueWords)
  
  probTable =seq(0,0,length.out=numTopics)
  
  sentenceST=list(seq(0,0,length.out=numDocuments)) 
  for(d in 1:numDocuments){
    numSentences=opiniones$sentence[nrow(opiniones)]
    sentenceST[[d]]=seq(0,0,length.out=numSentences)
  }
  
  sumSTW = seq(0,0,length.out=numTopics)
  sumDST = seq(0,0,length.out=numDocuments)
  
  
  matrixSWT = array(0,dim=c(numUniqueWords,numTopics)) 
  matrixSDT = array(0,dim=c(numDocuments,numTopics))
  
  #Inizialization
  for(i in 1:numDocuments){
    
    #opiniones=documentos[i]
    numSentences=opiniones$sentence[nrow(opiniones)]
    
    for(j in 1:numSentences){
      
      frase=opiniones[opiniones$sentence==j,]
      
      palabras=unique(frase$lemma)
      numWords=length(palabras)
      
      newTopic=sample(1:numTopics,1)
      
      sentenceST[[i]][j]=newTopic
      
      for(k in 1:numWords){
        
        wordNo=which(palabras[k]==wordList)
        matrixSWT[wordNo,newTopic]=matrixSWT[wordNo,newTopic]+1
        
        sumSTW[newTopic]=sumSTW[newTopic]+1
        
      }
      
      matrixSDT[i,newTopic]=matrixSDT[i,newTopic]+1
      
      sumDST[i]=sumDST[i]+1
      
    }
  }
  
  for(it in 1:numIterations){
    
    print(paste("iteration",it))
    
    for(i in 1:numDocuments){
      
      #opiniones=documentos[i]
      numSentences=opiniones$sentence[nrow(opiniones)]
      
      for(j in 1: numSentences){
        
        sumProb=0
        
        frase=opiniones[opiniones$sentence==j,]
        palabras=unique(frase$lemma)
        numWords=length(palabras)
        
        oldTopic=sentenceST[[i]][j]
        
        matrixSDT[i,oldTopic]=matrixSDT[i,oldTopic]-1
        sumDST[i]=sumDST[i]-1
        
        for(k in 1:numWords){
          
          wordNo=which(palabras[k]==wordList)
          matrixSWT[wordNo,oldTopic]=matrixSWT[wordNo,oldTopic]-1
          sumSTW[oldTopic]=sumSTW[oldTopic]-1
          
        }
        
        #Sampling
        
        for(ti in 1:numTopics){
          beta0 = sumSTW[ti] + sumBeta
          m0 = 0
          expectTSW = 1
          for(k in 1:numWords){
            
            wordNo=which(palabras[k]==wordList)
            
            betaw = matrixSWT[wordNo, ti] + beta
            
            cnt = length(which(palabras[k]==frase$lemma))

            for (m in 1:cnt) {
              expectTSW = expectTSW*(betaw + m) / (beta0 + m0)
              m0=m0+1
            }
            
          }
          
          probTable[ti]=((matrixSDT[i,ti] + alpha)/(sumDST[i]+sumAlpha))*expectTSW 
          
          sumProb= sumProb+probTable[ti]
          
        }
        
        newTopic = 0
        randNo = runif(1)*sumProb
        tmpSumProb = 0
        found = FALSE
        
        for (ti in 1:numTopics) {
          tmpSumProb= tmpSumProb+probTable[ti]
          if (randNo <= tmpSumProb) {
            newTopic = ti
            found = TRUE
          }
          if (found) break
        }
        
        sentenceST[[i]][j]=newTopic
        
        for(k in 1:numWords){
          
          wordNo=which(palabras[k]==wordList)
          matrixSWT[wordNo,newTopic]=matrixSWT[wordNo,newTopic]+1
          sumSTW[newTopic]=sumSTW[newTopic]+1
          
        }
        
        matrixSDT[i,newTopic]=matrixSDT[i,newTopic]+1
        sumDST[i]=sumDST[i]+1
        
      }
      
    }
    
  }
  
  Phi=calculatePhiSLDA(matrixSWT,sumSTW,beta,sumBeta)
  Theta=calculateThetaSLDA(matrixSDT,sumDST,alpha,sumAlpha)
  
  wordIndices = array(0,dim=c(numTopics,numProbWords))
  
  
  for(t in 1:numTopics){
    sortedIndexList=order(Phi[,t],decreasing = TRUE)[1:numProbWords]
    wordIndices[t,]=sortedIndexList
  }
  
  AspectosSLDA=NULL
  
  for(t in 1:numTopics){

    AspectosSLDA=unique(c(AspectosSLDA,wordList[wordIndices[t,]]))
    
  }
  
  AspectosSLDA=AspectosSLDA[!AspectosSLDA %in% stopwords()]
  AspectosSLDA=AspectosSLDA[grep("^[A-Za-z]+$",AspectosSLDA)]
  PurgaListaAspectos(AspectosSLDA)

}
