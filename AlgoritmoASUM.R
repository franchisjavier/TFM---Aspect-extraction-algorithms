
# Implementación del algoritmo ASUM creado por Yohan Jo y Alice Oh en:

# Jo, Y., & Oh, A. H. (2011, February). Aspect and sentiment unification model
# for online review analysis. In Proceedings of the fourth ACM international
# conference on Web search and data mining (pp. 815-824). ACM.

# Para usarlo hay que ejecutar el script entero y llamar a la función: algorithmASUM(opiniones, depend, apha, betas, gammas)
# opiniones: aquí se introduce la salida de NLP
# depend: aquí se introduce la salida de NLP
# alpha, betas y gammas son los parámetros así llamados en el paper.

library(tm)

calculateThetaASUM= function(matrixSDT,sumDST,alpha,sumAlpha){
  
  numSenti=nrow(matrixSDT)
  numDocs=ncol(matrixSDT)
  numTopics=length(matrixSDT[1,1,])
  
  Theta=array(0,dim=c(numSenti,numDocs,numTopics)) 
  
  for(i in 1:numSenti){
    for(j in 1:numDocs){
      for(k in 1:numTopics) Theta[i,j,k]=(matrixSDT[i,j,k]+alpha)/(sumDST[j,i]+sumAlpha)
    }
  }
  
  Theta
  
}

calculatePiASUM= function(matrixDS,sumDS,gammas,sumGamma){
  
  numDocs=nrow(matrixDS)
  numSenti=ncol(matrixDS)
  
  Pi=array(0,dim=c(numDocs,numSenti)) 
  
  for(s in 1:numSenti){
    for(d in 1:numDocs){
      Pi[d,s]=(matrixDS[d,s]+gammas[s])/(sumDS[d]+sumGamma)
    }
  }
  
  Pi
  
}

calculatePhiASUM= function(matrixSWT,sumSTW,betas,sumBeta,wordLexicons){
  
  numSenti=nrow(matrixSWT)
  numWords=ncol(matrixSWT)
  numTopics=length(matrixSWT[1,1,])
  
  Phi=array(0,dim=c(numSenti,numWords,numTopics)) 
  
  for(i in 1:numSenti){
    for(j in 1:numWords){
      if(wordLexicons[j]==0) beta=betas[1]
      else if (wordLexicons[j]==i) beta=betas[2]
      else beta=betas[3]
      
      for(k in 1:numTopics) Phi[i,j,k]=(matrixSWT[i,j,k]+beta)/(sumSTW[i,k]+sumBeta[i])
    }
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
algorithmASUM=function(opiniones, depend, alpha, betas, gammas){
  
  numIterations=100  #Son 100 en el paper original
  
  numTopics=10
  numSenti=2
  numDocuments=1
  
  for(d in 1:numDocuments){
    #opiniones=documentos[d]
    opinionesnot=opiniones
    PosNeg=which(opiniones$lemma=="good"|opiniones$lemma=="like"|opiniones$lemma=="recommend"|opiniones$lemma=="worth")
    SureNeg=PosNeg[which(opiniones$lemma[(PosNeg-1)]=="not"|opiniones$lemma[(PosNeg-1)]=="Not")]
    if(length(SureNeg>0)){
      opinionesnot$token[SureNeg]=sapply(SureNeg,function(x) paste("not",opinionesnot$lemma[x]))
      opinionesnot$lemma[SureNeg]=sapply(SureNeg,function(x) paste("not",opinionesnot$lemma[x]))
      opinionesnot=opinionesnot[-(SureNeg-1),]
    }
    #documentos[d]=opinionesnot
  }
  
  wordList=NULL
  for(d in 1:numDocuments){
    
    #opinionesnot=documentos[d]
    wordList=unique(c(opinionesnot$lemma,wordList))
    
  }
  
  numUniqueWords = length(wordList)
  
  sentiWordList = list(c("good", "nice", "excellent", "positive", "fortunate", "correct", "superior", 
                         "amazing", "attractive", "awesome", "best", "comfortable", "enjoy", "fantastic",
                         "favorite", "fun", "glad", "great", "happy", "impressive", "love", "perfect",
                         "recommend", "satisfied", "thank", "worth"),c( "bad", "nasty", "poor", "negative",
                        "unfortunate", "wrong", "inferior", "annoying", "complain", "disappointed",
                        "hate", "junk", "mess", "problem", "regret", "sorry", "terrible", "trouble",
                        "unacceptable", "upset", "waste", "worst", "worthless","not good","not like", 
                         "not recommend", "not worth"))
  
  numSentiWords=sum(sapply(sentiWordList,length))
  numProbWords=50 #Número de palabras probables
  
  sumAlpha = alpha*numTopics
  sumBeta=seq(0,0,length.out=numSenti)
  sumBetaCommon = betas[1]*(numUniqueWords - numSentiWords)
  for(s in 1:numSenti){
    numLexiconWords=0
    if(numSenti>s) numLexiconWords=length(sentiWordList[[s]])
    sumBeta[s]=sumBetaCommon + betas[2]*numLexiconWords + betas[3]*(numSentiWords-numLexiconWords)
  }
  sumGamma=sum(gammas)
  
  probTable = array(0,dim=c(numTopics,numSenti))
  
  sentenceST=list(seq(0,0,length.out=numDocuments))
  for(d in 1:numDocuments){
    
    numSentences=opinionesnot$sentence[nrow(opinionesnot)]
    sentenceS=seq(0,0,length.out=numSentences)
    sentenceT=seq(0,0,length.out=numSentences)
    sentenceST[[d]]=cbind(sentenceS,sentenceT)
  }
  
  sumSTW = array(0,dim=c(numSenti,numTopics))
  sumDST = array(0,dim=c(numDocuments,numSenti))
  sumDS = seq(0,0,length.out=numDocuments)
  
  matrixSWT = array(0,dim=c(numSenti,numUniqueWords,numTopics)) 
  matrixSDT = array(0,dim=c(numSenti,numDocuments,numTopics))
  matrixDS = array(0,dim=c(numDocuments,numSenti))
  
  #Inizialization
  for(i in 1:numDocuments){
    
    #opinionesnot=documentos[i]
    numSentences=opinionesnot$sentence[nrow(opinionesnot)]
    
    for(j in 1:numSentences){
      
      newSenti = -1
      numSentenceSenti = 0
      frase=opinionesnot[opinionesnot$sentence==j,]
      
      palabras=unique(frase$lemma)
      numWords=length(palabras)
      
      for(s in 1:numSenti){
        hay=length(which(palabras %in% sentiWordList[[s]])) 
        if(hay>0){
          newSenti=s
          numSentenceSenti=numSentenceSenti+1
        }
      }

      if (numSentenceSenti!=1) newSenti=sample(1:numSenti,1)
      newTopic=sample(1:numTopics,1)
      
      if (numSentenceSenti<=1){
        
        sentenceST[[i]][j,1]=newSenti
        sentenceST[[i]][j,2]=newTopic
        
        for(k in 1:numWords){
          
          wordNo=which(palabras[k]==wordList)
          matrixSWT[newSenti,wordNo,newTopic]=matrixSWT[newSenti,wordNo,newTopic]+1
          
          sumSTW[newSenti,newTopic]=sumSTW[newSenti,newTopic]+1
          
        }
        
        matrixSDT[newSenti,i,newTopic]=matrixSDT[newSenti,i,newTopic]+1
        matrixDS[i,newSenti]=matrixDS[i,newSenti]+1
        
        sumDST[i,newSenti]=sumDST[i,newSenti]+1
        sumDS[i]=sumDS[i]+1
      }
    }
  }
  
  wordLexicons=seq(0,0,length.out=numUniqueWords)
  for(s in 1:numSenti) wordLexicons[which(wordList %in% sentiWordList[[s]])]=s
  
  for(it in 1:numIterations){
    
    print(paste("iteration",it))
    
    for(i in 1:numDocuments){
      
      #opinionesnot=documentos[i]
      numSentences=opinionesnot$sentence[nrow(opinionesnot)]
      
      for(j in 1: numSentences){
        
        sumProb=0
        
        frase=opinionesnot[opinionesnot$sentence==j,]
        palabras=unique(frase$lemma)
        numWords=length(palabras)
        
        
        oldSenti=sentenceST[[i]][j,1]
        oldTopic=sentenceST[[i]][j,2]
        
        matrixSDT[oldSenti,i,oldTopic]=matrixSDT[oldSenti,i,oldTopic]-1
        matrixDS[i,oldSenti]=matrixDS[i,oldSenti]-1
        
        sumDST[i,oldSenti]=sumDST[i,oldSenti]-1
        sumDS[i]=sumDS[i]-1
        
        for(k in 1:numWords){
          
          wordNo=which(palabras[k]==wordList)
          matrixSWT[oldSenti,wordNo,oldTopic]=matrixSWT[oldSenti,wordNo,oldTopic]-1
          
          sumSTW[oldSenti,oldTopic]=sumSTW[oldSenti,oldTopic]-1
          
        }

        #Sampling
        
        for(si in 1:numSenti){
          
          trim=FALSE
          
          for(k in 1:numWords){
            sentiW=wordLexicons[which(palabras[k]==wordList)]
            if(sentiW>0 & sentiW!=si){
              trim=TRUE
              break
            }
          }
          if(trim){
            probTable[,si] = seq(0,0,length.out=numTopics)
          }
          else{
            for(ti in 1:numTopics){
              beta0 = sumSTW[si,ti] + sumBeta[si]
              m0 = 0
              expectTSW = 1
              for(k in 1:numWords){
                
                wordNo=which(palabras[k]==wordList)
                if(wordLexicons[wordNo]==0) beta=betas[1]
                else if (wordLexicons[wordNo]==si) beta=betas[2]
                else beta=betas[3]
                
                betaw = matrixSWT[si,wordNo, ti] + beta
                
                cnt = length(which(palabras[k]==frase$lemma))

                for (m in 1:cnt) {
                  expectTSW = expectTSW*(betaw + m) / (beta0 + m0)
                  m0=m0+1
                }
                
              }
              
              probTable[ti,si]=((matrixSDT[si,i,ti] + alpha)/(sumDST[i,si]+sumAlpha))*(matrixDS[i, si]+gammas[si])*expectTSW 
              
              sumProb= sumProb+probTable[ti,si]
              
            }
          }
        }
        
        newTopic = 0
        newSenti = 0
        randNo = runif(1)*sumProb
        tmpSumProb = 0
        found = FALSE
        
        for (ti in 1:numTopics) {
          for (si in 1:numSenti) {
            tmpSumProb= tmpSumProb+probTable[ti,si]
            if (randNo <= tmpSumProb) {
              newTopic = ti
              newSenti = si
              found = TRUE
            }
            if (found) break
          }
          if (found) break
        }
        
        sentenceST[[i]][j,1]=newSenti
        sentenceST[[i]][j,2]=newTopic
        
        for(k in 1:numWords){
          
          wordNo=which(palabras[k]==wordList)
          matrixSWT[newSenti,wordNo,newTopic]=matrixSWT[newSenti,wordNo,newTopic]+1
          
          sumSTW[newSenti,newTopic]=sumSTW[newSenti,newTopic]+1
          
        }
        
        matrixSDT[newSenti,i,newTopic]=matrixSDT[newSenti,i,newTopic]+1
        matrixDS[i,newSenti]=matrixDS[i,newSenti]+1
        
        sumDST[i,newSenti]=sumDST[i,newSenti]+1
        sumDS[i]=sumDS[i]+1
        
      }
    }
  }
  
  Phi=calculatePhiASUM(matrixSWT,sumSTW,betas,sumBeta,wordLexicons)
  Theta=calculateThetaASUM(matrixSDT,sumDST,alpha,sumAlpha)
  Pi=calculatePiASUM(matrixDS,sumDS,gammas,sumGamma)
  
  wordIndices = array(0,dim=c(numSenti,numTopics,numProbWords)) 
  
  for(s in 1:numSenti){
    for(t in 1:numTopics){
      sortedIndexList=order(Phi[s,,t],decreasing = TRUE)[1:numProbWords]
      wordIndices[s,t,]=sortedIndexList
    }
  }
  
  AspectosASUM=NULL
  
  for(s in 1:numSenti){
    for(t in 1:numTopics){
      AspectosASUM=unique(c(AspectosASUM,wordList[wordIndices[s,t,]]))
    }
  }
  
  AspectosASUM=AspectosASUM[!AspectosASUM %in% stopwords()]
  AspectosASUM=AspectosASUM[grep("^[A-Za-z]+$",AspectosASUM)]
  PurgaListaAspectos(AspectosASUM)

}
