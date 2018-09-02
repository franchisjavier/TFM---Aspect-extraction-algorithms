# TFM---Aspect-extraction-algorithms

Aquí se presentan varias implementaciones en R de diferentes algoritmos de extracción de aspectos.

Los datasets utilizados son:

sanders.txt
debate.txt
nikolaos_ted.txt
vader_amazon.txt
vader_movie.txt

Estos datasets están extraídos de diferentes fuentes y cada una de sus opiniones está manualmente clasificada en los siguientes archivos:

sanders_so_score.txt,
debate_so_score.txt
nikolaos_ted_so_score.txt
vader_amazon_so_score.txt
vader_movie_so_score.txt

Para más información sobre los datasets:
Ribeiro, F. N., Araújo, M., Gonçalves, P., Gonçalves, M. A., & Benevenuto, F. (2016). SentiBench-a benchmark comparison of state-of-the-practice sentiment analysis methods. EPJ Data Science, 5(1), 23.


Los algoritmos de entrada van a necesitar de entrada unos datasets producidos por el paquete coreNLP.
Se puede ver un ejemplo en el script "LeerReviewsNLP.R".
Lo que hace este script es transformar el archivo "vader_amazon.txt" en dos datasets llamados "opiniones" y "depend". El primero contiene información sobre el POS de las palabras, el segundo tiene incluido el parse tree de las opiniones.
