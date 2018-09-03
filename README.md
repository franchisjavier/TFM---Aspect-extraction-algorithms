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


Para usar los scripts hay que ejecutarlos y luego llamar las funciones según las indicaciones en cada script.

Los algoritmos creados son:

Algoritmo de Hu y Liu [1]
Algoritmo HAC (High Adjective Count) creado por Eirinaki y su equipo [2]
Algoritmo basado en reglas creado por Poria y su equipo [3]
Algoritmo SLDA creado por Yohan Jo y Alice Oh [4]
Algoritmo ASUM creado por Yohan Jo y Alice Oh [4]

[1] Hu, M., & Liu, B. (2004, August). Mining and summarizing customer reviews. In Proceedings of the tenth ACM SIGKDD international conference on Knowledge discovery and data mining (pp. 168-177). ACM.

[2] Eirinaki, M., Pisal, S., & Singh, J. (2012). Feature-based opinion mining and ranking. Journal of Computer and System Sciences, 78(4), 1175-1184.

[3] Poria, S., Cambria, E., Ku, L. W., Gui, C., & Gelbukh, A. (2014, August). A rule-based approach to aspect extraction from product reviews. In Proceedings of the second workshop on natural language processing for social media (SocialNLP) (pp. 28-37)

[4] Jo, Y., & Oh, A. H. (2011, February). Aspect and sentiment unification model for online review analysis. In Proceedings of the fourth ACM international conference on Web search and data mining (pp. 815-824). ACM.
