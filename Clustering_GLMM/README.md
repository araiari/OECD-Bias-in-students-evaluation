Clustering delle scuole attraverso GLMM con sample da DP

DATASET:
dati_all.csv contiene il dataset iniziale completo
dati_glmmDP.csv è la parte di dataset utilizzata per il modello GLMM (da cui si ricava poi il clustering delle scuole)
labels.csv contiene le etichette dei cluster di ciascuna scuola.

SCRIPT:
glmm_clustering.R contiene il processo di sampling da un Dirichlet Process che genera un modello lineare a effetti misti con intercetta casuale, e poi ricava la partizione ottimale in cluster delle scuole rispetto agli effetti casuali.
GLMM_DP_Pisa.bug è il file bug utilizzato da JAGS.
GLMMDP_Pisa_output.Rdata è il risultato dell'algoritmo di sampling prodotto da JAGS.

-------------------------------------------------------------------
Clustering of schools by means of GLMM sampling from a DP

DATASETS:
dati_all.csv contains the full original dataset
dati_glmmDP.csv was used in the GLMM model to cluster the schools
labels.csv contains the label of the schools resulting from clustering

SCRIPTS:
glmm_clustering.R samples from a Dirichlet Process and generates a GLMM (including the intercept)
GLMM_DP_Pisa.bug is the bug file used in JAGS
GLMMDP_Pisa_output.Rdata is the workspace with the results of the algorithm
