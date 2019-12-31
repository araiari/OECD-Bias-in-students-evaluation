
Nel file covariate_selection_con_WAIC.R sono fittati diversi modelli lineari a effetti misti con le covariate dello studente
(precedentemente selezionate, vedi cartella modello_lineare) e con una sola covariata relativa alla scuola.
In particolare, il modello gerarchico senza convariate della scuola è stato confrontato con i modelli gerarchici con una 
covariata della scuola. 

E' stato utilizzato JAGS per campionare dai diversi modelli. 
Nei files simple_LMM2.bug e one_school_LMM2.bug sono esplicitati i modelli utilizzati (priors, iperparametri, ...). 
simple_LMM2.bug è utilizzato per il modello a effetti misti senza covariate scuola, one_school_LMM2.bug è utilizzato per il
modello a effetti misti con una covariata scuola.

I diversi modelli sono stati confrontati in base alla loro capacità predittiva calcolandone l'indice WAIC.
I risultati sono contenuti nel dile risultati_WAIC.dat
