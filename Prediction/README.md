In  questa cartella sono contenuti gli script con i quali abbiamo fatto previsione del voto.

Ci siamo rifatti al caso di risposta multivariata [voto math, voto read] nel contesto del modello gerarchico.
Il modello di riferimento è comunque riportato anche in questa cartella: il file modello_hierarchical_biv.R e il file
modello_hierarchical_biv.bug sono stati utilizzati per campionare dal modello.

Sono stati utilizzati i dati contanuti nel file dati_bivariato.csv. Qui è riportato il workspace preparazione_dati.RData 
nel quale i dati sono già sistemati.

Abbiamo anche tentato a utilizzare lo stesso modello con dati standardizzati o un modello simile con una prior 
sulla media delle intercette variabili gamma0j, ma il risultato migliore è quello che riportiamo qui.

Lo script previsione_hier_biv.R è stato usato per valutare la predictive distribution dei voti sia nel caso di 
nuovo studente da scuola esistente sia nel caso di nuovo studente da nuova scuola.

--------------------------------------------------------------------------

The folder contains all the scripts to predict the evaluation.

We considered the univariate hierarchical model (response: math evaluation).
One can find the description of the model in modello_hierarchical_biv.R and
modello_hierarchical_biv.bug , which were used to sample from the model with JAGS.

The analysis was performed on the dataset dati_bivariato.csv. 
This folder contains the workspace preparazione_dati.RData with them.

We tried to use the same model and method for standardized data. We also tried a slightly different model, assigning a prior on the mean of gamma0j.
The model we chose resulted as the best one between them.

We used the script previsione_hier_biv.R to evaluate the predictive distribution of a new student's evaluation 
both if she is from an existing school and if she is from a new school.


