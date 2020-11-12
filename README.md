# Pisa-BurzacchiFalcoTeodori
L'obiettivo del progetto è capire quali sono i fattori che impattano maggiormente sui voti degli studenti italiani.
A partire dai dati dell'OCSE, che forniscono numerose caratterstiche sugli studenti e sulle scuole da loro frequentate,
si vuole costruire un modello gerarchico che abbia come risposta i voti degli studenti.
Ogni analisi è stata trattata con un approccio bayesiano.
Data la numerosità delle covariate presenti (più di 1000) si è prima proceduto con una scrematura iniziale.
L'analisi vera e propria scremerà ulteriormente le covariate.
L'analisi è stata effettuata rimuovendo le righe con dati mancanti,
in quanto le informazioni presenti non sono state ritenute sufficienti per imputare i dati mancanti in modo soddisfacente.
Ogni analisi è stata trattata con un approccio bayesiano.


La cartella dataset contiene i diversi dataset utilizzati.

Per i modelli lineari guardare solo la cartella modelli_finali (il resto sono codici/risultati vecchi).
Al suo interno è presente l'analisi del modello univariato (risposta: voto_matematica) e del modello bivariato (risposta: voto_matematica, voto_reading). Ogni sottocartella contiene un file read.me che descrive il contenuto della cartella stessa. E' inoltre presente un pdf con i modelli implementati.

Nella cartella ANOVA sono contenuti gli script con cui si è analizzata la differenza dei voti 
nelle diverse regioni (Campania, Tentino Alto Adige e Lombardia, le uniche di cui si aveva informazione) e i risutati dei test di ipotesi in merito.

La cartella Clustering_GLMM contiene l'analisi di clusterizzazione delle scuole.

Per una presentazione dei modelli e dei risultati, fare riferimento a ProjectPresentation.pdf

-------------------------------------------------------------------

The aim of the project is to understand which factors influence italian students' evaluations.
The OCSE dataset contains many information about the students and their schools. 
A Bayesian approach has been used in every analysis.
Starting from it, a hierarchical model was built to predict the students' evaluations.
Due to the large amount of covariates (more than 1000), we firsly proceed with an initial selection.
A further covariate selection has been done with waic index, cross validation and penalized regression.
We removed all of the observations with missing data, since the information carried by them were not sufficient to impute data adequately.


Folder dataset contains the different datasets that were used.

To investigate linear models, just check the folder modelli_finali.
Within it, one could find the univariate model (response: math evaluation) and bivariate model (responde: math evaluation, read evaluation). Each subfolder contains a read.me file to describe its content. Moreover a pdf file presents all the implemented models.

Folder ANOVA contains the scripts to analyse the difference of evaluation on three different regions (Campania, Trentino Alto Adige and Lombardia, the ones which we had information about) and the results of the related hypothesis test.

Folder Clustering_GLMM contains the results of the clustering over the schools.

For a presentation of models and results, refer to the file ProjectPresentation.pdf
