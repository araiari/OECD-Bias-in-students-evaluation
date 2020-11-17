In questa cartella sono contenuti i dataset utilizzati nei vari script.

data_tidy.csv è un dataset in cui sono selezionate solo alcune delle covariate relative allo studente e alla scuola
a partire dal dataset enorme fornito dall'OCSE. Il criterio di selezione è l'intuito: abbiamo scelto quelle variabili
che ci paiono più interessanti per la nostra analisi. In data_tidy_NA.csv vengono eliminate tutte le righe con missing values.

dati_reg.csv contiene i dati utilizzati per fittare il modello lineare per gli studenti. 
Contiene le informazioni base degli studenti (sesso, età, ...), informazioni sulle loro abitudini e sul loro stato economico-sociale.
Sono state eliminate tutte le righe che contenevano almeno un missing value.

data_hierarchical_WAIC.csv è utilizzato per confrontare modelli a effetti misti che considerano le stesse covariate dello 
studente, ma diverse covariate relative alla scuola. 

dati_bivariato.csv contengono i dati usati per costruire i modelli lineari e gerarchici bivariati.
dati_bivariato_std.csv contiene gli stessi dati, ma standardizzati.

----------------------------------------------------------------------------
data_tidy.csv : in this dataset we selected only some features about the student and the school from the huge OCSE-Pisa dataset.
The selection criterion was intuition: we selected the features that seemed to be more interesting for our analysis.
data_tidy_senza_NA.csv : reduction of data_tidy.csv without the observations with at least one missing value.

dati_reg.csv : used to fit univariate linear models, contains all the basic information about the students (gender, age, ...), 
about their habits and their socio-economic status. We deleted all the observations with at least a missing value.

data_hierarchical_WAIC.csv : used to fit univariate hierarchical models, contains both the relevant student features and the school features.

dati_bivariato.csv : used in bivariate linear and hierarchical models. 
dati_bivariato_std.csv : standardized version of dati_bivariato.csv
