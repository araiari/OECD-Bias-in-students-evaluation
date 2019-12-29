# READ ME

# In questa cartella sono contenuti i dataset utilizzati nei vari script.

# data_tidy.csv è un dataset in cui sono selezionate solo alcune delle covariate relative allo studente e alla scuola
# a partire dal dataset enorme fornito dall'OCSE. Il criterio di selezione è l'intuito: abbiamo scelto quelle variabili
# che ci paiono più interessanti per la nostra analisi. In data_tidy_NA.csv vengono eliminate tutte le righe con missing values.

# dati_reg.csv contiene i dati utilizzati per fittare il modello lineare per gli studenti. 
# Contiene le informazioni base degli studenti (sesso, età, ...), informazioni sulle loro abitudini e sul loro stato economico-sociale.
# Sono state eliminate tutte le righe che contenevano almeno un missing value.

# data_hierarchical_WAIC.csv è utilizzato per confrontare modelli a effetti misti che considerano le stesse covariate dello 
# studente, ma diverse covariate relative alla scuola. 
