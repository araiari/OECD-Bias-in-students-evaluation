In questa cartella sono contenuti gli script per il modello lineare.

La risposta del modello è il voto in matematica degli studenti,
mentre le covariate disponibili riguardano alcune informazioni di base, la loro condizione socio-economica, le loro abitudini e i loro interessi.

Abbiamo selezionato le covariate più significative utilizzando i metodi di SSVS e di Elastic Net.
Si è scelto di utilizzare il criterio di Hard Shrinkage controllando se 0 stia nei credible intervals dei coefficienti.
Tutti i metodi sono stati implementati tramite MCMC con JAGS.
Poichè selezione effettuata da SSVS e EN era troppo poco selettiva sono stati costruiti diversi modelli (cartella "modello")
e sono stati confrontati tramite crossvalidation.

Il file SSVS_Variable_Selection.R è lo script per implementare il metodo SSVS. 
Sono utilizzati diversi iperparametri e i diversi risultati sono confrontati alla fine dello script.

Nel file Confronto_LM_con_WAIC.R sono confrontati i diversi LM con le covariate più significative e più o meno covariate dubbie. 
E' calcolato l'indice WAIC dei LM ed è scelto il modello con il maggior WAIC.

E' inoltre aggiunta un'ulteriore cartella, Elastic_net. Contiene gli script che implementano il metodo di regressione penalizzata EN.


-------------------------------------------------------------------------
The folder contains the code regarding linear models.

The model responde is the students' evaluation in Maths,
while the covariates concern basic information, socio-economic status, habits and interest.

We performed a Stocastic Search Variable Selection (SSVS) and an Elastic net to select the covariates.
Hard Shrinkage criterion was used to discover whether 0 was in the credible intervals of the coeddicients.
All methods are implemented with MCMC in JAGS.
Since the selection was too permissive, saving too many covariates, we built and compared different models with CV. Folder "modello" contains them.

File SSVS_Variable_Selection.R performs the SSVS method.
Different hyperparameters were tried to be used and the different results are compared at the end of the script.

File Confronto_LM_con_WAIC.R compared different LM obtained by choosing different covariates (all, only the most relevant, mixed cases).
We finally chose the model with the highest WAIC index.

Folder Elastic_net contains the scripts to implement the EN penalized regression.



