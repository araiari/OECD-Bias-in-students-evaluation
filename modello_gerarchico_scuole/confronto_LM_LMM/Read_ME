# Il file confronto_LM_LMM.R serve a verificare l'effettiva miglioria di predizione apportata dal modello a effetti misti 
# rispetto al modello lineare. 

# E' utilizzato il dataset di partenza data_tidy.csv che contiene solo le variabili relative allo studente e il gruppo scuola. 
# Il dataset è modificato all'inizio dello script confronto_LM_LMM.R per renderlo adatto allo scopo dell'analisi (rimozione di NA,
# modifica del tipo dei dati, creazione di variabili dummies, ...).
# I risultati di tale modifica sono contenuti nel workspace dati_confronto_LM_LMM.RData

# Per la simulazione dei modelli è stato utilizzato JAGS. 
# I tre files simple_LMM.bug, simple_LMM2.bug e regressione_new_jags.bug specificano i modelli utilizzati

# Il file simple_LMM.bug serve per la simulazione del modello gerarchico Y_ij =  theta_j + gamma_ij * X_ij + err_ij 
# con i = studente nella j = scuola.

# Il file simple_LMM2.bug serve per la simulazione del modello gerarchico Y_ij =  theta0 + theta_j + gamma_ij * X_ij + err_ij 
# con i = studente nella j = scuola.

# Il file regressione_new_jags.bug serve per la simulazione del modello lineare Y_i = beta * X_i + eps_i con i = studente.

# In WAIC_confronto_LM_LMM.dat è contenuto come risultato dell'analisi l'indice WAIC dei tre modelli. Il modello LMM2 risulta 
# significativamente migliore degli altri due, provando che l'approccio con effetti misti è migliore rispetto a quello semplice.
