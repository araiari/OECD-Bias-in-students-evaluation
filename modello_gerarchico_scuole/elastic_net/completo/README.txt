Questa cartella contiene l'elastic net considerando tutte le covariate delle studente fissate allo step 1
e vuole fare selezione sulle migliori covariate della scuola.

I tre modelli differiscono per il tipo di prior utilizzata nello "scale parameter".
E' stata usata prima un'uniforme (0,100), poi una normale troncata e infine una inv-gamma(0.001,0.001).

Queste prior sono state scelte seguendo il suggerimento dell'articolo Gelman 2006
risultato_completo_unif continee il risultato considerando una prior uniforme per il parametri di "scale"

risultato_completo_0.001 continee il risultato considerando una prior inv-gamma (0.001,0.001 )per il parametri di "scale"

risultato_completo_norm continee il risultato considerando una prior normale troncata per il parametri di "scale"
