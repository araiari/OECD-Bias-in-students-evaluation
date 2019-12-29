Questa cartella contiene le elastic net del modello gerarchico.
Vogliamo selezionare le variabili relative alla scuola tenendo fissate quelle dello studente in precedenza selezionate.

Sono state fissate le covariate relative agli studenti. Come prova parziale se ne considerano solo 7
I modelli usati si trovano nei file .bug
Le differenze sono determinate da quale prior mettere sullo "scale parameter".

Prima è stata provata una inverse-gamma per ogni componente (quindi a priori niente correlazione) con iperparametri prima (2,10) e poi (0.001,0.001).
Poi si è provato con unif(0,100) e N(0,100^2)troncata. In tutti i casi quindi non si assume correlazione a priori.
Questi sono tutti suggerimanti dell'articolo Gelman 2006

Di seguito i file dei risultati e il dettaglio su quale distribuzione è stata usata, quante iterazioni, quali parametri sono stati salvati e quale file jags è stato usato
risultato_EN_0.001_0.001 è stato fatto con la invgamma(0.001,0.001) 20k+40k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_0.001_0.001

risultato_EN_unif è stato fatto con la unif(0,100) 20k+40k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_unif

risultato_EN_unif_big_iter è stato fatto con la unif(0,100) 60k+80k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_unif

risultato_EN_norm è stato fatto con la norm(0,10000) troncata 20k+40k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_troncated_normal

risultato_EN_norm_big_iter è stato fatto con la norm(0,10000) troncata 60k+80k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_troncated_normal

risultato_EN_0.001_big_iter è stato fatto con la invgamma(0.001,0.001) 60k+80k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_0.001_0.001

risultato_EN_unif_big_iter_meno_cov è stato fatto con la unif(0,100) 60k+80k iterazioni. param <- c( "theta",'a1','a2','prec_gamma'). file bug=elastic_net_hierarchical_ari_plus_no_inter_unif. Sono state tolte in partenza le due covariate che gli altri modello escludevano
