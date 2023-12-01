**Parte 1**
Ejecutar en paralelo los siguientes script's: 
- **fi923_1_lightgbm_binaria_BO_comp3.r**: sea paciente con los resultados (el script completo tarda ~ 2 dias con 8vCPU), NO hace undersampling. La iteracion 42 arroja los mejores hiperparametros.
- **fi923_2_lightgbm_binaria_BO_comp3.r**: por tener menor cantidad de features y hacer undersampling de 0.3, termina antes que el primer script. La iteracion 29 arroja los mejores hiperparametros.
- **fi923_3_lightgbm_binaria_BO_comp3.r**: es el mismo script que el segundo solo cambiando el motor a Lightgbm, en vez de utilizar gbdt utiliza goss. Para mi sorpresa, la segunda itaracion de la Optimizacion Bayesiana arrojo el mejor resultado.

**Parte 2**
Ejecutar en paralelo los siguientes script's:

- **fi924_1_lightgbm_final_semillerio**: Este script arroja cada 10 semillas un archivo .txt con la probabilidad acumulada. Tomar el resultado generado por el archivo: *prediccion_acum_140.txt* el cual contiene la probabilidad acumulada de 140 semillas.
- **fi924_2_lightgbm_final_semillerio**: idem fi924_1, tomar *prediccion_acum_100.txt*
- **fi924_3_lightgbm_final_semillerio**: idem fi924_1, tomar *prediccion_acum_30.txt*

**Parte 3**
Ejecutar el ensamble final **KA3000_ensamble_final.r**: Este script suma las probabilidades de baja de cada cliente de cada modelo. Elegir el el resultado *ensamble_ka41_ka42_ka43_100_30_11000.csv* el cual contiene la probabilidad acumulada de 140 semillas del modelo fi924_1, la probabilidad acumulada de 100 semillas del modelo fi924_2 y la probabilidad acumulada de 30 semillas.