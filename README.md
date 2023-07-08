# LTS_Forecasting_Project_ENSAE_2022-2023

This project aim at predicting the prices of dairy product in France using linear time series models. We used data from INSEE (French Institute of statistics).
We were awarded 20/20 for this project.

You can find the data here [INSEE Data](https://www.insee.fr/fr/statistiques/3544641?sommaire=3530678)


## Setup

We used R 5.3. To run the code you will need the following packages : zoo, forecast, tseries, fUnitRoots .


## Authors

- [@Thomasaujoux](https://github.com/Thomasaujoux)
- [@elea-bordais](https://www.linkedin.com/in/elea-bordais-446798218/?originalSubdomain=fr)

## Directory structure

- File **main.R** :
  - *PARTIE 1* : 
  What does the chosen serie represent (sector, any processing, logarithmic transformation, etc.)?
  If necessary, transform the serie to make it stationary (diff'erentiation, elimination of the tendency to be tendency, etc.). Justify your choices carefully.
  Graphical representation of the selected range before and after transformation

  - *PARTIE 2* :
   Choose and justify an ARMA(p,q) model for your corrected serie Xt. Estimate the parameters of the model and check its validity.
   Express the ARIMA(p,d,q) model for the selected serie.

  - *PARTIE 3* :
   Write the equation given by the confidence region of level α on the future values (XT +1, XT +2).
   Specify the assumptions used to obtain this region.
   Graph this region for α = 95%.

- File **selection_modele.R** :
*adj_r2* 

- File **stationnarite.R** :
*Qtests* 
*adfTest_valid*

- File **test_causalité.R** :
*arma_causal*

- File **tests.R** :
*Qtests*

- File **validation_parametres.R** :
*signif*
*Qtests*
*modelchoice*
*armamodelchoice*