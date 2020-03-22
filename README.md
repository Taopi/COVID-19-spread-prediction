# COVID-19-spread-prediction
This work presents the implementation of an online real-time Kalman filter algorithm to predict the spread of COVID19 in Switzerland. Coronavirus(COVID19 or SARS-CoV-2) has recently caused major worldwide concern. As the number of coronavirus cases reportedly increases, the spread of COVID19 is a serious threat to global health. In this work, I will try to predict the spread of coronavirus for each one of the infected Kantons. Fitting time series analysis and statistical algorithms to produce the best short term and long term prediction. 

The data has been collected from https://github.com/daenuprobst/covid19-cases-switzerland. covid19-cases-switzerland collects  data from twitter accounts and is the fasted data availuable. 

Predict algorithms of COVID-19 based on https://github.com/Rank23/COVID19. This work here adapts https://github.com/Rank23/COVID19, with three major changes:

*Thanks to the data supplied in covid19-cases-switzerland, predictions are made for actually the next days (no delay) 
*Thanks to the data supplied in covid19-cases-switzerland, predictions are made for Swiss Kantons 
*A prediction for three days in advance is made availuable
