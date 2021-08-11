# E-System
Ensembling System for forecasting  covid 19 infection casses <br />
 we developed the first version of the "Epidemic.TA" system that included the neural network model NNAR, and time series models BATS, TBATS, Holt's Linear trend, and the ARIMA model. The results of "Epidemic.TA" are very accurate for forecasting cummulattive infection cases,and we excluded the SIR model from this system. The MAPE\% error is less than 1\%, hence for daily infection cases, between 1.638\% and 1.638\%. In this paper, we developed the "Epidemic.TA" system to improve forecasting and reduce the error of MAPE\% for daily infection cases. The new developed system is called "Ensembling-system".In this system, we added a cubic smoothing splines model. On the other hand, to improve the forecast, we used two Ensembling methods. The first Ensembling method is the aggregation (average) results from time series and neural network models (NNAR, BATS, TBATS, Holt's linear trend, ARIMA, and cubic smoothing splines) models. The second Ensembling method is Ensembling by using average weight by using a prioritizer. It gives weights to time series models that were previously mentioned, and then gets the Ensembling model's "average weight. and compares the errors between these two systems <br />
# Makarovskikh Tatyana Anatolyevna “Макаровских Татьяна Анатольевна”<br />
Abotaleb mostafa“Аботалеб Мостафа”<br />
Faculty of Electrical Engineering and Computer Science<br />
Department of system programming<br />
South ural state university, Chelyabinsk, Russian federation <br />
The work was supported by the Ministry of Science and Higher Education of the Russian Federation (government order FENU-2020-0022).
