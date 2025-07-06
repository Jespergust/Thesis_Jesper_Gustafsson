# Thesis_Jesper_Gustafsson_580155

READme

Replication Variance Risk Premium.

The following steps provide a clear explanation on how the Data and Results are obtained. To follow these steps one should install RStudio. In brackets are the specifications of the tables in the paper.

1. To extract the Data used to replicate the paper by Bollerslev et al. (2009) Summary_stats_VRP.R should be run which also returns the table with the summary statistics of the research (Table 1). For extended data Summary_stats_VRP_extended.R should be run (Table 10). Both also create excel files which are used for the regression analyses.

2. Fig_1.R provides the plots of the VRP, IV and RV (Fig. 1). 

3. Table_2.R returns the results of the horizon regressions used in Table 2, where regression_data.xlsx is used for the original sample, and regression_data_recent.xlsx for the extended sample, both obtained in step 1.

4. Directly thereafter one should run Fig_3.R, which uses the data still in the global environment to plot Fig. 3

5. Monthly_quarterly_yearly_regressions.R then runs the simple and multiple regressions used in Table 3,4 and 15 using full_data.xlsx obtained in Step 1. The extended data is used for the results using recent data in Table 16,17 and 18.

We then shift to the Extension research on variable selection and macroeconomic factor analysis. We first discuss the full process of data collection and estimation, and thereafter provide how to obtain the tables and figures in the paper.

1. To obtain the data of financial variables one should run FinVar_data.R which creates an excel file containing data of the financial variables.

2. For the selection of variables Variable_selection.R should entirely be run which first runs the different variable selection models, and then also uses the selected variables in the re-selecting every 5 years approach.

3. The macroeconomic variable data can be obtained using MacVar_data.R 

4. All the different correlation matrices used in Fig. 2, 9 and 10 and Table 13 can be obtained from Corr_data.R 

5. The different macroeconomic thresholding procedures, estimations and forecasts can then be obtained from Hard_Thresh.R for the hard thresholding with both 1.04 and 1.28 thresholds. Soft_Thresh.R uses the soft thresholding procedure.

6. To then obtain the results for the statistics used in Tables and Figures one should first run Stat_tests_and_plots.R which first creates matrices with the results shown tin Table 6,7 and 8. And which then also plots the Hit Ratios of Fig. 7 and the excess returns of Fig. 8.

7. To obtain the different figures which dive deeper into the variables selected and macroeconomic factors, one should run Deep_dive.R which provides Fig. 4,5 and 6 and Tables 19 and 20.

8. Then the ececonomic value model results (Table 9 and 21) can be obtained from Economic_value.R

Reference:
Bollerslev, T., Tauchen, G. & Zhou, H. (2009). Expected stock returns and variance risk premia.
The Review of Financial Studies, 22 (11), 4463–4492. Retrieved from
https://academic.oup.com/rfs/article-abstract/22/11/4463/1565787 doi:
10.1093/rfs/hhp008
