# agwise-fertilizer

The AgWise Fertilizer Recommendation Module generates site-specific nutrient and fertilizer recommendations for key crops and regions. The module provides users with three alternative modelling pathways, allowing flexibility depending on data availability, scale, and use case. Users may select any one of the three options to generate fertilizer recommendations.


AgWise fertilizer recommendation generation options
https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/ML

i-Machine learning

The machine learning framework generates user advisories using only machine learning algorithms. After spatial soil, topography, and climate data are obtained from the data sourcing module, 
all datasets are compiled and processed into a model-ready training dataset. We use the H2O AutoML framework, which automatically trains multiple algorithms (e.g., GLM, GBM, Random Forest, 
XGBoost, and deep learning models) and selects the best-performing model. H2O also provides a parallelized training environment, which is essential for efficiently handling large datasets.

<img width="219" height="277" alt="image" src="https://github.com/user-attachments/assets/694e5be7-55a3-466b-97fb-50c81d80ef1b" />

a) Model training

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/ML/ML_training

This R script is an end-to-end automated machine learning pipeline built on H2O AutoML for tabular regression problems. 
•	It reads data from common formats (CSV/Excel/RDS/RData), cleans column names.
•	It then trains an AutoML model , evaluates performance using RMSE/MAE/R², and performs algorithm-specific hyperparameter tuning via grid search. 
•	Next, it ranks features by variable importance, retrains models using top predictor subsets to find better-performing model with reduced dimesionality.
•	Finally generates model diagnostics and interpretability outputs (observed vs predicted, variable importance, PDP/ICE, and SHAP), saving results generated from the best and tuned model.

b) Prediction

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Prediction/prepare_PredictionGrid.R

The trained model will then be used to predict yieldon a new spatial desired area.  So, this script does the preparation of the prediction grid, stacking the spatial covariates withih three climate scenarios;normal, 
above-normal and below-normal.

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Prediction/prediction.R

This script predicts on the spatial area using different nutrient range extracted from the trial data. The prediction will result on three huge datasets which is the spatial prediction of yield on normal weather scenario,
above-normal weather scenario and below normal scenario.

C) Optimization
The optimization involves in selecting the appropriate fertilizer rates based on interest and target for optimization.

1. Maximum yield optimization

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_yield.R

This script performs **yield optimization** by identifying fertilizer application combinations that achieve **maximum crop yield** within a specified tolerance. For each scenario and location, it evaluates predicted yields across nitrogen–phosphorus (N–P) rate combinations, selects those within 2% (default) of the maximum observed yield, and chooses the lowest N and P rates among them. The function processes all scenarios in parallel structure, aggregates results across locations, and saves the final maximum-yield recommendations as an RDS file for downstream analysis and advisory generation.

2. Maximum profit optimization

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_profit.R

This function performs maximum profit optimization across all scenarios and locations It calculates revenue, fertilizer cost, and profit using user-defined crop price and input costs. 
For each location, it selects the option with profit within a specified tolerance (default 2%) of the maximum profit, breaking ties by choosing the lowest N and P rates.

3. Marginal economic optimization

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_marginal.R

This script performs marginal economic optimization of fertilizer recommendations using the rule MR ≥ 1 (marginal return at least equals marginal cost). For each scenario (below, normal, above) and each location, it computes marginal revenue (DMR) and total cost (DTC) based on crop price and fertilizer costs. It then estimates the marginal ratio MR = ΔDMR / ΔDTC along increasing cost levels and selects the last rate where MR ≥ 1 (or the closest-to-1 option when none meet the threshold). 

4. Fertilizer Use efficiency

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_efficiency.R

The script calculates nitrogen and phosphorous use efficiency and selects from among top efficiencies, minimum N and P and maximum yield for each location.

The main optimization script accepts and reads predicted data and runs optimization scripts of interest.

ii-Machine learning and QUEFTS

This option integrates three complementary modelling components (Reverse-QUEFTS, Machine Learning, and Forward-QUEFTS), to generate spatially optimized, crop- and site-specific fertilizer recommendations.
The workflow consists of the following steps:
1.	Reverse-QUEFTS Computation: Soil nutrient supply indices for nitrogen, phosphorus, and potassium (INS, IPS, IKS) are derived for experimental sites using observed yield and nutrient input data.
2.	Machine learning training: ML models are trained to predict nutrient supply indices (INS, IPS, IKS) using environmental and soil covariates.
3.	Spatial prediction: The trained models are applied to spatial grids to generate continuous maps of nutrient supply indices across the landscape.
4.	Forward-QUEFTS simulation: The QUEFTS forward model computes nutrient requirements for each spatial unit to achieve a range of attainable yield levels.
5.	Optimization: The model iterates across yield targets to identify the yield–nutrient combination that maximizes agronomic efficiency, yield target or nutrient use efficiency (NUE).

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/QUEFT_ML


iii-Crop model

Process-based crop models to generate site specific fertilizer recommendations. The option is particularly useful when there is limited or no crop fertilizer response data. A spatialized crop model (APSIM, DSSAT), is utilized to undertake spatial simulations. A series of nutrient (N, P and K) rates, range of different planting dates and generic varieties, are used as inputs. A response curve is determined for each generic variety and planting time for each pixel to determine the fertilizer recommendations. 

This option uses process-based crop models to generate site-specific fertilizer recommendations and is particularly useful in contexts where fertilizer response trial data are limited or unavailable.
Spatialized crop models such as APSIM or DSSAT are used to conduct simulations across space. Model inputs include:
•	Multiple N, P, and K application rates,
•	A range of planting dates, and
•	Generic crop varieties representative of target environments.
For each spatial unit, the model simulates crop growth and yield under different management scenarios. Yield response curves are generated for each planting date and variety, and these curves are used to determine optimal fertilizer recommendations based on simulated yield responses.

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/useCases/Scripts/UseCase_Example_CropModelApproach/Maize/DSSAT



