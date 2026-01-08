# agwise-fertilizer

The AgWise Fertilizer Recommendation Module generates site-specific nutrient and fertilizer recommendations for key crops and regions. The module provides users with three alternative modelling pathways, allowing flexibility depending on data availability, scale, and use case. Users may select any one of the three options to generate fertilizer recommendations.


AgWise fertilizer recommendation generation options
https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/ML

i-Machine learning

The machine learning framework generates user advisories using machine learning algorithms only. After spatial soil, topography, and climate data are obtained from the data sourcing module, 
all datasets are compiled and processed into a model-ready training dataset. We use the H2O AutoML framework, which automatically trains multiple algorithms (e.g., GLM, GBM, Random Forest, 
XGBoost, and deep learning models) and selects the best-performing model. H2O also provides a parallelized training environment, which is essential for efficiently handling large datasets.

<img width="219" height="277" alt="image" src="https://github.com/user-attachments/assets/694e5be7-55a3-466b-97fb-50c81d80ef1b" />

a) Model training

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/ML/ML_training

This R script is an end-to-end automated machine learning pipeline built on H2O AutoML for tabular regression problems. 
•	It reads data from common formats (CSV/Excel/RDS/RData), cleans column names.
•	It then trains an AutoML model , evaluates performance using RMSE/MAE/R², and performs algorithm-specific hyperparameter tuning via grid search. 
•	Next, it ranks features by variable importance, retrains models using top predictor subsets to find the better-performing model with reduced dimensionality.
•	Finally generates model diagnostics and interpretability outputs (observed vs predicted, variable importance, PDP/ICE, and SHAP), saving results generated from the best and tuned model.

b) Prediction

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Prediction/prepare_PredictionGrid.R

The trained model will then be used to predict yield on a new spatial desired area.  So, this script does the preparation of the prediction grid, stacking the spatial covariates within three climate scenarios;normal, 
above-normal and below-normal.

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Prediction/prediction.R

This script predicts on the spatial area using different nutrient range extracted from the trial data. The prediction will result in three huge datasets which are the spatial prediction of yield on normal weather scenario,
above-normal weather scenario and below normal scenario.

C) Optimization
The optimization involves selecting the appropriate fertilizer rates based on interest and target for optimization.

1. Maximum yield optimization

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_yield.R

This script performs **yield optimization** by identifying fertilizer application combinations that achieve **maximum crop yield** within a specified tolerance. For each scenario and location, it evaluates predicted yields across nitrogen–phosphorus (N–P) rate combinations, selects those within 2% (default) of the maximum observed yield, and chooses the lowest N and P rates among them. The function processes all scenarios in parallel structure, aggregates results across locations, and saves the final maximum-yield recommendations as an RDS file for downstream analysis and advisory generation.

2. Maximum profit optimization

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_profit.R

This function performs maximum profit optimization across all scenarios and locations. It calculates revenue, fertilizer cost, and profit using user-defined crop price and input costs. 
For each location, it selects the option with profit within a specified tolerance (default 2%) of the maximum profit, breaking ties by choosing the lowest N and P rates.

3. Marginal economic optimization

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_marginal.R

This script performs marginal economic optimization of fertilizer recommendations using the rule MR ≥ 1 (marginal return at least equals marginal cost). For each scenario (below, normal, above) and each location, it computes marginal revenue (DMR) and total cost (DTC) based on crop price and fertilizer costs. It then estimates the marginal ratio MR = ΔDMR / ΔDTC along increasing cost levels and selects the last rate where MR ≥ 1 (or the closest-to-1 option when none meet the threshold). 

4. Fertilizer Use efficiency

https://github.com/CGIAR-AgWise/agwise-fertilizer/blob/main/generic/ML/Optimization/optimize_efficiency.R

The script calculates nitrogen and phosphorous use efficiency and selects from among top efficiencies, minimum N and P and maximum yield for each location.

The main optimization script accepts and reads predicted data and runs optimization scripts of interest.

ii-Machine learning and QUEFTS

This approach integrates three complementary modeling frameworks; Machine Learning, Reverse-QUEFTS, and Forward-QUEFTS to derive spatially optimized, crop and site-specific fertilizer recommendations. It hinges on the idea that if an ML model can accurately predict how yield responds to "no-input" and "high-input" scenarios across a landscape, a mechanistic model can then "reverse-engineer" those responses to identify the specific nutrient supply of the soil. 

1. Machine Learning Generation of Yield Surfaces 

The pipeline begins by training an ensemble of ML models (e.g. Random Forest, Gradient Boosted Trees, or XGBoost) on multi-location trial data to generate two critical spatial layers (yield surfaces): Control Yield Surface (Yo): Predicting yield where N=P=K=0 and High-Input Yield Surface (Ymax): Predicting yield under non-limiting fertilizer conditions (e.g., high NPK levels). By generating these surfaces first, we create a spatially continuous dataset of "modelled behaviours." These surfaces bridge the gap between sparse experimental points and a continuous landscape, serving as the primary input for the mechanistic solver. 

2. Reverse-QUEFTS (The Inversion Logic) 

The core of this approach lies in reverse quefts model. Here, the pipeline performs a pixel-wise inversion. For every cell in the raster grid, the system asks: "What combination of Indigenous Nitrogen, Phosphorus, and Potassium (INS, IPS, IKS) would cause the QUEFTS model to produce the exact Y0 and Ymax predicted by the ML model?". 
The output of this stage consists of three key GeoTIFFs: INS_rev.tif, IPS_rev.tif, and IKS_rev.tif. These maps represent the "decoded fertility" of the soil, grounded in both the ML model's spatial intelligence and the QUEFTS model's chemical logic. 

3. Forward-QUEFTS and NUE Optimization
    
With the soil supply surfaces established, the system shifts to the Forward-QUEFTS engine to prescribe actual fertilizer rates. 
Dynamic Target Yield Selection: The "Attainable Yield" (Ytarget) is not a static number. It is dynamically constructed by taking the 90th percentile of the maximum yields observed in the regional dataset. This creates a realistic "ambition ceiling" that accounts for localized climatic constraints while remaining agronomically challenging. 

Iterative Nutrient Use Efficiency (NUE) Scoring:The optimizer does not simply aim for the highest possible yield, which often requires excessive fertilizer. Instead, it iterates through a grid of potential yield scenarios (from 60% to 110% of Ytarget) to find the "Sweet Spot" of efficiency. For each pixel, it calculates: 

Yield Gain (∆Y): Yfertilized - Y0. 

Agronomic Efficiency (AE): The ratio of yield gain to the amount of nutrient applied. 

The Weighted Score: The system selects the rate (N, P, K) that maximizes the following objective function: Score = Yield x (NUEα). The α parameter allows for strategic tuning: a higher α prioritizes sustainability and efficiency (saving fertilizer), while a lower α prioritizes total food production (maximizing yield). 

4. Computational Scalability and National Implementation 

Performing these optimizations for every pixel at a national scale (e.g., 250m resolution) involves millions of individual simulations. The pipeline is designed for high-performance execution. 
The final stage exports a suite of GeoTIFF layers ready for integration into digital advisory apps: 

- Recommended N, P, K: Optimized application rates per pixel. 
- Expected Yield: The predicted outcome of the recommendation. 
- Limiting Nutrient Map: Identifying whether N, P, or K is the primary bottleneck for that specific field. 

This "Hybrid Behavioral Inference" workflow represents the frontier of site-specific management. By using Machine Learning to capture the broad environmental patterns of yield and QUEFTS to resolve the underlying nutrient mechanics, the system provides recommendations that are both spatially precise and agronomically sound. It transforms "big data" into "smart advice," empowering digital advisory systems to deliver tailored solutions for every farmer. 

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



