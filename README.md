# agwise-fertilizer

The AgWise Fertilizer Recommendation Module generates site-specific nutrient and fertilizer recommendations for key crops and regions. The module provides users with three alternative modelling pathways, allowing flexibility depending on data availability, scale, and use case. Users may select any one of the three options to generate fertilizer recommendations.


AgWise fertilizer recommendation generation options
i-Machine learning,
Machine learning models are trained using crop fertilizer response datasets and integrated with geospatial data layers to capture spatial variability. The trained models are then used to:
•	Predict yield responses across nutrient application rates,
•	Identify optimal fertilizer rates that maximize agronomic efficiency, and
•	Support recommendations aligned with user-defined yield targets.

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/ML


ii-Machine learning and QUEFTS, 

This option integrates three complementary modelling components (Reverse-QUEFTS, Machine Learning, and Forward-QUEFTS), to generate spatially optimized, crop- and site-specific fertilizer recommendations.
The workflow consists of the following steps:
1.	Reverse-QUEFTS Computation: Soil nutrient supply indices for nitrogen, phosphorus, and potassium (INS, IPS, IKS) are derived for experimental sites using observed yield and nutrient input data.
2.	Machine learning training: ML models are trained to predict nutrient supply indices (INS, IPS, IKS) using environmental and soil covariates.
3.	Spatial prediction: The trained models are applied to spatial grids to generate continuous maps of nutrient supply indices across the landscape.
4.	Forward-QUEFTS simulation: The QUEFTS forward model computes nutrient requirements for each spatial unit to achieve a range of attainable yield levels.
5.	Optimization: The model iterates across yield targets to identify the yield–nutrient combination that maximizes agronomic efficiency, yield target or nutrient use efficiency (NUE).

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/generic/QUEFT_ML


iii-Crop model.
Process-based crop models to generate site specific fertilizer recommendations. The option is particularly useful when there is limited or no crop fertilizer response data. A spatialized crop model (APSIM, DSSAT), is utilized to undertake spatial simulations. A series of nutrient (N, P and K) rates, range of different planting dates and generic varieties, are used as inputs. A response curve is determined for each generic variety and planting time for each pixel to determine the fertilizer recommendations. 

This option uses process-based crop models to generate site-specific fertilizer recommendations and is particularly useful in contexts where fertilizer response trial data are limited or unavailable.
Spatialized crop models such as APSIM or DSSAT are used to conduct simulations across space. Model inputs include:
•	Multiple N, P, and K application rates,
•	A range of planting dates, and
•	Generic crop varieties representative of target environments.
For each spatial unit, the model simulates crop growth and yield under different management scenarios. Yield response curves are generated for each planting date and variety, and these curves are used to determine optimal fertilizer recommendations based on simulated yield responses.

https://github.com/CGIAR-AgWise/agwise-fertilizer/tree/main/useCases/Scripts/UseCase_Example_CropModelApproach/Maize/DSSAT



