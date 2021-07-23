##########################################################

Lukas Beinhauer --- 30/08/2020
Research Assistant - lukas.beinhauer@gmail.com | +49 157 71193431

##########################################################

This sub-directory contains the scripts used to analyse the data from the RADAR study using SEM-forests. with the current
work of Caspar van Lissa, concerning emotional dysregulation in adolescents.

In order to analyse the data, please go through the following steps:

1. Save the data in this sub-directory, where also the <RADAR.Rproj> file is saved.
2. Should the data NOT be in .csv format yet, make sure to save it in .csv (e.g. in SPSS, use export as).
3. Use the <missRanger.R> script to process some of the data (rounding values, reformat NAs) and impute the missing values
	using the missForest algorithm. The results are saved in this sub-directory.
4. Use the <creatingscales.R> script to create scales from the raw scores. Individual average scores per scale/subscale will
	be computed, the resulting file will be saved in this sub-directory.
5. Use the <plantingforest.R> script to create the latent growth curve model and grow SEM-forests. The results will be saved in
	this sub-directory. WARNING: time-intensive; tune model
6. Use the <assessingforest.R> script to calculate variable importance, results are saved in this sub-directory. WARNING: time-
	intensive.
7. Use the <partDependence.R> script to compute partial dependence of the top 5 and bottom 5 most relevant variables. WARNING:
	time-intense - results are saved in this sub-directory.
8. Use the <ForestVIM.Rmd> and <DependencePlots.Rmd> files to screen and visualize the results. The first document creates 
	default plots of variable importance, as defined by the semtree package. The second document creates partial dependence
	plots for the top 5 and bottom 5 most relevant variables.

NOTE: packages are called by <library(semtree)> or similar. If a respective library has not been installed yet, use 
	<install.packages(semtree)> beforehand, or alternatively <require(semtree)>. (It is sometimes discouraged to use require,
	as it doesn't throw an error unlike library, if a package is unknown. Less relevant if not used within function)

Feel free to adjust any of the scripts as necessary.