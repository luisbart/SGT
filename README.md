# SGT
Repository with R scripts for master thesis: Snow spatial distribution from terrain features

## Abstract
Snow is an important element in the earth system, and especially in the water cycle. Therefore, hydrological models are dependent to accurately represent snow. Prediction of hydrological behavior in ungauged basins remains one of the main challenges for these models. Traditional models with many calibrated parameters are not well-suited in those conditions. With this purpose, spatial variability of precipitation has been used earlier to constrain the spatial snow water equivalent (SWE) distribution as a two-parameter gamma distribution (Snow Gamma – Rainfall, SG_R), i.e. without adding more parameters to be calibrated. But this method cannot be applied where spatial variability of precipitation is unknown. Terrain parameters emerge as the convenient pathway for parameterizing the spatial variability of SWE, since detailed Digital Terrain Models (DTM) are much more common to be found nationwide. In this study, the parameterization of the spatial snow water equivalent (SWE) distribution is implemented solely from terrain information (Snow Gamma – Terrain, SG_T), testing two spatial resolutions for the DTM (1×1 and 10×10-meter). The two models are implemented in the parsimonious rainfall-runoff model Distance Distribution Dynamics (DDD). Then they are compared for 18 catchments in central and southern Norway. The results regarding streamflow have very similar accuracy when comparing to SG_R, in terms of Kling-Gupta Efficiency criteria (KGE) (average KGE is 0.83 for SG_R and 0.82 for SG_T). SCA estimations are slightly better for SG_R than SG_T when compared to MODIS observations. Based on this findings, the new method SG_T for quantifying the parameters of the spatial SWE gamma distribution does not degrade the performance of the model.

## Description
In the template folder there is an example of how to use this package: main.R. 

Function SGR_par calculates the terrain features and assign gamma parameter a0 and decorrelation length D to each elevation zone. Beforehand, gridded DTM (1- or 10m resolution) should be downloaded and located in one folder. DTM can be downloaded for Norway in https://hoydedata.no/LaserInnsyn2/. Landscape cover AR50 should also be downloaded for the whole country from https://kart8.nibio.no/nedlasting/dashboard. In template folder, ar50.R selects the classes that will be needed, merge them and export them as an object: ar50f for forest/wetland areas, and ar50w for water bodies. Them those can be imported as an object.

Function stat_DDD takes the results from DDD model using SG_R and SG_T and makes a comparison between both models and observations (Q, SWE and SCA)


## Installation

```R
devtools::install_git("https://github.com/luisbart/SGT")
```
