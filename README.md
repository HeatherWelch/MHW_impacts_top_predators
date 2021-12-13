# MHW_impacts_top_predators

Code to support the publication Welch, H. et al. "Idiosyncratic effects of marine heatwaves on top predators". 
Placeholder for zendo DOI. 
Code authors: Heather Welch (UCSC/NOAA). 

The purpose of this reposity is to archive the scripts used in Welch et al. 20xx.
1. *1_Process_environmental_data*. Contains scripts to process CMEMS netcdfs into rasters for extraction and prediction.  
2. *2_Process_species_data.* Contains scripts to process telemetry data for model fitting and novel data for model validation. Generates pseudo-absences and minimum bounding polygons.    
3. *3_Extract_environmental_data* Contains scripts to extract environmental data to telemetry and novel data.  
4. *4_Model_fitting.* Contains scripts to fit multi-variable and SST-only boosted regression tree models.  
5. *5_Model_validation.* Contains scripts to validate boosted regression tree models.  
6. *6_Model_prediction.* Contains scripts to predict boosted regression tree models onto environmental data.   
7. *7_MHW_impacts.* Contains scripts to calculate MHW impacts: core habitat, range, displacement, redistribution of core across EEZs.  
8. *Utilities.* Contains functions called by other scripts.  

Relevant papers:  
Hazen, E. L. et al. Predicted habitat shifts of Pacific top predators in a changing climate. Nat. Clim. Change 3, 234–238 (2013).  
Jacox, M. G., Alexander, M. A., Bograd, S. J. & Scott, J. D. Thermal displacement by marine heatwaves. Nature 584, 82–86 (2020).  
Block, B. A. et al. Tracking apex marine predator movements in a dynamic ocean. Nature 475, 86–90 (2011).  
Harrison, A.-L. et al. The political biogeography of migratory marine predators. Nat. Ecol. Evol. 2, 1571–1578 (2018).  
McHenry, J., Welch, H., Lester, S. E. & Saba, V. Projecting marine species range shifts from only temperature can mask climate vulnerability. Glob. Change Biol. 25, 4208–4221 (2019).  
