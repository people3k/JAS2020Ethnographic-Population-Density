# JAS2020Ethnographic-Population-Density

This supporting document should allow one to recreate the analysis performed as part of ``The Global Ecology of Human Population Density and Interpreting Changes in Paleo-Population Density." To replicate the analysis, one can either analyze the data files provided or build their own data set. As discussed in the main body of the text, we built three data sets following the procedures outlined by Tallavaara et al. (2017) for linking species richness values, net primary productivity and pathogen load to each ethnographic case. We do not replicate the scripts provided by Tallavaara et al.(2017) as these are available, clear and should be cited when used. To replicate our analysis, one needs to set their working directory in R to the file location that contains the data files. The data file is: “AllSocieties.csv”. The variables are defined as follows:

    Group/Country–name of the ethnographic society of country
    Latitude–the latitude at the geographic center of a group’s territory or a country’s territory.
    Longitude–the longitude at the geographic center of a group’s territory or a country’s territory.
    DENSITY–Population density in people per square kilometer. This is a point in time estimate for hunter-gatherer and     agricultural groups and an average density since 1973 among nation states.
    npp–net primary productivity estimated at the center of each group’s territory
    nppZ-Net primary productivity z score
    biodiv–Standardized estimate of species richness at the center of each group’s range.
    biodivZ–Species richness z score
    pathos–Index of pathogen stress at the center of a group’s territory.
    pathosZ-pathogen index z score
    ID–A nominal variable that denotes economy type. HG=hunter-gatherer, AG=subsistence agriculturalist, IND=modern nation state

Tallavaara, M., J. T. Eronen, and M. Luoto2017. Supporting data and script for ”productivity, biodiversity, and pathogens influence the global hunter-gatherer population density” (Tallavaara et al. pnas 2018).https://doi.org/10.5281/zenodo.1167852
