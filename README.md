# PhenoFlex application for Golden Delicious data at Laimburg
This project applies the PhenoFlex model to phenological data of Golden Delicious together with temperature data at the location Laimburg, Italy. It has been made for a Bachelor thesis and uses the PhenoFlex model proposed by Eike Ludeling et.al in 2021. 

The aim is not only the application of the Model for past years, but the forecast of reliable temperature data for the current year to actually predict future bloom dates.Moreover, by using the latest knowledge about the phenology of fruit trees, the phenological states between bud burst and beginning of harvest are predicted by using the Growing Degree Hours(GDH).

The publicly available temperature and phenological data of the Research Center Laimburg can be retrieved by an API and the forecast can therefore be updated automatically. The provided Code can be used to extend and adapt for different locations as well as variaties. 

## Description
The repository contains a workflow to update the database temperature data with the latest measurements with an API. Moreover, the weather forecast with three different approach is available. And the bloom prediction based on this temperature prediction. 
Two diagramms are available, containing general information about the used data and the temperature profile of the current year in comparison to the yearly average.
Additionally, the prediction of all phenological states of the Golden Delicious can be found in this workflow. 

Additionally information how the underlying database structure can be processed to use it for model training and data interpolation is provided. 

1. Website: Use the forecast of the website [websiteLinkEinfügen] to retrieve the latest prediction of the Golden Delicious apple in Laimburg.
2. Download: Copy the repository and run it locally. It can be extended, modified and adapated to different varieties and locations. 

## Getting Started

### Dependencies
I've run the project on the 
- R version 4.5.2 by using a 
- Windows 11 operating system
- SQLight 
- Shiny
- Connected Cloud

The following libraries and packages are used inside the project:
- * [awesome-readme](https://github.com/matiassingers/awesome-readme)

### Installing
Download the complete repository to run the shiny app locally. Therefore, R needs to be installed. A suitable database has to be called for the project to work. Look at the following picture to understand the required database structure. 

### Executing program

* How to run the program
* Step-by-step bullets
```
code blocks for commands
```

## Help
So far, no common issues have been discovered. In case you run into issues contact me via mail: johannakuemmerer7@gmail.com


## Authors
Johanna Kümmerer, as part of the Bachelor thesis at the Free University of Bolzano

## Version History
* 0.1
    * Initial Release

## License

This project is licensed under the [GPL-3] License - see the LICENSE.md file for details

## Acknowledgments
Inspiration, code snippets, etc.
* [Fitting_Procedure](https://github.com/EduardoFernandezC/phenoflex_exp_data/tree/main/code)
* [Usage_PhenoFlex](https://hortibonn.github.io/Tree-Phenology/index.html)
* [Meteo_Laimburg](https://meteo.laimburg.it/)
* [API_weather](https://data.civis.bz.it/de/dataset/misure-meteo-e-idrografiche)
* [API_localForecast](https://data.civis.bz.it/de/dataset/southtyrolean-weatherservice-weatherdistricts)