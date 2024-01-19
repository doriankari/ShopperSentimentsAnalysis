# Rshiny App

## ShopperSentimentsAnalysis



*This application offers an in-depth exploration of crime data in Los Angeles,
  providing a comprehensive analysis of trends, variations, and criminal 
  patterns prevalent within this metropolis.
  Its aim is to deliver a nuanced and contextualized understanding of 
  the city's criminal landscape through interactive visualizations and 
  data analysis tools.*

*View of application*

![DASHBOARD](https://github.com/doriankari/ShopperSentimentsAnalysis/assets/146330254/5518a955-204f-4fc6-baa5-5585b02e0ffe)


## Getting Started

### Prerequisites

- R (version 3.6.0 or higher)
- RStudio
- Shiny and R packages

### Installation

1. Clone the repo:
   ```sh
   git clone [(https://github.com/doriankari/ShopperSentimentsAnalysis.git)]
   ```
2. Download the dataset using this link [TeePublic](https://www.kaggle.com/datasets/nelgiriyewithana/shoppersentiments?resource=download)

3. Download the repo, store the files in a folder and then create subfolders "www" in which you can put photos, "data" which will contain your data.
   
4. Open Packages.R : 
check that you have the same packages as in packages.R if not below the command :
```R
install.packages('package')
```
Then don't forget to load it :
```R
library(package)
```
5. Run the application locally by opening the `ShopperSentimentsAnalysis.R` file in RStudio and clicking 'Run App'.

6. Load your data in CSV format with button in the app

## Application Structure

- `ShopperSentimentsAnalysis.R`: Defines the user and server interface
- `global.R`: Usually contains the variables, but here it's empty because datas are loaded by the user
- `Packages.R`: Includes all packages for build an application
- `wwww` : Contains pictures

## Instructions 

Navigate through the tabs to access different analyses.
you can download in pdf by clicking on the button.

## Contact

- [My Linkedin](https://www.linkedin.com/in/dorian-amri-8685a2177/)
- [Email](amri.dk@hotmail.com)
