# LTPowerPriceSim
R project for long term simulation of power prices
Created by Nadia Daneva
Dec 2020


GENERAL

For detailed description of the model, results and technical details please refer to
the document Long_term_power_price_simulation.html

For the naming convention used in the code please refer to R Naming Convention.pdf

The repository is designed to be a portable R script. For this reason R portable is 
included in the repo. 


EXECUTION

unfortunately R is difficult to wrap in a portable stable solution.
In our case execution is possible on Windows with the available batch script:
startRScript.bat

The script will perform several things:
1. ATTENTION: the script will first try to download and install RTools into C:\Rtools. This is necessary
because otherwise some of the binary packages may not be able to install ('make' will not be avilable to R)

2. start the RScript.exe available in R portable. During first execution
renv package will install all needed libraries for the simulation. Then the simulation will be
performed by the wrapper StartPriceSimScript.R
The results will be saved under ./Output

If you run into problems with R it is best to download and extract R portable again
from https://sourceforge.net/projects/rportable/files/R-Portable/4.2.0/R-Portable_4.2.0.paf.exe/

If you would like to change some of the parameters of the simulation which are defined in the wrapper
you can edit StartPriceSimScript.R with a simple text editor and then execute the .bat script.


CODE EDIT

If you would like to test and edit the R files it is best to install R Studio and point it to the 
R portable version of R. 

A description how to achieve this is available under:
https://rpubs.com/jsmccid/rportable


