
# geolocators_simplified

This is a simplified, yet rough extraction of migration tracks from geolocator data.

Based on [Light level geolocation analyses](https://geolocationmanual.vogelwarte.ch/).

Developed for [Matt Reudink](https://www.mattreudink.com/)

## Using this workflow

1. Download this repository 
    - clone it OR
    - click the green Code button above and download the zip file

2. Open as an RStudio project 
    - double click on the `geolocators_simplifed.Rproj` file OR
    - open in RStudio File > Open Project > Select `geolocators_simplifed.Rproj`
    
3. Make sure you have the right packages installed (active the `renv` project to automatically ensure this)
    - Run the function `renv::restore()` in the console.

4. Make sure you have your data in the `Raw` folder inside the `Data` folder.

5. Copy a template and rename it
    - `process_SGAT_TEMPLATE.qmd` -> `process_SGAT_BIRDID.qmd`
    - `process_flightr_TEMPLATE.qmd` -> `process_flightr_BIRDID.qmd`  **NOT COMPLETE**

6. Now open the *new* and **renamed** template file and follow the instruction/steps
    - Blockquotes (`>`) represent steps where you need to make a chance or decision
    - `:::{.callout-notes}` are sections where you should write down any notes related to or
      explaining the decisions you've made.
    - Run each code block as it comes
    
7. When you are happy with the analysis and results, compile the document into an HTML file
   to keep a reproducible record. 
    - Ctrl-Shift-K to render OR
    - Click on the "Render" button
    - The `process_SGAT_BIRDID.html` is now your reproducible record of this analysis.
