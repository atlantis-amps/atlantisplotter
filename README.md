# atlantisplotter
###Atlantis plotter code modified for Puget Sound. This plotter code was originally developed by Pierre-Yves Hernvann @pyhernvann and Owen Liu @owenrliu. 

Package that allowes creating an RMD document that plots spatial abundance, weight-at-age, biomass, center of gravity, diet and predation for functional groups

To use:

1. Install package `remotes::install_github("https://github.com/atlantis-amps/atlantisplotter)`
2. Load the package library(atlantisplotter)
3. Open the RMD file plot_html_outputs.Rmd and modify the parameters in the first section to suit your needs
4. Save the Atlantis outputs you want to save into an "outputFolder" in the directory you are using
5. Knit the RMD with `knitr::knit("plot_html_outputs_faster.Rmd")`, it will create an HTML file
