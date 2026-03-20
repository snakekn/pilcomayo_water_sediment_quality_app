#' This file isa helper file for uploading the current app to a website using shinylive
#' Instructions: 
#'              1) https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
#'              2) https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc
#' Note: This process can take awhile (~30min), as it's compiling the app then making it into an HTML format in the /docs folder
#'       You'll only need to do this when confirming things work as expected
#'       The app was setup on the Zenodo-connected repo, not the Jackson-R branch.


## Publish the app into the docs directory
library(shinylive)
shinylive::export(appdir = here::here(), destdir = "docs")

## View the app locally to confirm it works as expected
httpuv::runStaticServer("docs/", port = 8008)


#' Next Steps:
#' 1) Ensure you've set the pages feature on GitHub to load from the docs/ folder
#' 2) View the webpage. The release version is on https://snakekn.github.io/pilcomayo_water_sediment_quality_app
#' 3) Ensure the website is loaded without any Trinational data...
#' 4) Anything else you can think of! This is my first time using this process