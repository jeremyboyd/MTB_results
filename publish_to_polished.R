# Code to deploy an app to Polished
# App must be split into global, ui, and server files. App name is the name of
# the directory.
library(polished)
polished::deploy_app(
    app_name = "MTB_results",
    api_key = "v1GDJ310gbQGvLFOboOmpD3HPAP4FHtZS0"
)
