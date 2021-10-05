# Author: Jeremy Boyd (jeremyboyd@pm.me)
# Description: Code to deploy an app to Polished.

# NOTE: App must be split into global, ui, and server files. App name is the
# name of the directory.
# NOTE: Comment out the source() line in .Rprofile, move renv folder out of the
# directory, and restart R. Otherwise the call to deploy_app() will fail.
library(polished)
polished::deploy_app(
    app_name = "MTB_results",
    api_key = "v1GDJ310gbQGvLFOboOmpD3HPAP4FHtZS0"
)
