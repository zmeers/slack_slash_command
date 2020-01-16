## Deploy APIs on Digital Ocean
## Zoe Meers

# Main steps:
#
# 
# 1.Create a DigitalOcean account.
# 2.Setup an SSH key and deploy the public portion to DigitalOcean so you’ll be able to login to your server.
# 3. Install the analogsea R package and run a test command like analogsea::droplets() to confirm that it’s able to connect to your DigitalOcean account. Install SSH and install plumber (all dev versions)!
# 4. Run mydrop <- plumber::do_provision(). This will start a virtual machine (or “droplet”, as DigitalOcean calls them) and install Plumber and all the necessary prerequisite software. Once the provisioning is complete, you should be able to access port 8000 on your server’s IP and see a response from Plumber.
# 5. Install any R packages on the server that your API requires using analogsea::install_r_package().
# 6. You can use plumber::do_deploy_api() to deploy or update your own custom APIs to a particular port on your server.
# (Optional) Setup a domain name for your Plumber server so you can use www.myplumberserver.com instead of the server’s IP address.
# (Optional) Configure SSL


devtools::install_github("trestletech/plumber")
remotes::install_github("sckott/analogsea")
install.packages("ssh")

library(plumber)
library(analogsea)
library(ssh)

# primary pollls API
mydrop <- plumber::do_provision(example = FALSE)

droplet_name <- mydrop[['name']]

# In terminal run the following:
# sudo apt-get update
# sudo apt-get install -y libssl-dev
# sudo apt-get install libcurl4-openssl-dev
# sudo apt-get install libxml2-dev


# install packages needed for API to work
analogsea::install_r_package(package = c("tidyverse", "lubridate",
                                         "glue", "config", "httr"), 
                             droplet = droplet_name)


plumber::do_deploy_api(droplet = droplet_name,
                       path = "plumber",
                       localPath = here::here("R/slack_bots"),
                       # preflight = paste0(Sys.getenv('SLACK_SIGNING_SECRET')),
                       port = 8002)
 
 plumber::do_remove_api(droplet = droplet_name, 
                        path = "plumber", 
                        delete = TRUE)

 # DO NOT RUN unless completely busted
 # analogsea::droplet_delete(droplet = droplet_name)
 