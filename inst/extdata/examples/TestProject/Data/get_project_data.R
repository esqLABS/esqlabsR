# Confidential data must be stored on esqlabs sharepoint
# Use code like below to retrieve data from sharepoint
# Never version them on GitHub.


# install.packages("Microsoft365R")

library(Microsoft365R)

# Replace the url with your sharepoint project url
site <- get_sharepoint_site(site_url = "https://esqlabs.sharepoint.com/sites/QualityManagement")

drive <- site$get_drive(drive_name = "Dokumente")

# List items available to make sure you are in the right folder
drive$list_items(path = "Templates/Time Measurement Data")

# Download the file
drive$download_file(src = "Templates/Time Measurement Data/DT04_v02.01_ProjName_TimeValuesData.xlsx",
                    dest = "Data/DT04_v02.01_ProjName_TimeValuesData.xlsx",
                    overwrite = TRUE)
