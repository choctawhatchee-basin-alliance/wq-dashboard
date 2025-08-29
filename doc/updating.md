# Updating the dashboard

This document describes how to update the CBA water quality dashboard, including adding new data, deploying the dashboard to [shinyapps.io](https://basinalliance.shinyapps.io/wq-dashboard/), and updating documentation. 

## Updating data and redeploying the dashboard

The wq-dashboard repository that hosts the source code for the dashboard includes an automated workflow that uses GitHub Actions to update data and redeploy the dashboard.  This action can be run manually each to accomplish these tasks. 

To run the update worklfow, complete the following steps. 

1. Make any changes to the source data on Google Drive.  Note that the data structure and file names cannot change when updating data. 
1. Navigate to Actions -> data-build on the main repository page.
1. On the right side of the page, you will see a button that says "Run workflow".  Clicking the button will open a dropdown menu with a green button for "Run workflow".  Click this green button to initiate the update. 
![](update.png)
1. You will see a new "data-build" process initiate from the menu.  You can click on the process to view the update progress. The update should only take about 10 or 15 minutes, but somtimes can take longer if the system or R dependencies are updating.
![](buildsteps.png)
1. If the update is successful, you will see a green check next to the process when it is complete.  You will see a red x if it fails.  You can examine the build details to see where the process failed.  Sometimes rerunning the workflow is necessary, but failures may also occur if the data import steps require attention.  In the latter case, please email [mbeck@tbep.org](mailto:mbeck@tbep.org) for troubleshooting. 

Briefly, the update workflow installs the necessary system and R dependencies, imports and updates the data from Google Drive by running the script [prep/dat_proc.R](https://github.com/choctawhatchee-basin-alliance/wq-dashboard/blob/main/prep/dat_proc.R), deploys the dashboard to [shinyapps.io](https://basinalliance.shinyapps.io/wq-dashboard/), commits any data changes to the main repository, runs [tests](https://github.com/choctawhatchee-basin-alliance/wq-dashboard/blob/main/doc/tests.md) to verify the updated data do not have any issue, and runs cleanup steps to complete the workflow.

## Updating documentation

All documentation on the dashboard is included in the folder [wq-dashboard/www](https://github.com/choctawhatchee-basin-alliance/wq-dashboard/tree/main/wq-dashboard/www).

