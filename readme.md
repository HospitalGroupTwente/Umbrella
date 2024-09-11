# UMBRELLA 

This repository contains the Umbrella Dashboard, a tool designed specifically for the ZGT hospital infrastructure. The dashboard is powered by a back-end that automatically retrieves and updates data every week through the CastorEDC API. This ensures that the information remains current without manual intervention.

The front-end is built using the R Shiny library, providing an interactive and user-friendly interface. It can be deployed on Shinyproxy for seamless integration within the ZGT environment.

Please note that the dashboard has been tailored for ZGT’s specific setup, so certain queries or design elements may not be transferable to other healthcare settings. While it can serve as a reference, adjustments may be required for use in different infrastructures. 


## Usage
1. Change the back_end/config.R and front_end/config.yml files with the right credentials.    
2. Docker the applications or run them without containerization. Some filepaths might need to be altered depending on your R working direcotry
3. If containerized: Start the Dockers, the Shiny Docker can be deployed on shinyproxy

## Contact
For any question please contact Job Maathuis at j.maathuis@zgt.nl or github@zgt.nl 