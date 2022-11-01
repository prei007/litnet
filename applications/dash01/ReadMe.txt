Here's how to get the Plus Gallery working: 

The call to dashboardPage in the UI needs to be shinydashboardPlus::dashboardPage(). 

The library sequence needs to be 

library(shinydashboard)
library(shinydashboardPlus)

then run: 
shinydashboardPlusGallery().

