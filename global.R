## global.R
library(tidyverse)
library(shiny)
library(shinydashboard)
library(leaflet)
library(highcharter)
library(shinyWidgets) # enhanced radio button
library(reshape2)
library(sf)
library(shinyBS)
library(shinyjs)
library(htmltools)
library(ggthemes)
library(RCurl)
library(png)
library(grid)
library(rintrojs)
# library(glue)
# library(shinycssloaders)

# Total <- read_csv("Total.csv")
# Total[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
# Total2 <- read_csv("Total2.csv")
# Total2[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
# Total$star <- c(rep(NA, 29), 1750000,rep(NA,6))
# 




Private <- read_csv("PrivateAngling2.csv")
Private[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
Private$star <- c(rep(NA, 29), 0.325,rep(NA,6))
###Private2 is for Alternative 5 only
Private2 <- read_csv("PrivateAngling.csv")
Private2[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
Private2$star <- c(rep(NA, 29), 1000000,rep(NA,6))
ForHire <- read_csv("ForHire2.csv")
ForHire[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
ForHire$star <- c(rep(NA, 29), 0.325,rep(NA,6))

ForHire2 <- read_csv("ForHire.csv")
ForHire2[30,2:6] <- NA ##remove 2010 (i.e., oil spill year)
ForHire2$star <- c(rep(NA, 29), 750000,rep(NA,6))

#########Total 2
### Adjust methodology to weight proportions by allocation
TotalFLW <- (Private$FLW * .577) + (ForHire$FLW * .423)
TotalAL <- (Private$AL * .577) + (ForHire$AL * .423)
TotalMS <- (Private$MS * .577) + (ForHire$MS * .423)
TotalLA <- (Private$LA * .577) + (ForHire$LA * .423)
TotalTX <- (Private$TX * .577) + (ForHire$TX * .423)
Total3 <- tibble(YEAR = ForHire$YEAR,
                 FLW=TotalFLW,
                 AL=TotalAL,
                 MS=TotalMS,
                 LA=TotalLA,
                 TX=TotalTX)
Total2 <- Total3
########################## End Total 2
###Load effort data for Alternative 6
TotalEffort <- read_csv("TotalEffort.csv")
PrivateEffort <- read_csv("PrivateEffort.csv")
ForHireEffort <- read_csv("ForhireEffort.csv")
##Load data for Alternative 6
## rasterize.R for details.
load("mappedData.RData")
load("StateBoundaries.RData")
pal <- colorNumeric(
  palette = "viridis",
  domain = Fig7mid$layer, reverse=TRUE)

map <- leaflet() %>%
  #addTiles() %>% 
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/Mapserver/tile/{z}/{y}/{x}',
           options = providerTileOptions(noWrap = TRUE)) %>%
  addScaleBar(position="bottomright") %>%
  setView(-88, 27, zoom=5) %>% 
  #addMiniMap() %>%
  addPolygons(data=Fig7mid, color = ~pal(layer),weight=1,
              opacity=.6, fillOpacity=0.5, group='Biomass') %>% 
  addPolylines(data=ALMS, color="#2ca25f", weight=3, group="State boundaries") %>% 
     addPolylines(data=EEZ, color="#636363", weight=3, group="EEZ boundary") %>% 
  addPolylines(data=FLLA, color="#2ca25f", weight=3, group="State boundaries") %>% 
  addLegend("bottomright",pal = pal, 
            values = Fig7mid$layer,title = "Index of biomass - with artificial strucures",
            opacity = 0.5) %>% 
  addLayersControl(overlayGroups=c("Biomass",#"State boundaries",
                                   "EEZ boundary"),
                   options = layersControlOptions(collapsed = FALSE))

#########Description for UI
Alt1Text <- HTML("For a red snapper state management program to be enacted,
a portion of the recreational sector ACL would need to be designated for that 
state.  The recreational sector ACL is currently divided into separate private
angling and federal for-hire component ACLs.  This action addresses how to 
apportion the component ACLs of the recreational sector to provide a portion of
the recreational ACL to a state that has an approved state management program
for red snapper.  A state would establish its state management program through
a state-specific plan amendment.  For states that do not participate in state
management, management would continue with the remaining private angling and
federal for-hire component ACLs.
<p><br>

Allocation is an inherently controversial issue because a limited resource is 
divided among competing user groups, each of which benefits from receiving the 
largest portion possible.  In this action, the Council is determining the method
to calculate the apportionment, not the percentage each state would receive.  
The percentages would change based on the data used in the calculation equation.
Additionally, the landings are subject to high levels of uncertainty, especially
for Mississippi, and should be viewed with caution.  Regardless of the 
alternative selected, in some years, each state’s landings exceeds its average 
landings.  This means that requiring a state with an active state management 
program to constrain its catches to a fixed percentage of the recreational 
sector ACL could restrict the fluctuations in annual landings that occur in 
some years. Alternative 1 (No Action) would not apportion the recreational 
sector ACL among the states.  Separate management of the private angling and 
for-hire components’ harvest of red snapper would continue throughout federal 
waters of the Gulf through 2022.  Currently, there is no expressed state 
allocation; the proportion of the total recreational landings made up by 
each state varies from year to year. Tables are provided for landings by the 
recreational sector as a whole, the private angling component, and the federal 
for-hire component.  

<p><br>

<b>Alternative 1:</b> No Action - Do not establish an 
                 allocation of the recreational sector component ACLs that 
                 may be used for state management programs. <p><br>")

Alt2Text <- HTML("<b>Alternative 2</b> provides eight options to apportion the 
                 recreational sector ACL based on the average proportion of 
                 historical landings for various time series that end in 2015.
                 The tables below provide the resulting percentages from 
                 apportioning the private angling (57.7%) and for-hire (42.3%) 
                components of the ACL by state.  For the private angling component, 
                 the sum of the state private angling ACLs for each alternative
                 totals 100% of the private angling ACL.  The federal for-hire 
                 component is assigned the remaining 42.3% of the recreational sector ACL.  
Dependings on the alternative selected in the previous action, the federal for-hire component 
may remain under federal management.  The allocation percentages would be applicable if the Council decides to include the for-hire component in state management.")

Alt3Text <- HTML("<b>Alternative 3</b> provides four options to apportion the 
                 recreational sector ACL based on the average proportion of 
                 historical landings for various time series ending in 2009.  
                 The difference between <b>Alternative 2</b> and <b>3</b> is that the time 
                 series ends in 2015 under Alternative 2 and ends in 2009 under 
                 Alternative 3.  For <b>Alternative 3</b>, the tables below provide the 
                 resulting percentages from apportioning the private angling 
                 component ACL by state for <b>Alternative 3</b>.")

Alt4Text <- HTML("<b>Alternative 3</b> provides options for excluding particular years
                 from the historical landings averages:  
2006 (<b>Option 3a</b>),  2014 (<b>Option 3b</b>), and 2015 (<b>Option 3c</b>).")

Alt5Text <- HTML("
<b>Alternative 4</b>:  Establish an allocation of the recreational sector ACL that may 
be used for state management programs by apportioning the private angling ACL 
and federal for-hire ACL among the states based on each state’s average of the 
best ten years of historical landings for the years 1986-2015.
<p>
Alternative 4 would apportion the recreational sector ACL by 
averaging each state’s highest 10 years of red snapper landings for each 
component for the years 1986-2015, and then converting the average landings
into percentages.
<p> Alternative 4 currently considers only the top 10 years for each state
from 1986 through 2015 (2010 excluded).  However, a different number of years
or range of years could be selected.  Use the sliders below to explore these options.")

Alt6Text <- HTML("<b>Alternative 5</b> would establish an allocation of the 
recreational sector ACL that may be used for state management programs by
apportioning the private angling ACL and federal for-hire ACL among the states 
based on spatial abundance of red snapper biomass and recreational trips based
                 on the selected recreational components, time series for trips 
                 (or landings), and weighting factors.")

percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}
#   hc_add_theme(hc_theme_smpl()) %>% 
#   hc_yAxis(title = list(text = "Landings (lbs ww)"),
#            labels = list(style = list(color = "#000000", fontWeight="bold"))) %>% 
#   hc_exporting(enabled = TRUE, url="https://export.highcharts.com",
#                filename = "Recreational Landings") %>% 
#   hc_title(text = "Recreational red snapper landings")

inline_numericInput=function(ni){
  tags$div( class="form-inline",ni)
}

enableBookmarking(store="url")


