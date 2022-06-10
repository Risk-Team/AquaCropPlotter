correct AquaCrop name
legend of variable

homepage standard, plugin > daily, seasonal

plot 
-select name from user


statistics-
30 year period - averages
summary stat
slope, variability in peroid
garch model - model variance
timechange point analysis mcp package



button size larger
hide sidebar and open up when clicked
dynamic sidebar specific to differnt mode, select mode in menu navi bar tab
homepage - user guide, flowchart-

plot
facet labeller function to add custom labels-
text label bigger in plots
text in selection box smaller size

change climate model var to climate
bug element select if deselect all plot disappear

box to select variables for plotting coming up in order
customisation come last
add grouping mean variable

add explanation in the select box


stats
summary stats
pca
variable determinant effects

- standard upload
give options to add multiple rounds of simulations eg season,crop,
add append more dataset and associate custom parameter

actionButton('switchtab', 'Switch tab')

observeEvent(input$switchtab, {
  newtab <- switch(input$tabs,
                   "dashboard" = "widgets",
                   "widgets" = "dashboard"
  )
  updateTabItems(session, "tabs", newtab)
})
}



-show interive plot first
-increase font size of facet_grid strip.text 
-(rotate year axis text)
-ggplot size customise display

- legend dot size increase
-theme bw
-change default theme()
-defaut with grid
-facet grid background remove, only outline and text black

-try plot and save different data()
-add info
-standard upload
-home page diagram
-analysis example


-home page and display theme design






-standard- upload different parameters with the file. eg. crop,climate etc

-dimension
-customised plot button direct to customise box
- update rename button
-legend change name/remove

- shape different group
-customise point size, alpha



