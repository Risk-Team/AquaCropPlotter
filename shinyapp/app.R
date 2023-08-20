# p_load install the packages if not present
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,shinydashboard, tidyverse, DT, lubridate, shinyjs, shinyBS, furrr, broom, scales)

#sets of input variables to select for plotting
input_color_choice <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
input_plot_element_choice <- c("point", "line", "linear_trend", "loess_smooth_trend","background_grid")
input_legend_pos <- c("none","right","bottom","left","top")
input_shape_choice <- c("circle", "triangle", "rectangle", "diamond", "cross", "hollow_circle", "hollow_triangle", "hollow_rectangle", "hollow_diamond")
input_linetype_choice <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

#define UI dashboard
ui <- dashboardPage(
  # dashboardHeader(
  #   # Set height of dashboardHeader
  #   tags$li(class = "dropdown",
  #           tags$style(".main-header .logo {height: 130px}")
  #           ),
  #   #use image logo, link to github
  #   title = tags$a(href="https://github.com/Risk-Team/aquacrop_shiny",
  #           tags$img(src="shinyaquacrop_logo.png",height="121",width="181"))
  #   ),
  dashboardHeader(title = "AquaCropPlotter"),
  
  dashboardSidebar(collapsed = FALSE,
                   # Adjust the sidebar padding to allow large logo in the heading
                   #tags$style(".left-side, .main-sidebar {padding-top: 130px}"),
                   
                   sidebarMenu(id = "menu_tabs",
                               menuItem("Home", tabName = "tab_home", icon = icon("home")),
                               menuItem("Workflow", tabName = "aquacrop_plugin", icon = icon("list-alt"), startExpanded = TRUE,
                                        menuSubItem("Upload data", tabName = "tab_upload_data", icon = icon("caret-right")),
                                        menuSubItem("Combine data", tabName = "tab_combined_data_plugin", icon = icon("caret-right")),
                                        menuSubItem("Plot", tabName = "tab_plot_plugin", icon = icon("caret-right")),
                                        menuSubItem("Analysis", tabName = "tab_analysis_plugin", icon = icon("caret-right"))
                               ),
                               menuItem("Glossary", tabName = "aquacrop_glossary", icon = icon("book")),
                               menuItem("Help", tabName = "aquacrop_help", icon = icon("question-circle"))
                   )
  ),
  
  dashboardBody(
    #use shinyjs library
    useShinyjs(),
    #customise fonts and colors in the header and sidebar 
    tags$head(tags$style(HTML(".main-header .logo {font-weight: bold; font-size: 24px;}
                                    .main-sidebar {font-weight: bold; font-size: 22px;}
                                    .treeview-menu>li>a {font-weight: bold; font-size: 22px!important;}
                                    
                                    .skin-blue .main-header .logo {background-color: #ffffff; color: #446380;}
                                    .skin-blue .main-header .logo:hover{background-color: #ffffff!important;}
                                    .skin-blue .main-header .navbar {background-color: #ffffff;}
                                    .skin-blue .main-sidebar {background-color: #D4EAFF; color: #D4EAFF;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #D4EAFF; color: #414042;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #446380!important; color: #D4EAFF!important;border-left-color: #446380;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #446380; color: #ffffff;border-left-color: #446380;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu .menu-open .active a{background-color: #D4EAFF!important; color: #414042!important;}
                                    .box.box-solid.box-primary>.box-header {background-color: #5792c9;}
                                    .content-wrapper, .right-side {background-color: #f2f2f2;}
                                    .content-wrapper, .right-side {background-color: #ffffff;}
                                    .btn {background-color: #D4EAFF;}

                                    #add customise color to palette choices 
                                    .option[data-value=black], .item[data-value=black] {background: #000000 !important; color: white !important;}
                                    .option[data-value=grey], .item[data-value=grey] {background: #999999 !important; color: black !important;}
                                    .option[data-value=skyblue], .item[data-value=skyblue] {background: #56B4E9 !important; color: black !important;}
                                    .option[data-value=orange], .item[data-value=orange] {background: #E69F00 !important; color: black !important;}
                                    .option[data-value=green], .item[data-value=green] {background: #009E73 !important; color: black !important;}
                                    .option[data-value=yellow], .item[data-value=yellow] {background: #F0E442 !important; color: black !important;}
                                    .option[data-value=blue], .item[data-value=blue] {background: #0072B2 !important; color: black !important;}
                                    .option[data-value=vermillion], .item[data-value=vermillion] {background: #D55E00 !important; color: black !important;}
                                    .option[data-value=purple], .item[data-value=purple] {background: #CC79A7 !important; color: black !important;}
                                    .option[data-value=red], .item[data-value=red] {background: #E41A1C !important; color: black !important;}
                                    .option[data-value=lightgreen], .item[data-value=lightgreen] {background: #A6D854 !important; color: black !important;}
                                    
                                    .btn-xs { width: 20px!important; height: 20px!important; font-size: 12px!important } #customise info popup icon

                                  "))),
    
    #customise style text size of different elements
    tags$style(type='text/css', ".selectize-input { font-size: 20px; line-height: 20px; width:80%; } 
                                      .selectize-dropdown { font-size: 20px; line-height: 20px; width:80%; }
                                      .control-label { font-size: 20px; line-height: 20px; }
                                      .btn { font-size: 20px; }
                                      .form-control { font-size: 18px; line-height: 18px; height:42px; width:80%; }
                                      .box-title { font-size: 22px!important; line-height: 22px; font-weight:bold; }
                                      .nav-tabs { font-size: 22px; line-height: 20px; font-weight:bold; }
                                      .shiny-output-error-validation { font-size: 22px; line-height: 22px; padding-top: 15px; }
                                      .shiny-notification { font-size: 20px;}
                                    
                                    #font size of slider input
                                    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {font-size: 14px!important}

                                    .irs-min {font-size: 14px!important;}
                                    .irs-max {font-size: 14px!important;}
                   "),
    
    tabItems(
      tabItem(tabName = "tab_home",
              h2(
                fluidRow(
                  tags$style(".box {background-color: transparent; border-color: transparent; border-top-color: transparent; box-shadow: none}"),
                  box(width = 12,height=0,
                      imageOutput("aquacrop_logo"))
                )
              )
      ),
      tabItem(tabName = "tab_upload_data",
              h2(
                #to select data mode
                selectizeInput("standard_vs_plugin_select", "AquaCrop programme used", choices = c("GUI","plugin"), multiple = TRUE, options = list(maxItems = 1)),
                
                #show upload data according to mode selected
                #plugin
                fluidRow(
                  conditionalPanel(condition = "input.standard_vs_plugin_select == 'plugin'",
                                   #display boxes for data and prm files upload
                                   box(title = "Batch upload all files", status = "primary", solidHeader = TRUE, width = 12,
                                       fileInput("upload_all_files", "Upload all files (season.OUT, .PRM or .PRO, (optional: day.out))", multiple = TRUE),
                                       tableOutput("upload_progress"),
                                       div(textOutput("upload_warning"), style = "font-size: 18px;")
                                   ),
                                   box(title = "Project files", status = "primary", solidHeader = TRUE, width = 4,
                                       #upload parameter file
                                       #fileInput("upload_prm_files", "Add more files (.PRM)", multiple = TRUE, accept = ".PRM"),
                                       div(dataTableOutput("upload_prm_combined_display"), style = "font-size: 75%; width: 100%"),
                                       div(dataTableOutput("missing_prm_file_error"), style = "font-size: 75%; width: 100%")
                                   ),
                                   box(title = "Seasonal data files", status = "primary", solidHeader = TRUE, width = 4,
                                       #upload data file
                                       #fileInput("upload_data_files", "Add more files (season.OUT)", multiple = TRUE, accept = ".OUT"),
                                       div(dataTableOutput("upload_data_combined_display"), style = "font-size: 75%; width: 100%"),
                                       dataTableOutput("missing_seasonal_file_error")
                                   ),
                                   box(title = "Daily data files", status = "primary", solidHeader = TRUE, width = 4,
                                       #upload data file
                                       #fileInput("upload_daily_data_files", "Add more files (day.OUT)", multiple = TRUE, accept = ".OUT"),
                                       div(dataTableOutput("upload_daily_data_combined_display"), style = "font-size: 75%; width: 100%")
                                   )
                  )
                ),
                #standard
                fluidRow(
                  conditionalPanel(condition = "input.standard_vs_plugin_select == 'GUI'",
                                   box(title = "Data and PRM files", status = "primary", solidHeader = TRUE, width = 6,
                                       #upload data file
                                       fileInput("upload_data_files_standard", "Upload data files (.OUT and .PRM)", multiple = TRUE)
                                   ),
                                   box(title = "Uploaded files", status = "primary", solidHeader = TRUE, width = 6,
                                       div(dataTableOutput("upload_standard_list"), style = "font-size: 75%; width: 100%")
                                   )
                  )
                )
              )
      ),
      tabItem(tabName = "tab_combined_data_plugin",
              h2(
                fluidRow(
                  #display combined data table
                  tabBox(width = 12,
                         tabPanel(title = "Project files",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = FALSE,
                                  #button for downloading all combined data
                                  downloadButton("download_combined_prm", "Download", style = "margin-bottom: 15px; "),
                                  #data table
                                  div(dataTableOutput("prm_combined_display"), style = "font-size: 75%; width: 100%")
                         ),
                         tabPanel(title = "Seasonal dataset",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = FALSE,
                                  #button for downloading all combined data
                                  downloadButton("download_combined_dataset", "Download", style = "margin-bottom: 15px; "),
                                  #data table
                                  div(dataTableOutput("data_prm_combined_display"), style = "font-size: 75%; width: 100%")
                         ),
                         tabPanel(title = "Daily dataset",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = FALSE,
                                  #button for downloading all combined data
                                  downloadButton("download_combined_daily_dataset", "Download", style = "margin-bottom: 15px; "),
                                  #data table
                                  div(dataTableOutput("daily_data_prm_combined_display"), style = "font-size: 75%; width: 100%")
                         )
                  ),  
                  box(title = "Rename column",
                      width = 4,
                      #height = "550px",
                      status = "primary",
                      solidHeader = TRUE,
                      selectizeInput("rename_col_from", "Select column to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                      textInput("rename_col_to", "Rename to"),
                      actionButton("rename_col_button", "Rename")
                  ),
                  box(title = "Filter dataset",
                      width = 4,
                      #height = "550px",
                      status = "primary",
                      solidHeader = TRUE,
                      selectizeInput("filter_data_column", "Select column to filter by", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                      conditionalPanel(condition = "input.filter_data_column == 'Year'",
                                       sliderInput("filter_data_num", "Select year range", sep = "", min = 0, max = 0, value = c(0,0)),
                      ),
                      conditionalPanel(condition = "input.filter_data_column != 'Year'",
                                       selectizeInput("filter_data_chr", "Select values to keep", choices = NULL, multiple = TRUE),
                      ),
                      actionButton("filter_data_button", "Filter"),
                      actionButton("filter_reset_button", "Reset")
                  )
                ),
                actionButton("advanced_button", "Advanced data labeling", style = "margin-bottom: 15px; margin-left: 20px; "),
                shinyjs::hidden(div(id = "advanced_box", fluidRow(
                  box(title = "Add custom labels by year range",
                      width = 4,
                      #height = "550px",
                      status = "primary",
                      solidHeader = TRUE,
                      selectizeInput("historical_column", tags$span("Select column to add label to", bsButton("advanced_info", label = "", icon = icon("info"), size = "extra-small")), choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                      bsPopover(id = "advanced_info", title = "add custom labels to values of a selected column in a selected year range", placement = "right", trigger = "hover"),
                      sliderInput("historical_year", "Select year range to label", sep = "", min = 0, max = 0, value = c(0,0)),
                      textInput("historical_text", "Label as"),
                      actionButton("historical_button", "Label"),
                      actionButton("historical_reset_button", "Reset")
                  ))))
              )
      ),
      tabItem(tabName = "tab_plot_plugin",
              h2(
                fluidRow(
                  tabBox(width = 12,
                         tabPanel(title = "Scatter plot",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = FALSE,
                                  fluidRow(
                  box(title = "Select plotting variables",
                      width = 4,
                      height = "520px",
                      status = "primary",
                      solidHeader = TRUE,
                      selectizeInput("plot_mode", "Data type to plot", c("daily","seasonal"), multiple = TRUE, options = list(maxItems = 1)),
                      selectizeInput("y_var", "Variable to plot on Y axis", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                      selectizeInput("x_var", "Variable to plot on X axis", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                      selectizeInput("plot_element", 
                                     label = tags$span("Components of plot to show", bsButton("plot_info5", label = "", icon = icon("info"), size = "extra-small")), 
                                     input_plot_element_choice, multiple = TRUE, selected = c("point", "linear_trend","background_grid")),
                      bsPopover(id = "plot_info5", title = "Select or delete any number of components to show in the plot", placement = "right", trigger = "hover"),
                      div(style = "position:absolute;right:0.1em; bottom:0.1em;date",actionButton("plot_next1", "Plot", icon = icon("chevron-right")))
                  ),
                  shinyjs::hidden(div(id = "hiddenbox2",
                                      box(title = "Optional: Select grouping variables",
                                          width = 4,
                                          height = "520px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          selectizeInput("col_var", 
                                                         label = tags$span("Variable to split into colors by",  bsButton("plot_info3", label = "", icon = icon("info"), size = "extra-small")), 
                                                         choices = NULL,
                                                         multiple = TRUE, options = list(maxItems = 1)),
                                          bsPopover(id = "plot_info3", title = "Each value of the selected variable will be plotted with different colors", placement = "right", trigger = "hover"),
                                          selectizeInput("shape_var", 
                                                         label = tags$span("Variable to split into point shapes by",  bsButton("plot_info7", label = "", icon = icon("info"), size = "extra-small")), 
                                                         choices = NULL,
                                                         multiple = TRUE, options = list(maxItems = 1)),
                                          bsPopover(id = "plot_info7", title = "Each value of the selected variable will be plotted with different shapes", placement = "right", trigger = "hover"),
                                          selectizeInput("linetype_var", 
                                                         label = tags$span("Variable to split into line types by",  bsButton("plot_info9", label = "", icon = icon("info"), size = "extra-small")), 
                                                         choices = NULL,
                                                         multiple = TRUE, options = list(maxItems = 1)),
                                          bsPopover(id = "plot_info9", title = "Each value of the selected variable will be plotted with different line types", placement = "right", trigger = "hover"),
                                          selectizeInput("facet_var", 
                                                         label = tags$span("Variable to split into subpanels by", bsButton("plot_info4", label = "", icon = icon("info"), size = "extra-small")), 
                                                         choices = NULL,
                                                         multiple = TRUE, options = list(maxItems = 1)),
                                          selectizeInput("facet_var2", 
                                                         label = NULL,
                                                         choices = NULL,
                                                         multiple = TRUE, options = list(maxItems = 1)),
                                          bsPopover(id = "plot_info4", title = "Selected variable will be used to split plot into subplots. maximum of 2 variables can be selected", placement = "right", trigger = "hover")
                                      )
                  )),
                  shinyjs::hidden(div(id = "hiddenbox3",
                                      box(title = "Optional: Summary statistics",
                                          width = 4,
                                          height = "520px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          selectInput("use_mean", 
                                                      label = tags$span("Plot summary statistics",   bsButton("plot_info1", label = "", icon = icon("info"), size = "extra-small")), 
                                                      c("none","mean","median", "sum", "min", "max"), selected = "none"),
                                          bsPopover(id = "plot_info1", title = "plot the selected summary statistics from all data points within the grouping variables selected in the previous box and additional variables in the box below", placement = "right", trigger = "hover"),
                                          conditionalPanel(condition = "input.use_mean != 'none'",
                                                           selectizeInput("group_var", 
                                                                          label = tags$span("Extra grouping variables for summary statistics", bsButton("plot_info15", label = "", icon = icon("info"), size = "extra-small")), 
                                                                          choices = NULL,
                                                                          multiple = TRUE)),
                                          bsPopover(id = "plot_info15", title = "Selected variable will be used for calculating summary statistics together with previously selected grouping variables for plotting ", placement = "right", trigger = "hover"),
                                      )
                  ))
                ),
                shinyjs::hidden(div(id = "hiddenbox5",
                                    div(style= "overflow-x: scroll;" , fluidRow(
                                      box(title = "Rename / Reorder",
                                          width = 2,
                                          height = "700px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          selectizeInput("rename_variable", "Select variable to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                          selectizeInput("rename_from", "Select value to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                          textInput("rename_to", "Rename to"),
                                          actionButton("rename_button", "Rename", style = "margin-bottom: 15px; "),
                                          selectizeInput("reorder_variable", "Select variable to reorder", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                          selectizeInput("reorder_order", "Select values in order", choices = NULL, multiple = TRUE),
                                          actionButton("reorder_button", "Reorder", style = "margin-bottom: 15px; ")
                                      ),
                                      box(title = "Customise plot",
                                          width = 2,
                                          height = "730px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          selectizeInput("col_palette", 
                                                         label = tags$span("color palette", bsButton("plot_info6", label = "", icon = icon("info"), size = "extra-small")), 
                                                         input_color_choice, multiple = TRUE),
                                          bsPopover(id = "plot_info6", title = "Select the same number of colors as the number of values within the selected variable, in order", placement = "right", trigger = "hover"),
                                          selectizeInput("shape_palette", 
                                                         label = tags$span("point shape palette", bsButton("plot_info8", label = "", icon = icon("info"), size = "extra-small")), 
                                                         input_shape_choice, multiple = TRUE),
                                          bsPopover(id = "plot_info8", title = "Select the same number of shapes as the number of values within the selected variable, in order", placement = "right", trigger = "hover"),
                                          selectizeInput("linetype_palette", 
                                                         label = tags$span("line type palette", bsButton("plot_info10", label = "", icon = icon("info"), size = "extra-small")), 
                                                         input_linetype_choice, multiple = TRUE),
                                          bsPopover(id = "plot_info10", title = "Select the same number of line types as the number of values within the selected variable, in order", placement = "right", trigger = "hover"),
                                          textInput("point_size", "point size", value = "2"),
                                          textInput("line_size", "line size", value = "1"),
                                          sliderInput("point_transparency", "point transparency", min = 0, max = 1, value = 1, step = 0.1, ticks = FALSE),
                                          sliderInput("line_transparency", "line transparency", min = 0, max = 1, value = 1, step = 0.1, ticks = FALSE)
                                      ),
                                      box(title = "Customise labels",
                                          width = 2,
                                          height = "730px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          textInput("title_label", "plot title"),
                                          textInput("y_var_label", "Y axis label"),
                                          textInput("x_var_label", "X axis label"),
                                          textInput("x_axis_label_angle", "X axis label angle", value = "45"),
                                          selectInput("legend_position", "Legend position", input_legend_pos, selected = "bottom"),
                                          selectInput("show_legend_title", "Legend title", c("yes","no"), selected = "yes")
                                      ),
                                      box(title = "Customise font size",
                                          width = 2,
                                          height = "730px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          textInput("font_size_plot_title", "plot title", value = "16"),
                                          textInput("font_size_axis_text", "axis text", value = "16"),
                                          textInput("font_size_axis_title", "axis title", value = "16"),
                                          textInput("font_size_legend", "legend", value = "16"),
                                          textInput("font_size_facet", "subpanel label", value = "16")
                                      ),
                                      box(title = "Customise axis",
                                          width = 2,
                                          height = "730px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          sliderInput("y_var_range", "Y axis range to plot", sep = "", min = 0, max = 0, value = c(0,0)),
                                          sliderInput("x_var_range", "X axis range to plot", sep = "", min = 0, max = 0, value = c(0,0))
                                      ),
                                      box(title = "Export plot",
                                          width = 2,
                                          height = "730px",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          textInput("export_plot_width", "Width (cm)", value = "19"),
                                          textInput("export_plot_height", "Height (cm)", value = "12"),
                                          selectInput("export_plot_format", "Format", c("pdf","png"), selected = "pdf"),
                                          downloadButton("ggplot_plugin_download", "Download"))
                                    )
                                    ))),
                shinyjs::hidden(div(id = "hiddenbox4",
                                    fluidRow(
                                      tabBox(width = 12,
                                             height = "900px",
                                             id = "plugin_plot_tabbox",
                                             tabPanel("Plot",
                                                      div(style = "position:absolute;right:1em; top:0.25em;",actionButton("plot_next5", "Customise & export plot")),
                                                      div(style = "overflow-x: scroll; height:850px; overflow-y: scroll;", plotOutput("ggplot_plugin_display")),
                                                      
                                             )
                                      )
                                    )))
                  ),
                tabPanel(title = "Boxplot",
                         width = 12,
                         status = "primary",
                         solidHeader = FALSE,
                         fluidRow(
                           box(title = "Select plotting variables",
                               width = 4,
                               height = "520px",
                               status = "primary",
                               solidHeader = TRUE,
                               selectizeInput("y_var_boxplot", "Variable to plot on Y axis", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                               selectizeInput("x_var_boxplot", "Variable to plot on X axis", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                               # selectizeInput("plot_element_boxplot", 
                               #                label = tags$span("Components of plot to show", bsButton("plot_info5_boxplot", label = "", icon = icon("info"), size = "extra-small")), 
                               #                input_plot_element_choice, multiple = TRUE, selected = c("point", "linear_trend","background_grid")),
                               bsPopover(id = "plot_info5_boxplot", title = "Select or delete any number of components to show in the plot", placement = "right", trigger = "hover"),
                               div(style = "position:absolute;right:0.1em; bottom:0.1em;date",actionButton("plot_next1_boxplot", "Plot", icon = icon("chevron-right")))
                           ),
                           shinyjs::hidden(div(id = "hiddenbox2_boxplot",
                                               box(title = "Optional: Select grouping variables",
                                                   width = 4,
                                                   height = "520px",
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   selectizeInput("col_var_boxplot", 
                                                                  label = tags$span("Variable to split into colors by",  bsButton("plot_info3_boxplot", label = "", icon = icon("info"), size = "extra-small")), 
                                                                  choices = NULL,
                                                                  multiple = TRUE, options = list(maxItems = 1)),
                                                   bsPopover(id = "plot_info3_boxplot", title = "Each value of the selected variable will be plotted with different colors", placement = "right", trigger = "hover"),
                                                   selectizeInput("facet_var_boxplot", 
                                                                  label = tags$span("Variable to split into subpanels by", bsButton("plot_info4_boxplot", label = "", icon = icon("info"), size = "extra-small")), 
                                                                  choices = NULL,
                                                                  multiple = TRUE, options = list(maxItems = 1)),
                                                   selectizeInput("facet_var2_boxplot", 
                                                                  label = NULL,
                                                                  choices = NULL,
                                                                  multiple = TRUE, options = list(maxItems = 1)),
                                                   bsPopover(id = "plot_info4_boxplot", title = "Selected variable will be used to split plot into subplots. maximum of 2 variables can be selected", placement = "right", trigger = "hover"),
                                                   conditionalPanel(condition = "input.facet_var_boxplot == 'time.horizon'",
                                                                    sliderInput("time_horizon_duration", "Time horizon duration (year)", sep = "", min = 1, max = 1, value = 1),)
                                               )
                           )),
                           shinyjs::hidden(div(id = "hiddenbox3_boxplot",
                                               box(title = "Optional: Plot values relative to an average from reference period",
                                                   width = 4,
                                                   height = "520px",
                                                   status = "primary",
                                                   solidHeader = TRUE,
                                                   selectInput("use_mean_boxplot", 
                                                               label = tags$span("Plot relative values",   bsButton("plot_info1_boxplot", label = "", icon = icon("info"), size = "extra-small")), 
                                                               c("no","yes"), selected = "no"),
                                                   bsPopover(id = "plot_info1_boxplot", title = "plot values as relative percentage change compared to an average from selected reference period", placement = "right", trigger = "hover"),
                                                   conditionalPanel(condition = "input.use_mean_boxplot != 'no'",
                                                                    sliderInput("ref_period_boxplot", "reference period (year range)", sep = "", min = 0, max = 0, value = c(0,0)),
                                                                    downloadButton("download_ref_norm_dataset", "Download relative data", style = "margin-bottom: 15px; "),
                                                   )
                                                   
                                               )
                           ))
                         ,
                        shinyjs::hidden(div(id = "hiddenbox5_boxplot",
                                               div(fluidRow(
                                                 box(title = "Rename / Reorder",
                                                     width = 2,
                                                     height = "700px",
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     selectizeInput("rename_variable_boxplot", "Select variable to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                                     selectizeInput("rename_from_boxplot", "Select value to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                                     textInput("rename_to_boxplot", "Rename to"),
                                                     actionButton("rename_button_boxplot", "Rename", style = "margin-bottom: 15px; "),
                                                     selectizeInput("reorder_variable_boxplot", "Select variable to reorder", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                                     selectizeInput("reorder_order_boxplot", "Select values in order", choices = NULL, multiple = TRUE),
                                                     actionButton("reorder_button_boxplot", "Reorder", style = "margin-bottom: 15px; ")
                                                 ),
                                                 box(title = "Customise plot",
                                                     width = 2,
                                                     height = "730px",
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     selectizeInput("col_palette_boxplot", 
                                                                    label = tags$span("color palette", bsButton("plot_info6_boxplot", label = "", icon = icon("info"), size = "extra-small")), 
                                                                    input_color_choice, multiple = TRUE),
                                                     bsPopover(id = "plot_info6_boxplot", title = "Select the same number of colors as the number of values within the selected variable, in order", placement = "right", trigger = "hover"),

                                                 ),
                                                 box(title = "Customise labels",
                                                     width = 2,
                                                     height = "730px",
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     textInput("title_label_boxplot", "plot title"),
                                                     textInput("y_var_label_boxplot", "Y axis label"),
                                                     textInput("x_var_label_boxplot", "X axis label"),
                                                     textInput("x_axis_label_angle_boxplot", "X axis label angle", value = "45"),
                                                     selectInput("legend_position_boxplot", "Legend position", input_legend_pos, selected = "bottom"),
                                                     selectInput("show_legend_title_boxplot", "Legend title", c("yes","no"), selected = "yes")
                                                 ),
                                                 box(title = "Customise font size",
                                                     width = 2,
                                                     height = "730px",
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     textInput("font_size_plot_title_boxplot", "plot title", value = "16"),
                                                     textInput("font_size_axis_text_boxplot", "axis text", value = "16"),
                                                     textInput("font_size_axis_title_boxplot", "axis title", value = "16"),
                                                     textInput("font_size_legend_boxplot", "legend", value = "16"),
                                                     textInput("font_size_facet_boxplot", "subpanel label", value = "16")
                                                 ),
                                                 box(title = "Customise axis",
                                                     width = 2,
                                                     height = "730px",
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     sliderInput("y_var_range_boxplot", "Y axis range to plot", sep = "", min = 0, max = 0, value = c(0,0)),
                                                 ),
                                                 box(title = "Export plot",
                                                     width = 2,
                                                     height = "730px",
                                                     status = "primary",
                                                     solidHeader = TRUE,
                                                     textInput("export_plot_width_boxplot", "Width (cm)", value = "19"),
                                                     textInput("export_plot_height_boxplot", "Height (cm)", value = "12"),
                                                     selectInput("export_plot_format_boxplot", "Format", c("pdf","png"), selected = "pdf"),
                                                     downloadButton("ggplot_plugin_download_boxplot", "Download"))
                                               )
                                               ))),
                           shinyjs::hidden(div(id = "hiddenbox4_boxplot",
                                               fluidRow(
                                                 tabBox(width = 12,
                                                        height = "900px",
                                                        id = "plugin_plot_tabbox_boxplot",
                                                        tabPanel("Plot",
                                                                 div(style = "position:absolute;right:1em; top:0.25em;",actionButton("plot_next5_boxplot", "Customise & export plot")),
                                                                 div(style = "overflow-x: scroll; height:850px; overflow-y: scroll;", plotOutput("ggplot_plugin_display_boxplot")),
                                                                 
                                                        )
                                                 )
                                               )))


                         )
                         )
                )
                )
              )
      ),
      tabItem(tabName = "tab_analysis_plugin",
              h2(
                fluidRow(
                  tabBox(width = 12,
                         id = "analysis_tabbox",
                         tabPanel(title = "Time period window",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = FALSE,
                                  sliderInput("time_range", label = "Select year range to analyse", sep = "", min = 1, max = 1, value = c(1,1), step = 1),
                                  sliderInput("time_period", label = "Select time period window size (years)", min = 1, max = 1, value = 1, step = 1),
                                  selectizeInput("time_period_variable", label = "Select variable to calculate summary", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                  selectizeInput("time_period_group", label = "Select grouping variable", choices = NULL, multiple = TRUE),
                                  div(dataTableOutput("data_prm_combined_timeperiod_display"), style = "font-size: 75%; width: 100%"),
                                  downloadButton("download_data_prm_combined_timeperiod", "Download")
                         ),
                         # tabPanel(title = "Stress duration",
                         #          width = 12,
                         #          status = "primary",
                         #          solidHeader = FALSE,
                         #          fluidRow(
                         #            column(3, sliderInput("StExp_threshold", label = "Threshold of water stress reducing leaf expansion (StExp)", min = 0, max = 100, value = 5, step = 1, ticks = FALSE)),
                         #            column(3, sliderInput("StSto_threshold", label = "Threshold of water stress inducing stomatal closure (StSto)", min = 0, max = 100, value = 5, step = 1, ticks = FALSE)),
                         #            column(3, sliderInput("StSen_threshold", label = "Threshold of water stress triggering early senescence (StSen)", min = 0, max = 100, value = 5, step = 1, ticks = FALSE)),
                         #            column(3, sliderInput("StTr_threshold", label = "Threshold of temperature stress affecting transpiration (StTr)", min = 0, max = 100, value = 5, step = 1, ticks = FALSE))
                         #          ),
                         #          selectizeInput("stress_group", label = "Select grouping variable", choices = NULL, multiple = TRUE),
                         #          selectInput("by_phenological", label = "Separate by phenological stages", choices = c("yes","no"), selected = "no"),
                         #          div(dataTableOutput("daily_data_prm_combined_stress_display"), style = "font-size: 75%; width: 100%"),
                         #          downloadButton("download_daily_data_prm_combined_stress", "Download"),
                         #          actionButton("append_stress_data_button", "Append stress data to Seasonal dataset for plotting and other analyses", icon = icon("share-square")),
                         #          
                         # ),
                         tabPanel(title = "Regression",
                                  width = 12,
                                  status = "primary",
                                  solidHeader = FALSE,
                                  selectizeInput("regression_mode", "Select data type", c("daily","seasonal"), multiple = TRUE, options = list(maxItems = 1)),
                                  selectizeInput("regression_y_variable", label = "Select dependent (Y) variable", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                  selectizeInput("regression_x_variable", label = "Select independent (X) variable", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                  selectizeInput("regression_group", label = "Select grouping variable", choices = NULL, multiple = TRUE),
                                  
                                  div(dataTableOutput("regression_display"), style = "font-size: 75%; width: 100%"),
                                  downloadButton("download_regression", "Download"),
                         )
                  )
                )
              )
      ),
      tabItem(tabName = "aquacrop_glossary",
              h2(fluidRow(
                box(width = 12 ,
                    div(dataTableOutput("glossary_display"), style = "font-size: 75%; width: 100%")
                )
              ))
      ),
      tabItem(tabName = "aquacrop_help",
              div(
                h2(strong("File naming convention")),
                h3("AquaCropPlotter facilitates the analysis of outputs from multiple AquaCrop simulations by automatically processing  all user-uploaded files at once. To allow the app to do this, some file naming conventions should be followed"),
                br(),
                h3(strong("1."), "Output files (day.OUT and season.OUT) and project files (.PRO or .PRM) of each simulation are recognised automatically by matching the name prefixes, so make sure all file names from one simulation have the same prefix (This should already be a default for output from AquaCrop)"),
                h3(strong("For example,"), "if a simulation is to be named simulation1, the resulting file names should be simulation1PRMday.OUT, simulation1PRMseason.OUT and simulation1.PRM"),
                br(),
                h3(strong("2."), "Variables from file name prefixes are automatically extracted when they are separated by underscores ( _ ). This allows users to supply variables/information associated with each simulation that can be used for plotting and analysis of the data inside the app"),
                h3(strong("For example,"), "if 2 simulations were run on different crops (maize and wheat) at two different locations, file name prefixes could be maize_location1 for simulation 1 and wheat_location2 for simulation 2"),
                h3("When these data are imported into the app, the detected variables are added into the dataset as new columns, designated by 'name.variable' followed by a number in the order that they were detected from the file name prefix."),
                h3("In this example, the first variable, crop, will be extracted into name.variable1 and the second variable, location, will be extracted into name.variable2",
                   "These automatically generated name.variable columns can be renamed by the user to reflect the values they contain")
                
              )
      )
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  #allow upload file size max limit of 50 MB
  options(shiny.maxRequestSize=50*1024^2)
  
  #home image
  output$aquacrop_logo <- renderImage({
    list(
      src = file.path("www/workflow.png"),
      contentType = "image/jpg",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  ##glossary for varaible names
  glossary <- reactive({
    read.csv("www/glossary.csv")
  })
  output$glossary_display <- renderDataTable(glossary(), rownames = FALSE, options = list(pageLength = 140))
  
  ##########standard
  ###upload data
  ###read upload data files and combine all data into dataframe
  upload_data_standard_combined <-
    reactive({
      #require uploaded data files before evaluating
      req(input$upload_data_files_standard)
      
      #set up parallel processing for future_map function
      plan(multisession, workers = 2) 
      
      #list of file extensions of all output files (9 files)
      file.extension = c("Clim.OUT","CompEC.OUT","CompWC.OUT","Crop.OUT","Inet.OUT","Prof.OUT","Run.OUT","Salt.OUT","Wabal.OUT")
      
      withProgress(message = "Processing data", value = 0.7,{
        #read data
        input$upload_data_files_standard %>%
          filter(str_detect(name, "\\.OUT$")) %>%
          filter(!str_detect(name, "Inet\\.OUT$")) %>% #some Inet file if dataset are empty can cause error
          #detect file extensions
          mutate(extension = str_extract(name, paste(paste0("(",file.extension,")"), collapse="|"))) %>%
          mutate(name.variable = str_replace(name, extension,"")) %>%
          mutate(dataset = future_map2(datapath, extension, function(datapath, extension){
            #read in data as lines, to identify blank lines that separate sections
            file.line = read_lines(paste0(datapath))
            line.before.data = which(file.line == "")[1] #first blank line comes before dataset
            line.after.data = which(file.line == "")[2] #second blank line comes after dataset
            
            #read in data as whole and clean spaces to tabs to allow read_tsv later
            file.clean = read_file(paste0(datapath)) %>%
              str_replace_all(" +?(?=\\S)","\t") 
            
            #read heading
            heading.skip = ifelse(extension == "Run.OUT", line.before.data, 
                                  ifelse(extension %in% c("CompEC.OUT","CompWC.OUT"),line.before.data+2, line.before.data+1))
            heading = read_lines(file = file.clean, skip = heading.skip, n_max = 1) %>%
              str_split("\\t") %>%
              unlist() 
            
            #read in data as tsv, specify lines from blank lines sectioning identified before, add heading
            line.skip = ifelse(extension == "Run.OUT", line.before.data + 2, line.before.data + 3)
            line.n = ifelse(extension == "Run.OUT", 1, line.after.data - line.before.data - 4)
            data = read_tsv(file = file.clean, skip = line.skip, n_max = line.n, col_names = heading, show_col_types = FALSE) %>%
              select(-1) #remove blank column at the start
          })) %>%
          select(-size, -type, -datapath) 
      })
    })
  
  #output uploaded files list
  output$upload_standard_list <- renderDataTable(datatable(upload_data_standard_combined() %>% select(name), options = list(scrollX = TRUE)))
  
  ##combined all daily data  
  upload_data_standard_combined_daily <- reactive({
    #require uploaded data files before evaluating
    req(input$upload_data_files_standard)
    
    #set up parallel processing for future_map function
    plan(multisession, workers = 2) 
    
    #filter out Run.OUT file as data format (seasonal)  different from others (daily)
    upload_data_standard_combined() %>%
      filter(extension != "Run.OUT") %>%
      group_by(name.variable) %>%
      nest() %>%
      mutate(all_data = future_map(data, function(data){
        data$dataset %>%
          reduce(left_join, by = c("Day", "Month", "Year", "DAP", "Stage"))
      })) %>%
      select(-data) %>%
      unnest(all_data) %>%
      mutate(date = dmy(paste(Day, Month, Year, sep="-")))
  })
  
  ##seasonal data
  upload_data_standard_combined_seasonal <- reactive({
    upload_data_standard_combined() %>%
      filter(extension == "Run.OUT") %>%
      unnest(dataset) 
  })
  
  ###read uploaded parameter files and combine
  upload_prm_standard_combined <-
    reactive({
      #require uploaded prm files before evaluating
      req(input$upload_data_files_standard)
      
      #set up parallel processing for future_map function
      plan(multisession, workers = 2) 
      
      withProgress(message = "Processing PRM data", value = 0.7,{
        #get a list of prm file path from uploaded
        prm.df = input$upload_data_files_standard %>%
          #filter to read only .prm files
          filter(str_detect(name, "\\.PRM$")) %>% 
          #read in each prm file from the list and extract parameter of interest
          mutate(parameter = future_map(datapath, function(datapath){
            #read in each prm file from the list
            prm.file = read_file(paste0(datapath))
            #extract parameter from each param file 
            #in this case we use str_extract to get parameters of the first time point (should be the same for all time points)
            #get climate file
            climate.file = str_extract(prm.file, ".+?\\.CLI") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.CLI)","")
            #get temperature file
            temperature.file = str_extract(prm.file, ".+?\\.((Tnx)|(TMP))") %>%
              unlist() %>%
              str_replace_all("(\\s)|((\\.Tnx)|(\\.TMP))","")    
            #get reference eto file
            reference.ET.file = str_extract(prm.file, ".+?\\.ETo") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.ETo)","")  
            #get rain  file
            rain.file = str_extract(prm.file, ".+?\\.PLU") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.PLU)","")  
            #get co2  file
            co2.file = str_extract(prm.file, ".+?\\.CO2") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.CO2)","")  
            #get crop
            crop.file = str_extract(prm.file, ".+?\\.CRO") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.CRO)","")  
            #get irrigation
            irrigation.file = str_extract(prm.file, ".+?\\.IRR") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.IRR)","")  
            #get field management
            field.management.file = str_extract(prm.file, ".+?\\.MAN") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.MAN)","")  
            #get soil
            soil.file = str_extract(prm.file, ".+?\\.SOL") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.SOL)","")  
            #get groundwater table
            groundwater.table.file = str_extract(prm.file, ".+?\\.GWT") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.GWT)","")  
            #get initial condition 
            initial.condition.file = str_extract(prm.file, ".+?\\.SW0") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.SW0)","")  
            #get offseason condition 
            offseason.condition.file = str_extract(prm.file, ".+?\\.OFF") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.OFF)","")
            #start and end date of cropping period
            cropping.period.start = str_extract(prm.file, "(?<=First day of cropping period - )[[:digit:]]+ [[:alpha:]]+ [[:digit:]]+") %>%
              dmy() %>%
              map_chr(function(x){paste(day(x), month(x, label = T), sep = "_")})
            cropping.period.end = str_extract(prm.file, "(?<=Last day of cropping period - )[[:digit:]]+ [[:alpha:]]+ [[:digit:]]+") %>%
              dmy() %>%
              map_chr(function(x){paste(day(x), month(x, label = T), sep = "_")})
            #put parameters together in a table
            param.table = data.frame(climate.file, temperature.file, reference.ET.file, rain.file, co2.file, crop.file, irrigation.file, field.management.file, soil.file, groundwater.table.file, initial.condition.file, offseason.condition.file, cropping.period.start, cropping.period.end)
          })) %>%
          unnest(parameter) %>%
          select(-size, -type, -datapath) %>%
          mutate(irrigation.file = ifelse(is.na(irrigation.file), "rainfed", irrigation.file)) %>%
          mutate(name.variable = str_replace(name, "\\.PRM$","")) %>%
          rename(prm.file.name = name)
      })
      
      #check name for _ delimiter to extract different variables out of file name and give number
      n.name.var = str_split(prm.df$name.variable,"_") %>%
        map(length) %>%
        unlist() %>%
        max()
      
      prm.df %>%
        separate(name.variable, into = paste0("name.variable",c(1:n.name.var)), sep = "_", remove = F)
    })
  
  
  ####add parameters to the seasonal dataset
  data_prm_standard_combined_seasonal <- reactive({
    req(input$upload_data_files_standard)
    
    withProgress(message = "Processing seasonal data", value = 0.7,{
      upload_data_standard_combined_seasonal() %>%
        # mutate(sowing.dmy = dmy(paste(Day1, Month1, Year1, sep="-"))) %>%
        # mutate(sowing.date = paste(day(sowing.dmy), month(sowing.dmy, label = T), sep = "_")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    })  
  })
  
  
  ####add parameters to the daily dataset
  data_prm_standard_combined_daily <- reactive({
    req(input$upload_data_files_standard)
    
    withProgress(message = "Processing daily data", value = 0.7,{
      upload_data_standard_combined_daily() %>%    
        mutate(date = dmy(paste(Day, Month, Year, sep="-"))) %>% 
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    })  
  })
  
  output$prm_std <- renderDataTable(datatable(upload_prm_standard_combined(), options = list(scrollX = TRUE)))
  output$seasonal_std <- renderDataTable(datatable(data_prm_standard_combined_seasonal(), options = list(scrollX = TRUE)))
  output$daily_std <- renderDataTable(datatable(data_prm_standard_combined_daily(), options = list(scrollX = TRUE)))
  
  
  ###################################################################################
  ##########plugin
  ###seasonal data
  ###read upload data files and combine all data into dataframe
  upload_data_combined <-
    reactive({
      # #require uploaded data files before evaluating
      # req(input$upload_data_files)
      # #check to make sure uploaded files has the correct extension .OUT, return error if not 
      # upload_data_files_ext <- str_detect(input$upload_data_files$name, "season\\.OUT$")
      # if(!any(upload_data_files_ext)){
      #   validate("Invalid input for seasonal data: season.OUT files needed")
      # }
      
      #set up parallel processing for future_map function
      plan(multisession, workers = 2) 
      
      #get a list of file paths from uploaded files
      req(input$upload_all_files)
      
      #wrap data processing within progress bar so when it runs the progress bar shows up
      withProgress(message = "Processing seasonal data", value = 0.7,{
        
        data.df = input$upload_all_files %>%
          #filter to read only seasonal.out files
          filter(str_detect(name, "season\\.OUT$")) %>% 
          #import dataset and clean up, format into dataframe
          mutate(dataset = future_map(datapath, function(datapath){
            #read in and clean up each data file from the list
            #modify file to convert variable number of spaces that separate values into tab so data can be loaded as tsv
            file.clean = read_file(paste0(datapath)) %>%
              #regex pattern to replace any number of space string before a non-space character (\\S) with a tab
              str_replace_all(" +?(?=\\S)","\t") 
            #format the cleaned data file into dataframe form
            #read in heading
            heading = read_lines(file = file.clean, skip = 2, n_max = 1) %>%
              str_split("\\t") %>%
              unlist() %>%
              #add last heading (parameter file (prm)) to the end of the list
              c(., "prm.file") 
            #read in data in tsv format, skipping heading lines, add headings from before
            data = read_tsv(file = file.clean, skip = 4, col_names = heading, show_col_types = FALSE) %>%
              select(-1) #remove blank column in the first position
          })) %>%
          unnest(dataset) %>%
          select(-size, -type, -datapath)
      })
      #return data.df as output
      data.df
    })
  #output datatable of the combined data
  output$upload_data_combined_display <- renderDataTable(upload_data_combined() %>%
                                                           select(name) %>%
                                                           distinct(),
                                                         options = list(scrollX = TRUE, pageLength = 5))
  
  ###read uploaded parameter files and combine
  upload_prm_combined <-
    reactive({
      # #require uploaded prm files before evaluating
      # req(input$upload_prm_files)
      # #check to make sure at least some uploaded files has the correct extension .prm, return error if not 
      # upload_prm_files_ext <- str_detect(input$upload_prm_files$name, "\\.PRM$")
      # if(!any(upload_prm_files_ext)){
      #   validate("Invalid input for parameter data: .PRM files needed")
      # }
      
      #set up parallel processing for future_map function
      plan(multisession, workers = 2) 
      
      #get a list of prm file path from uploaded
      req(input$upload_all_files)
      
      #wrap data processing within progress bar so when it runs the progress bar shows up
      withProgress(message = "Processing project files", value = 0.7,{
        
        prm.df = input$upload_all_files %>%
          #filter to read only .prm files
          filter(str_detect(name, "\\.PR[MO]$")) %>% 
          #read in each prm file from the list and extract parameter of interest
          mutate(parameter = future_map(datapath, function(datapath){
            #read in each prm file from the list
            prm.file = read_file(paste0(datapath))
            #extract parameter from each param file 
            #in this case we use str_extract to get parameters of the first time point (should be the same for all time points)
            #get climate file
            climate.file = str_extract(prm.file, ".+?\\.CLI") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.CLI)","")
            #get temperature file
            temperature.file = str_extract(prm.file, ".+?\\.((Tnx)|(TMP))") %>%
              unlist() %>%
              str_replace_all("(\\s)|((\\.Tnx)|(\\.TMP))","")    
            #get reference eto file
            reference.ET.file = str_extract(prm.file, ".+?\\.ETo") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.ETo)","")  
            #get rain  file
            rain.file = str_extract(prm.file, ".+?\\.PLU") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.PLU)","")  
            #get co2  file
            co2.file = str_extract(prm.file, ".+?\\.CO2") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.CO2)","")  
            #get crop
            crop.file = str_extract(prm.file, ".+?\\.CRO") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.CRO)","")  
            #get irrigation
            irrigation.file = str_extract(prm.file, ".+?\\.IRR") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.IRR)","")  
            #get field management
            field.management.file = str_extract(prm.file, ".+?\\.MAN") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.MAN)","")  
            #get soil
            soil.file = str_extract(prm.file, ".+?\\.SOL") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.SOL)","")  
            #get groundwater table
            groundwater.table.file = str_extract(prm.file, ".+?\\.GWT") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.GWT)","")  
            #get initial condition 
            initial.condition.file = str_extract(prm.file, ".+?\\.SW0") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.SW0)","")  
            #get offseason condition 
            offseason.condition.file = str_extract(prm.file, ".+?\\.OFF") %>%
              unlist() %>%
              str_replace_all("(\\s)|(\\.OFF)","")
            #start and end date of cropping period
            cropping.period.start = str_extract(prm.file, "(?<=First day of cropping period - )[[:digit:]]+ [[:alpha:]]+ [[:digit:]]+") %>%
              dmy() %>%
              map_chr(function(x){paste(day(x), month(x, label = T), sep = "_")})
            cropping.period.end = str_extract(prm.file, "(?<=Last day of cropping period - )[[:digit:]]+ [[:alpha:]]+ [[:digit:]]+") %>%
              dmy() %>%
              map_chr(function(x){paste(day(x), month(x, label = T), sep = "_")})
            #put parameters together in a table
            param.table = data.frame(climate.file, temperature.file, reference.ET.file, rain.file, co2.file, crop.file, irrigation.file, field.management.file, soil.file, groundwater.table.file, initial.condition.file, offseason.condition.file, cropping.period.start, cropping.period.end)
          })) %>%
          unnest(parameter) %>%
          select(-size, -type, -datapath) %>%
          mutate(irrigation.file = ifelse(is.na(irrigation.file), "rainfed", irrigation.file)) %>%
          mutate(name.variable = str_replace(name, "\\.PR[MO]$","")) %>%
          rename(prm.file.name = name)
        
      })
      
      #check name for _ delimiter to extract different variables out of file name and give number
      n.name.var = str_split(prm.df$name.variable,"_") %>%
        map(length) %>%
        unlist() %>%
        max()
      
      prm.df %>%
        separate(name.variable, into = paste0("name.variable",c(1:n.name.var)), sep = "_", remove = F)
    })
  #output datatable of the combined parameters
  output$upload_prm_combined_display <- renderDataTable(upload_prm_combined() %>%
                                                          select(prm.file.name) %>%
                                                          distinct(),
                                                        options = list(scrollX = TRUE, pageLength = 5))
  
  #give warning/notification of number of factors detected from file names
  upload_factors <- reactive({
    req(input$upload_all_files)
    length(colnames(upload_prm_combined())[str_detect(colnames(upload_prm_combined()), "name.variable[0-9]+")])
  })
  output$upload_warning <- renderText(paste0(upload_factors()," variables detected from file names (Multiple variables separated by underscores in file names are automatically detected)"))
  
  #option for renaming column name
  #create observe event module to monitor if user input select variable to rename
  #if variable selected, update the select input list for value choices of the selected variable
  upload_prm_combined_renamecol <- reactiveValues()
  
  observe({
    req(input$standard_vs_plugin_select)
    
    #select if standard or plugin mode used
    if(input$standard_vs_plugin_select == "GUI"){
      req(input$upload_data_files_standard)
      upload_prm_combined_renamecol$data <- upload_prm_standard_combined()
    }else{
      req(input$upload_all_files)
      upload_prm_combined_renamecol$data <- upload_prm_combined()
    }
  })
  
  observe({
    req(input$standard_vs_plugin_select)
    req(upload_prm_combined_renamecol$data)
    
    choices <- colnames(upload_prm_combined_renamecol$data)
    choices <- choices[! choices %in% c("name.variable")]
    updateSelectizeInput(inputId = "rename_col_from", choices = sort(choices)) 
  })
  #change value of selected variable to the value from user
  observeEvent(input$rename_col_button, {
    if(input$rename_col_to != "" & !input$rename_col_to %in% colnames(upload_prm_combined_renamecol$data)){
      rename_df <- upload_prm_combined_renamecol$data
      #change colname
      colnames(rename_df)[which(colnames(rename_df) == input$rename_col_from)] = input$rename_col_to
      upload_prm_combined_renamecol$data <- rename_df
    }
    choices <- colnames(upload_prm_combined_renamecol$data)
    choices <- choices[! choices %in% c("name.variable")]
    updateSelectizeInput(inputId = "rename_col_from", choices = sort(choices)) 
  })
  
  #output datatable of the combined parameters
  output$prm_combined_display <- renderDataTable(datatable(tryCatch(error = function(cnd) NULL,
                                                                    upload_prm_combined_renamecol$data %>%
                                                                      select(prm.file.name, matches("name.variable[0-9]"), everything())), 
                                                           options = list(scrollX = TRUE, pageLength = 5)
  ))
  #for downloading combined prm
  output$download_combined_prm <- downloadHandler(
    filename = "combined_parameter_data.tsv",
    content = function(file) {
      write_tsv(upload_prm_combined_renamecol$data, file)
    }
  )
  
  ####add parameters to the output dataset
  data_prm_combined <- reactiveValues()
  observe({
    req(input$standard_vs_plugin_select)
    
    #select if standard or plugin mode used
    if(input$standard_vs_plugin_select == "GUI"){
      req(input$upload_data_files_standard)
      data_prm_combined$data <- data_prm_standard_combined_seasonal()
    }else{
      req(input$upload_all_files)
      data_prm_combined$data <- upload_data_combined() %>%
        # mutate(sowing.dmy = dmy(paste(Day1, Month1, Year1, sep="-"))) %>%
        # mutate(sowing.date = paste(day(sowing.dmy), month(sowing.dmy, label = T), sep = "_")) %>%
        mutate(name.variable = str_replace(name, "PRMseason.OUT$","")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    }
  })
  
  
  #output datatable of the combined data and parameters
  output$data_prm_combined_display <- renderDataTable(datatable(tryCatch(error = function(cnd) NULL,
                                                                         data_prm_combined$data  %>%
                                                                           select(prm.file.name, matches("name.variable[0-9]"), everything())), 
                                                                options = list(scrollX = TRUE, pageLength = 5)
  ))
  #for downloading combined dataset
  output$download_combined_dataset <- downloadHandler(
    filename = "combined_seasonal_data.tsv",
    content = function(file) {
      write_tsv(data_prm_combined$data, file)
    }
  )
  
  ###daily data
  #first check if daily data are uploaded
  daily_upload_check <- reactive({
    req(input$upload_all_files)
    any(str_detect(input$upload_all_files$name, "day\\.OUT$"))
  })
  
  #
  upload_daily_data_combined <-
    reactive({
      # #require uploaded data files before evaluating
      # req(input$upload_daily_data_files)
      # #check to make sure uploaded files has the correct extension .OUT, return error if not 
      # upload_daily_data_files_ext <- str_detect(input$upload_daily_data_files$name, "day\\.OUT$")
      # if(!any(upload_daily_data_files_ext)){
      #   validate("Invalid input for daily data: day.OUT files needed")
      # }
      
      #set up parallel processing for future_map function
      plan(multisession, workers = 2) 
      
      #get a list of file paths from uploaded files
      req(input$upload_all_files)
      
      #check if daily data uploaded, only run if daily data were uploaded
      req(daily_upload_check())
      
      #wrap data processing within progress bar so when it runs the progress bar shows up
      withProgress(message = "Processing daily data", value = 0.7,{
        
        data.df = input$upload_all_files %>%
          #filter to read only day.out files
          filter(str_detect(name, "day\\.OUT$")) %>% 
          #import dataset and clean up, format into dataframe
          mutate(dataset = future_map2(datapath, name, function(datapath, name){
            ###read in one output file for daily data 
            #read in data as lines, clean up spaces to allow reading as tsv
            file.clean = read_lines(datapath) %>%
              str_replace_all(" +?(?=\\S)","\t") 
            
            if(str_detect(name, "PRMday\\.OUT")){
              ###for .PRM files
              #read heading, line 4
              heading = file.clean[4] %>%
                str_replace_all("(?<=WC|ECe)\\t(?=[2-9])", "0") %>%
                str_split("\\t") %>%
                unlist() 
              
              #find index of blank lines that separate sections, different Runs (seasons)
              file.line.blank = which(file.clean == "") 
              #find index of lines that are not data, starts from every blank line and 3 consecutive following lines, also remove first line
              file.line.remove = c(1, file.line.blank, file.line.blank+1, file.line.blank+2, file.line.blank+3)
              #recreate data file, remove unwanted lines 
              file.clean = file.clean[-file.line.remove] %>%
                paste0(collapse = "\n") 
            } else{
              ###for .PRO files
              #read heading, line 4
              heading = file.clean[3] %>%
                str_replace_all("(?<=WC|ECe)\\t(?=[2-9])", "0") %>%
                str_split("\\t") %>%
                unlist() 
              
              #find index of blank lines that separate sections, different Runs (seasons)
              file.line.blank = which(file.clean == "") 
              #find index of lines that are not data, starts from every blank line and 2 consecutive following lines, also remove first line
              file.line.remove = c(1, file.line.blank, file.line.blank+1, file.line.blank+2)
              #recreate data file, remove unwanted lines 
              file.clean = file.clean[-file.line.remove] %>%
                paste0(collapse = "\n") 
            }
            
            #read in data as tsv
            data = read_tsv(file = file.clean, col_names = heading, show_col_types = FALSE) %>%
              select(-1) #remove blank column at the start
            
            #remove duplicated column
            colnames(data) = str_replace(colnames(data),"\\.\\.\\..+$","")
            data = data[!duplicated(colnames(data))]
            
          })) %>%
          unnest(dataset) %>%
          select(-size, -type, -datapath) %>%
          mutate(Stage = as.factor(Stage))
      })
      #return data.df as output
      data.df
    })
  
  #output datatable of the combined data
  output$upload_daily_data_combined_display <- renderDataTable(upload_daily_data_combined() %>%
                                                                 select(name) %>%
                                                                 distinct(),
                                                               options = list(scrollX = TRUE, pageLength = 5))
  
  
  
  ####add parameters to the daily dataset
  daily_data_prm_combined <- reactiveValues()
  observe({
    req(input$standard_vs_plugin_select)
    #select if standard or plugin mode used
    if(input$standard_vs_plugin_select == "GUI"){
      req(input$upload_data_files_standard)
      daily_data_prm_combined$data <- data_prm_standard_combined_daily()
    }else{
      req(input$upload_all_files)
      daily_data_prm_combined$data <- upload_daily_data_combined() %>%    
        mutate(date = dmy(paste(Day, Month, Year, sep="-"))) %>% 
        mutate(name.variable = str_replace(name, "PRMday.OUT$","")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    }
  })
  
  
  #if no daily data uploaded, reset the dataframe to NULL
  observe({
    req(input$standard_vs_plugin_select)
    req(input$upload_all_files)
    if(daily_upload_check() == FALSE){
      daily_data_prm_combined$data <- NULL
    }
  })
  
  #output datatable of the combined daily data and parameters
  output$daily_data_prm_combined_display <- renderDataTable(datatable(tryCatch(error = function(cnd) NULL,
                                                                               daily_data_prm_combined$data  %>%
                                                                                 select(prm.file.name, matches("name.variable[0-9]"), everything())), 
                                                                      options = list(scrollX = TRUE, pageLength = 5)
  ))
  #for downloading combined daily dataset
  output$download_combined_daily_dataset <- downloadHandler(
    filename = "combined_daily_data.tsv",
    content = function(file) {
      write_tsv(daily_data_prm_combined$data, file)
    }
  )
  
  ###check if all parameter .PRM files are uploaded as needed for all .OUT datasets
  #find a list of any missing prm file required in the data files
  missing_prm_file <- reactive({
    req(input$upload_all_files)
    
    bind_rows(upload_data_combined() %>%
                mutate(name.variable = str_replace(name, "PR[MO]season.OUT$","")) %>%
                select(name.variable), 
              upload_daily_data_combined() %>%
                mutate(name.variable = str_replace(name, "PR[MO]day.OUT$","")) %>%
                select(name.variable)) %>%
      distinct() %>%
      anti_join(upload_prm_combined(), by = "name.variable") %>%
      rename(`Missing files not uploaded`= name.variable)
  })
  #return error if there is any missing prm files
  output$missing_prm_file_error <- renderDataTable(
    if(nrow(missing_prm_file()) > 0){
      datatable(missing_prm_file(), options = list(scrollX = TRUE, pageLength = 5))
    })
  
  ### filter data
  #update choices of columns to filter
  observe({
    req(input$standard_vs_plugin_select)
    req(input$upload_all_files)
    
    choices <- c(colnames(upload_prm_combined_renamecol$data),"Year")
    if("Stage" %in% colnames(data_prm_combined$data)){
      choices <- c(choices, "Stage")
    }
    updateSelectizeInput(inputId = "filter_data_column", choices = sort(choices)) 
  })
  #update variables to select to keep
  observeEvent(input$filter_data_column, {
    req(input$standard_vs_plugin_select)
    req(input$upload_all_files)
    
    if(input$filter_data_column == "Year"){ 
      min = min(data_prm_combined$data[["Year1"]])
      max = max(data_prm_combined$data[["Year1"]])
      updateSliderInput(inputId = "filter_data_num", min = min, max = max, value = c(min,max), step = 1) 
    }else{
      choices <- unique(upload_prm_combined_renamecol$data[[input$filter_data_column]])
      updateSelectizeInput(inputId = "filter_data_chr", choices = sort(choices)) 
    }
  })
  #filter data
  observeEvent(input$filter_data_button,{
    req(input$filter_data_column)
    if(input$filter_data_column == "Year"){ 
      daily_data_prm_combined$data <- daily_data_prm_combined$data %>%
        filter(Year >= as.numeric(input$filter_data_num[1]) & Year <= as.numeric(input$filter_data_num[2]))
      data_prm_combined$data <- data_prm_combined$data %>%
        filter(Year1 >= as.numeric(input$filter_data_num[1]) & Year1 <= as.numeric(input$filter_data_num[2]))
    }else{
      req(input$filter_data_chr)
      daily_data_prm_combined$data <- daily_data_prm_combined$data %>%
        filter(.data[[input$filter_data_column]] %in% input$filter_data_chr)
      data_prm_combined$data <- data_prm_combined$data %>%
        filter(.data[[input$filter_data_column]] %in% input$filter_data_chr)
    }
  })
  ##reset data to original before filter
  observeEvent(input$filter_reset_button | input$historical_reset_button,{
    #reset daily data to the original processing from uploads
    req(input$standard_vs_plugin_select)
    #select if standard or plugin mode used
    if(input$standard_vs_plugin_select == "GUI"){
      req(input$upload_data_files_standard)
      daily_data_prm_combined$data <- data_prm_standard_combined_daily()
    }else{
      req(input$upload_all_files)
      daily_data_prm_combined$data <- upload_daily_data_combined() %>%    
        mutate(date = dmy(paste(Day, Month, Year, sep="-"))) %>% 
        mutate(name.variable = str_replace(name, "PRMday.OUT$","")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    }
    #reset seasonal data to the original processing from uploads
    req(input$standard_vs_plugin_select)
    #select if standard or plugin mode used
    if(input$standard_vs_plugin_select == "GUI"){
      req(input$upload_data_files_standard)
      data_prm_combined$data <- data_prm_standard_combined_seasonal()
    }else{
      req(input$upload_all_files)
      data_prm_combined$data <- upload_data_combined() %>%
        # mutate(sowing.dmy = dmy(paste(Day1, Month1, Year1, sep="-"))) %>%
        # mutate(sowing.date = paste(day(sowing.dmy), month(sowing.dmy, label = T), sep = "_")) %>%
        mutate(name.variable = str_replace(name, "PRMseason.OUT$","")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    }
  })
  
  ###label historical data by year
  #show box when action button pressed
  observeEvent(input$advanced_button, {
    shinyjs::toggle(id = "advanced_box")
  })
  
  #update choices of columns to add label to
  observe({
    req(input$standard_vs_plugin_select)
    req(input$upload_all_files)
    choices <- colnames(upload_prm_combined_renamecol$data)
    updateSelectizeInput(inputId = "historical_column", choices = sort(choices)) 
  })
  #update year range to select for labeling
  observeEvent(input$historical_column, {
    req(input$standard_vs_plugin_select)
    req(input$upload_all_files)
    min = min(data_prm_combined$data[["Year1"]])
    max = max(data_prm_combined$data[["Year1"]])
    updateSliderInput(inputId = "historical_year", min = min, max = max, value = c(min,max), step = 1) 
  })
  #add label to historical data
  observeEvent(input$historical_button, {
    req(input$standard_vs_plugin_select)
    req(input$upload_all_files)
    req(input$historical_text)
    daily_data_prm_combined$data <- daily_data_prm_combined$data %>%
      mutate(!!input$historical_column := ifelse(Year >= as.numeric(input$historical_year[1]) & Year <= as.numeric(input$historical_year[2]), input$historical_text, .data[[input$historical_column]]))
    data_prm_combined$data <- data_prm_combined$data %>%
      mutate(!!input$historical_column := ifelse(Year1 >= as.numeric(input$historical_year[1]) & Year1 <= as.numeric(input$historical_year[2]), input$historical_text, .data[[input$historical_column]]))
  })
  
  
  ############### ggplot ###############
  #make a reactive value to monitor plot visibility
  plot_visibility <- reactiveValues()
  observe({plot_visibility$value <- FALSE})
  
  #reactive for showing next boxes to input plotting instructions
  observeEvent(input$plot_next1, {
    shinyjs::show(id = "hiddenbox4")
    plot_visibility$value = TRUE
  })
  observeEvent(input$plot_next1, {
    shinyjs::show(id = "hiddenbox2")
  })
  observeEvent(input$plot_next1, {
    shinyjs::show(id = "hiddenbox3")
  })
  observeEvent(input$plot_next5, {
    shinyjs::toggle(id = "hiddenbox5")
  })
  
  ###data for plotting
  
  #update options for plot mode, if daily data not uploaded, remove daily option for plot mode
  observe({
    req(input$upload_all_files)
    if(daily_upload_check() == FALSE){
      updateSelectizeInput(inputId = "plot_mode", choices = c("seasonal"))
    }else{
      updateSelectizeInput(inputId = "plot_mode", choices = c("daily","seasonal"))
    }
  })
  
  #observe event to toggle plot visibility state on/off when plot mode changes
  observeEvent(input$plot_mode, ignoreInit = TRUE, ignoreNULL = TRUE, {
    plot_visibility$value = FALSE
  })
  
  ##select data mode daily or seasonal  
  data_mode_selected <- reactive({
    req(input$plot_mode)
    
    if(input$plot_mode == "daily"){
      req(daily_data_prm_combined$data)
      #update choices for plotting axis
      axis.choices = unique(colnames(daily_data_prm_combined$data))
      updateSelectInput(inputId = "y_var", choices = sort(axis.choices))
      updateSelectInput(inputId = "x_var", choices = sort(axis.choices))
      #update choices for grouping variable
      group.choices <- if(input$standard_vs_plugin_select == "plugin"){
        setdiff(colnames(daily_data_prm_combined$data), colnames(upload_daily_data_combined()))
      } else{
        setdiff(colnames(daily_data_prm_combined$data), colnames(upload_data_standard_combined_daily()))
      }
      
      group.choices <- c(group.choices, "Stage", "Year","Month")
      group.choices <- setdiff(group.choices, "date")
      updateSelectizeInput(inputId = "col_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "shape_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "linetype_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "facet_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "facet_var2", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "group_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "rename_variable", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "reorder_variable", choices = sort(group.choices)) 
      #return data to use
      daily_data_prm_combined$data
    }else{
      req(data_prm_combined$data)
      #update choices for plotting axis
      axis.choices = unique(colnames(data_prm_combined$data))
      updateSelectInput(inputId = "y_var", choices = sort(axis.choices))
      updateSelectInput(inputId = "x_var", choices = sort(axis.choices))
      #update choices for grouping variable
      group.choices <- if(input$standard_vs_plugin_select == "plugin"){
        setdiff(colnames(data_prm_combined$data), colnames(upload_data_combined()))
      } else{
        setdiff(colnames(data_prm_combined$data), colnames(upload_data_standard_combined_seasonal()))
      }
      
      if("Stage" %in% colnames(data_prm_combined$data)){
        group.choices <- c(group.choices, "Stage")
      }
      group.choices <- group.choices[which(!str_detect(group.choices, "\\.duration\\."))]
      
      updateSelectizeInput(inputId = "col_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "shape_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "linetype_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "facet_var", choices = sort(group.choices))
      updateSelectizeInput(inputId = "facet_var2", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "group_var", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "rename_variable", choices = sort(group.choices)) 
      updateSelectizeInput(inputId = "reorder_variable", choices = sort(group.choices)) 
      #return data to use
      data_prm_combined$data   
    }
  })
  
  
  delay(10000, 
        observe({
          req(input$plot_mode)
          req(data_prm_combined_plot_rename$data)
          
          #update choices for plotting axis
          axis.choices = unique(colnames(data_prm_combined_plot_rename$data))
          updateSelectInput(inputId = "y_var", choices = sort(axis.choices), selected = plot_var_select_cache$y_var)
          updateSelectInput(inputId = "x_var", choices = sort(axis.choices), selected = plot_var_select_cache$x_var)
          #update choices for grouping variable
          if(input$plot_mode == "daily"){
            group.choices <- if(input$standard_vs_plugin_select == "plugin"){
              setdiff(colnames(data_prm_combined_plot_rename$data), colnames(upload_daily_data_combined()))
            } else{
              setdiff(colnames(data_prm_combined_plot_rename$data), colnames(upload_data_standard_combined_daily()))
            }
            group.choices <- c(group.choices, "Stage", "Year","Month")
            group.choices <- setdiff(group.choices, "date")
          }else{
            group.choices <- if(input$standard_vs_plugin_select == "plugin"){
              setdiff(colnames(data_prm_combined_plot_rename$data), colnames(upload_data_combined()))
            } else{
              setdiff(colnames(data_prm_combined_plot_rename$data), colnames(upload_data_standard_combined_seasonal()))
            }
            
            if("Stage" %in% colnames(data_prm_combined$data)){
              group.choices <- c(group.choices, "Stage")
            }
            group.choices <- group.choices[which(!str_detect(group.choices, "\\.duration\\."))]
          }
          updateSelectizeInput(inputId = "col_var", choices = sort(group.choices), selected = plot_var_select_cache$col_var)
          updateSelectizeInput(inputId = "shape_var", choices = sort(group.choices), selected = plot_var_select_cache$shape_var)
          updateSelectizeInput(inputId = "linetype_var", choices = sort(group.choices), selected = plot_var_select_cache$linetype_var)
          updateSelectizeInput(inputId = "facet_var", choices = sort(group.choices), selected = plot_var_select_cache$facet_var)
          updateSelectizeInput(inputId = "facet_var2", choices = sort(group.choices), selected = plot_var_select_cache$facet_var2)
          updateSelectizeInput(inputId = "rename_variable", choices = sort(group.choices))
          updateSelectizeInput(inputId = "reorder_variable", choices = sort(group.choices))
        })
  )
  
  observe({
    updateSliderInput(inputId = "x_var_range", value = c(plot_var_select_cache$x_var_range1, plot_var_select_cache$x_var_range2))
    
  })
  
  ## if plotting mean is selected, calculate mean based on grouping variable selected
  data_prm_combined_plot <- reactive({
    req(input$plot_mode)
    req(data_mode_selected())
    
    if(input$use_mean == "mean"){
      tryCatch(error = function(cnd) data_mode_selected(),
               data_mode_selected() %>%
                 group_by(across(all_of(c(input$x_var, input$col_var,input$shape_var,input$linetype_var, input$facet_var, input$facet_var2, input$group_var)))) %>%
                 mutate(across(where(is.numeric),  ~ mean(.x, na.rm = TRUE))) %>%
                 ungroup() 
      )
    }else if(input$use_mean == "sum"){
      tryCatch(error = function(cnd) data_mode_selected(),
               data_mode_selected() %>%
                 group_by(across(all_of(c(input$x_var, input$col_var,input$shape_var,input$linetype_var, input$facet_var, input$facet_var2, input$group_var)))) %>%
                 mutate(across(where(is.numeric),  ~ sum(.x, na.rm = TRUE))) %>%
                 ungroup()
      )
    }else if(input$use_mean == "median"){
      tryCatch(error = function(cnd) data_mode_selected(),
               data_mode_selected() %>%
                 group_by(across(all_of(c(input$x_var, input$col_var,input$shape_var,input$linetype_var, input$facet_var, input$facet_var2, input$group_var)))) %>%
                 mutate(across(where(is.numeric),  ~ median(.x, na.rm = TRUE))) %>%
                 ungroup()
      )
    }else if(input$use_mean == "max"){
      tryCatch(error = function(cnd) data_mode_selected(),
               data_mode_selected() %>%
                 group_by(across(all_of(c(input$x_var, input$col_var,input$shape_var,input$linetype_var, input$facet_var, input$facet_var2, input$group_var)))) %>%
                 mutate(across(where(is.numeric),  ~ max(.x, na.rm = TRUE))) %>%
                 ungroup()
      )
    }else if(input$use_mean == "min"){
      tryCatch(error = function(cnd) data_mode_selected(),
               data_mode_selected() %>%
                 group_by(across(all_of(c(input$x_var, input$col_var,input$shape_var,input$linetype_var, input$facet_var, input$facet_var2, input$group_var)))) %>%
                 mutate(across(where(is.numeric),  ~ min(.x, na.rm = TRUE))) %>%
                 ungroup()
      )
    }else{
      data_mode_selected()
    }
  })
  
  ###option for renaming variable
  #create observe event module to monitor if user input select variable to rename
  #if variable selected, update the select input list for value choices of the selected variable
  observeEvent(input$rename_variable, {
    choices <- unique(data_prm_combined_plot_rename$data[[input$rename_variable]])
    updateSelectInput(inputId = "rename_from", choices = sort(choices)) 
  })
  #change value of selected variable to the value from user
  data_prm_combined_plot_rename <- reactiveValues()
  observe({data_prm_combined_plot_rename$data <- data_prm_combined_plot()})
  observeEvent(input$rename_button, {
    if(input$rename_to != ""){
      rename_df <- data_prm_combined_plot_rename$data
      rename_df[input$rename_variable][rename_df[input$rename_variable] == input$rename_from] <- input$rename_to
      data_prm_combined_plot_rename$data <- rename_df
    }
    choices <- unique(data_prm_combined_plot_rename$data[[input$rename_variable]])
    updateSelectInput(inputId = "rename_from", choices = sort(choices)) 
  })
  
  ###option for reordering variable
  #create observe event module to monitor if user input select variable to reorder
  #if variable selected, update the select input list for value choices of the selected variable
  observeEvent(input$reorder_variable, {
    choices <- unique(data_prm_combined_plot_rename$data[[input$reorder_variable]])
    updateSelectInput(inputId = "reorder_order", choices = sort(choices)) 
  })
  #change value of selected variable to the value from user
  observeEvent(input$reorder_button, {
    req(input$reorder_variable, input$reorder_order)
    new_order = c(input$reorder_order, setdiff(unique(data_prm_combined_plot_rename$data[[input$reorder_variable]]),input$reorder_order))
    
    reorder_df <- data_prm_combined_plot_rename$data
    reorder_df[[input$reorder_variable]] <- factor(reorder_df[[input$reorder_variable]], levels = new_order)
    data_prm_combined_plot_rename$data <- reorder_df
  })
  
  #remember variable set for plotting, so can be recovered after making change to dataframe and plotting engine reactively update, reinitialise
  plot_var_select_cache <- reactiveValues() 
  
  #record everytime that the value change, keep only the most recent previous value and current value
  observeEvent(input$y_var, ignoreNULL = F,{
    plot_var_select_cache$y_var <- input$y_var
  })
  observeEvent(input$x_var, ignoreNULL = F,{
    plot_var_select_cache$x_var <- input$x_var
  })
  observeEvent(input$col_var, ignoreNULL = F,{
    plot_var_select_cache$col_var <- input$col_var
  })
  observeEvent(input$shape_var, ignoreNULL = F,{
    plot_var_select_cache$shape_var <- input$shape_var
  })
  observeEvent(input$linetype_var, ignoreNULL = F,{
    plot_var_select_cache$linetype_var <- input$linetype_var
  })
  observeEvent(input$facet_var, ignoreNULL = F,{
    plot_var_select_cache$facet_var <- input$facet_var
  })
  observeEvent(input$facet_var2, ignoreNULL = F,{
    plot_var_select_cache$facet_var2 <- input$facet_var2
  })
  
  observeEvent(input$x_var_range, ignoreNULL = F,{
    plot_var_select_cache$x_var_range1 <- input$x_var_range[1]
    plot_var_select_cache$x_var_range2 <- input$x_var_range[2]
  })
  
  ###set y and x axis range in plot
  observeEvent(input$y_var, {
    if(is.numeric(data_prm_combined_plot_rename$data[[input$y_var]])){
      min = min(data_prm_combined_plot_rename$data[[input$y_var]])
      max = max(data_prm_combined_plot_rename$data[[input$y_var]])
      updateSliderInput(inputId = "y_var_range", min = 0, max = ceiling(max*1.5), value = c(min,max)) 
    }
  })
  observeEvent(input$x_var, {
    if(is.numeric(data_prm_combined_plot_rename$data[[input$x_var]])){
      min = min(data_prm_combined_plot_rename$data[[input$x_var]])
      max = max(data_prm_combined_plot_rename$data[[input$x_var]])
      if(str_detect(input$x_var, "[y,Y]ear")){
        updateSliderInput(inputId = "x_var_range", min = min, max = max, value = c(min,max), step =1) 
      } else{
        updateSliderInput(inputId = "x_var_range", min = 0, max = ceiling(max*1.5), value = c(min,max)) 
      }
    }
  })
  
  ###update axis range once summary statistics are used
  observeEvent(data_prm_combined_plot_rename$data, {
    req(data_prm_combined_plot_rename$data, input$y_var, input$x_var)
    if(is.numeric(data_prm_combined_plot_rename$data[[input$y_var]])){
      min = min(data_prm_combined_plot_rename$data[[input$y_var]])
      max = max(data_prm_combined_plot_rename$data[[input$y_var]])
      updateSliderInput(inputId = "y_var_range", min = 0, max = ceiling(max*1.5), value = c(min,max)) 
    }
    
    if(is.numeric(data_prm_combined_plot_rename$data[[input$x_var]])){
      min = min(data_prm_combined_plot_rename$data[[input$x_var]])
      max = max(data_prm_combined_plot_rename$data[[input$x_var]])
      if(str_detect(input$x_var, "[y,Y]ear")){
        updateSliderInput(inputId = "x_var_range", min = min, max = max, value = c(min,max), step =1) 
      } else{
        updateSliderInput(inputId = "x_var_range", min = 0, max = ceiling(max*1.5), value = c(min,max)) 
      }
    }
  })
  
  #set color palette
  custom_palette <- reactive({
    default_palette <- c("#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
    
    #vector of available color choices to form custom palette
    color_choice_hex <- c("#000000","#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E41A1C","#A6D854")
    names(color_choice_hex) <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
    
    #make palette from custom colors selected from user
    if(length(input$col_palette) > 0){
      palette <- c(color_choice_hex[input$col_palette], setdiff(default_palette, color_choice_hex[input$col_palette]))
    }else{
      palette <- default_palette
    }
    unname(palette)
  })
  
  #set shape palette
  custom_shape <- reactive({
    default_shape <- c(16,17,15,18,4,1,2,0,5)
    
    #vector of available shape choices to form custom palette
    shape_choice <- c(16,17,15,18,4,1,2,0,5)
    names(shape_choice) <- c("circle", "triangle", "rectangle", "diamond", "cross", "hollow_circle", "hollow_triangle", "hollow_rectangle", "hollow_diamond")
    
    
    #make palette from custom shapes selected from user
    if(length(input$shape_palette) > 0){
      shape.palette <- c(shape_choice[input$shape_palette], setdiff(default_shape, shape_choice[input$shape_palette]))
    }else{
      shape.palette <- default_shape
    }
    unname(as.numeric(shape.palette)) 
  })
  
  #set linetype palette
  custom_linetype <- reactive({
    default_linetype <- c(1:6)
    
    #vector of available shape choices to form custom palette
    linetype_choice <- c(1:6)
    names(linetype_choice) <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    
    
    #make palette from custom shapes selected from user
    if(length(input$linetype_palette) > 0){
      linetype.palette <- c(linetype_choice[input$linetype_palette], setdiff(default_linetype, linetype_choice[input$linetype_palette]))
    }else{
      linetype.palette <- default_linetype
    }
    unname(as.numeric(linetype.palette)) 
  })
  
  #set legend direction
  legend_direction <- reactive({
    if(input$legend_position %in% c("top","bottom")){
      "horizontal"
    } else{
      "vertical"
    }
  })
  
  ggplot_plugin <- reactive({
    req(data_prm_combined_plot_rename$data)
    showModal(modalDialog(title = "Processing data...", "Please wait while the data is being processed.", easyClose = FALSE, footer = NULL))
    
    #initial plot according to selected coloring and group variable
    if(length(input$shape_var) > 0 & length(input$col_var) > 0 & length(input$linetype_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = interaction(.data[[input$shape_var]], .data[[input$col_var]], .data[[input$linetype_var]]), col = .data[[input$col_var]], fill = .data[[input$col_var]], shape = .data[[input$shape_var]], linetype = .data[[input$linetype_var]]))
    }
    else if(length(input$shape_var) > 0 & length(input$col_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = interaction(.data[[input$shape_var]], .data[[input$col_var]]), col = .data[[input$col_var]], fill = .data[[input$col_var]], shape = .data[[input$shape_var]]))
    }  
    else if(length(input$shape_var) > 0 & length(input$linetype_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = interaction(.data[[input$shape_var]], .data[[input$linetype_var]]), shape = .data[[input$shape_var]], linetype = .data[[input$linetype_var]]))
    }
    else if(length(input$col_var) > 0 & length(input$linetype_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = interaction(.data[[input$col_var]], .data[[input$linetype_var]]), col = .data[[input$col_var]], fill = .data[[input$col_var]], linetype = .data[[input$linetype_var]]))
    }
    else if(length(input$col_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]], fill = .data[[input$col_var]]))
    }
    else if(length(input$shape_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$shape_var]], shape = .data[[input$shape_var]]))
    }
    else if(length(input$linetype_var) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$linetype_var]], linetype = .data[[input$linetype_var]]))
    }        
    else{
      p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]]))
    }
    #add plot
    p <- p +
      theme(axis.title = element_text(size = as.numeric(input$font_size_axis_title)), 
            axis.text = element_text(size = as.numeric(input$font_size_axis_text)),
            #legend.title = element_text(size = as.numeric(input$font_size_legend)),
            legend.title = element_text(size = as.numeric(input$font_size_legend), face = "bold"),
            legend.text = element_text(size = as.numeric(input$font_size_legend)),
            strip.text = element_text(size = as.numeric(input$font_size_facet)),
            plot.title = element_text(size = as.numeric(input$font_size_plot_title)),
            legend.position = paste(input$legend_position),
            legend.direction = paste(legend_direction()),
            panel.background = element_rect(colour = "black", fill = "white"),
            plot.background = element_rect(colour = NA, fill = "white"),
            axis.line = element_line(colour="black",size=0.1),
            axis.ticks = element_line(),
            axis.title.x = element_text(vjust = -2.5, face = "bold"),
            axis.title.y = element_text(vjust = +2.5, face="bold"),
            legend.key = element_rect(colour = NA, fill = NA),
            #legend.key.size= unit(0.75, "cm"),
            strip.background=element_rect(colour="black",fill="grey80"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            legend.box = "vertical"
      ) +
      scale_color_manual(values=custom_palette()) +
      scale_shape_manual(values=custom_shape()) +
      scale_linetype_manual(values = custom_linetype())+
      #guides(color = guide_legend(override.aes = list(size=1)))
      guides(color = guide_legend(keywidth = 5, keyheight = 3),
             fill = guide_legend(keywidth = 5, keyheight = 3),
             linetype = guide_legend(keywidth = 5, keyheight = 3),
             shape = guide_legend(keywidth = 5, keyheight = 5)
      )
    
    #pretty scales for numeric variable
    if(is.numeric(data_prm_combined_plot_rename$data[[input$x_var]])){
      p <- p +
        scale_x_continuous(breaks = breaks_pretty(), limits = c(as.numeric(input$x_var_range[1]),as.numeric(input$x_var_range[2])))
    }
    if(is.numeric(data_prm_combined_plot_rename$data[[input$y_var]])){
      p <- p +
        scale_y_continuous(breaks = breaks_pretty(), limits = c(as.numeric(input$y_var_range[1]),as.numeric(input$y_var_range[2])))
    }
    if(is.Date(data_prm_combined_plot_rename$data[[input$x_var]])){
      p <- p +
        scale_x_date(breaks = breaks_pretty())
    }
    if(is.Date(data_prm_combined_plot_rename$data[[input$y_var]])){
      p <- p +
        scale_y_date(breaks = breaks_pretty())
    }
    
    #show legend title
    if(input$show_legend_title == "no"){
      p <-  p + theme(legend.title = element_blank())
    }
    
    #x axis text angle
    if(as.numeric(input$x_axis_label_angle) > 0){
      p <- p + theme(axis.text.x = element_text(angle = as.numeric(input$x_axis_label_angle), hjust = 1, vjust = 1))
    }
    
    #select facet variable
    if(length(input$facet_var) == 1 & length(input$facet_var2) == 1){
      p <- p + facet_grid(get(input$facet_var2)~get(input$facet_var))
    } else if(length(input$facet_var) == 1){
      p <- p + facet_grid(.~get(input$facet_var))
    } else if(length(input$facet_var2) == 1){
      p <- p + facet_grid(get(input$facet_var2)~.)
    } else {
      p <- p
    }
    
    #select plotting elements (geom)
    if("point" %in% input$plot_element){
      p <- p + geom_point(size = as.numeric(input$point_size), alpha = as.numeric(input$point_transparency), stat = "unique")
    }     
    if("line" %in% input$plot_element){
      p <- p + geom_line(size = as.numeric(input$line_size), alpha = as.numeric(input$line_transparency))
    }
    if("linear_trend_error" %in% input$plot_element){
      p <- p + geom_smooth(method="lm", se = T, show.legend = FALSE, size = as.numeric(input$line_size))
    }
    if("linear_trend" %in% input$plot_element & !("linear_trend_error" %in% input$plot_element)){
      p <- p + stat_smooth(geom="line", method="lm", se = F, show.legend = T, size = as.numeric(input$line_size), alpha = as.numeric(input$line_transparency))
    }
    if("loess_smooth_trend_error" %in% input$plot_element){
      p <- p + geom_smooth(method="loess", se = T, show.legend = FALSE, size = as.numeric(input$line_size))
    }
    if("loess_smooth_trend" %in% input$plot_element & !("loess_smooth_trend_error" %in% input$plot_element)){
      p <- p + stat_smooth(geom="line", method="loess", se = F, show.legend = T, size = as.numeric(input$line_size), alpha = as.numeric(input$line_transparency))
    }
    if("background_grid" %in% input$plot_element){
      p <- p + theme(panel.grid.major = element_line(colour="#f0f0f0"),
                     panel.grid.minor = element_blank())
    }else{
      p <- p + theme(panel.grid.major = element_blank())
    }
    if(length(input$plot_element) == 0){
      p <- p
    }
    
    #add custom text for axis label
    if(nchar(input$y_var_label) > 0){
      p <- p + labs(y = paste(input$y_var_label))
    }
    if(nchar(input$x_var_label) > 0){
      p <- p + labs(x = paste(input$x_var_label))
    }
    if(nchar(input$title_label) > 0){
      p <- p + labs(title = paste(input$title_label))
    }
    #reset custom title when axis choice changes
    observeEvent(input$x_var,ignoreInit = T, {
      updateTextInput(inputId = "x_var_label", value = "")
    })
    observeEvent(input$y_var, ignoreInit = T, {
      updateTextInput(inputId = "y_var_label", value = "")
    })
    
    removeModal()
    
    #plot
    print(p)
    
    
  })
  
  #adjust default plot size according to facets
  #select facet variable
  observeEvent(input$facet_var, {
    if(length(input$facet_var) == 1){
      if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var]])) == 1){
        updateTextInput(session, "export_plot_width", value = "19")
      }
      if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var]])) == 2){
        updateTextInput(session, "export_plot_width", value = "29")
      }
      if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var]])) > 2){
        updateTextInput(session, "export_plot_width", value = "39")
      }
    }
  })
  observeEvent(input$facet_var2, {
    if(length(input$facet_var2) == 1){
      if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var2]])) == 1){
        updateTextInput(session,"export_plot_height", value = "12")
      }
      if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var2]])) == 2){
        updateTextInput(session,"export_plot_height", value = "22")
      }
      if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var2]])) > 2){
        updateTextInput(session,"export_plot_height", value = "32")
      }
    }
  })
  
  
  #render ggplot display in app
  output$ggplot_plugin_display <- renderPlot({
    if(isTRUE(plot_visibility$value)){
      if(!is_null(input$x_var) & !is_null(input$y_var)){
        ggplot_plugin()
      }
    }
  },width=exprToFunction(as.numeric(input$export_plot_width)*36), height=exprToFunction(as.numeric(input$export_plot_height)*36))
  
  
  #for downloading ggplot
  output$ggplot_plugin_download <- downloadHandler(
    filename = function() {paste0("plot.", input$export_plot_format)},
    content = function(file) {
      ggsave(file, plot = ggplot_plugin(), device = {{input$export_plot_format}} , width = as.numeric({{input$export_plot_width}}), height = as.numeric({{input$export_plot_height}}), units = "cm")
    }
  )
  
  ############### boxplot ###############
  #make a reactive value to monitor plot visibility
  plot_visibility_boxplot <- reactiveValues()
  observe({plot_visibility_boxplot$value <- FALSE})
  
  #reactive for showing next boxes to input plotting instructions
  observeEvent(input$plot_next1_boxplot, {
    shinyjs::show(id = "hiddenbox4_boxplot")
    plot_visibility_boxplot$value = TRUE
  })
  observeEvent(input$plot_next1_boxplot, {
    shinyjs::show(id = "hiddenbox2_boxplot")
  })
  observeEvent(input$plot_next1_boxplot, {
    shinyjs::toggle(id = "hiddenbox3_boxplot")
  })
  observeEvent(input$plot_next5_boxplot, {
    shinyjs::toggle(id = "hiddenbox5_boxplot")
  })
  
  #update axis choice  
  observe({
    req(data_prm_combined$data)
    #update choices for plotting axis
    numeric.column = data_prm_combined$data %>% select_if(is.numeric) %>% colnames()
    factor.column = data_prm_combined$data %>% select_if(is.factor) %>% colnames()
    character.column = data_prm_combined$data %>% select_if(is.character) %>% colnames()
    
    updateSelectInput(inputId = "y_var_boxplot", choices = sort(numeric.column))
    updateSelectInput(inputId = "x_var_boxplot", choices = sort(union(factor.column, character.column)))
    
    updateSelectizeInput(inputId = "col_var_boxplot", choices = sort(union(factor.column, character.column)))
    updateSelectizeInput(inputId = "facet_var_boxplot", choices = c(sort(union(factor.column, character.column)), "time.horizon"))
    updateSelectizeInput(inputId = "facet_var2_boxplot", choices = sort(union(factor.column, character.column)))
    
    #update choice of time horizon duration based on data
    horizon.year.max = max(data_prm_combined$data$Year1)
    horizon.year.min = min(data_prm_combined$data$Year1)
    horizon.year.range = horizon.year.max - horizon.year.min
    horizon.default = ifelse(horizon.year.range >= 10, 10, 1)
    updateSliderInput(inputId = "time_horizon_duration", min = 1, max = horizon.year.range, step = 1, value = horizon.default)
    
  })
  
  #remember variable set for plotting, so can be recovered after making change to dataframe and plotting engine reactively update, reinitialise
  plot_var_select_cache_boxplot <- reactiveValues() 
  
  #record everytime that the value change, keep only the most recent previous value and current value
  observeEvent(input$y_var_boxplot, ignoreNULL = F,{
    plot_var_select_cache_boxplot$y_var <- input$y_var_boxplot
  })
  observeEvent(input$x_var_boxplot, ignoreNULL = F,{
    plot_var_select_cache_boxplot$x_var <- input$x_var_boxplot
  })
  observeEvent(input$col_var_boxplot, ignoreNULL = F,{
    plot_var_select_cache_boxplot$col_var <- input$col_var_boxplot
  })
  observeEvent(input$facet_var_boxplot, ignoreNULL = F,{
    plot_var_select_cache_boxplot$facet_var <- input$facet_var_boxplot
  })
  observeEvent(input$facet_var2_boxplot, ignoreNULL = F,{
    plot_var_select_cache_boxplot$facet_var2 <- input$facet_var2_boxplot
  })
  
  observeEvent(input$y_var_range_boxplot, ignoreNULL = F,{
    plot_var_select_cache_boxplot$y_var_range1 <- input$y_var_range_boxplot[1]
    plot_var_select_cache_boxplot$y_var_range2 <- input$y_var_range_boxplot[2]
  })
  
  #update value selected with the cache stored from previous user selection
  delay(10000, 
        observe({
          req(data_prm_combined_plot_rename_boxplot$data)
          #update choices for plotting axis
          numeric.column = data_prm_combined_plot_rename_boxplot$data %>% select_if(is.numeric) %>% colnames()
          factor.column = data_prm_combined_plot_rename_boxplot$data %>% select_if(is.factor) %>% colnames()
          character.column = data_prm_combined_plot_rename_boxplot$data %>% select_if(is.character) %>% colnames()
          
          #update choices for plotting axis
          updateSelectInput(inputId = "y_var_boxplot", choices = sort(numeric.column), selected = plot_var_select_cache_boxplot$y_var)
          updateSelectInput(inputId = "x_var_boxplot", choices = sort(union(factor.column, character.column)), selected = plot_var_select_cache_boxplot$x_var)
          #update choices for grouping variable
          updateSelectizeInput(inputId = "col_var_boxplot", choices = sort(union(factor.column, character.column)), selected = plot_var_select_cache_boxplot$col_var)
          updateSelectizeInput(inputId = "facet_var_boxplot", choices = sort(c(union(factor.column, character.column), "time.horizon")), selected = plot_var_select_cache_boxplot$facet_var)
          updateSelectizeInput(inputId = "facet_var2_boxplot", choices = sort(union(factor.column, character.column)), selected = plot_var_select_cache_boxplot$facet_var2)
          updateSelectizeInput(inputId = "rename_variable_boxplot", choices = sort(union(factor.column, character.column)))
          updateSelectizeInput(inputId = "reorder_variable_boxplot", choices = sort(union(factor.column, character.column)))
        })
  )
  
  # observe({
  #   updateSliderInput(inputId = "y_var_range_boxplot", value = c(plot_var_select_cache_boxplot$y_var_range1, plot_var_select_cache_boxplot$y_var_range2))
  # })
  

  
  #update year range choice for selecting reference period for calculating relative to mean historial period 
  observe({
    req(data_prm_combined$data)
    #update choices for plotting axis
    min_range = min(data_prm_combined$data$Year1)
    max_range = max(data_prm_combined$data$Year1)
    updateSliderInput(inputId = "ref_period_boxplot", min = min_range, max = max_range, step = 1, value = c(min_range, min_range+2))
  })
  
  ## if plotting relative to mean is selected, calculate mean based on selected reference period
  data_prm_combined_plot_boxplot <- reactive({
    req(data_prm_combined$data)
    req(input$use_mean_boxplot)
    req(input$ref_period_boxplot)
    req(input$x_var_boxplot)
    
    #add time horizon variable for data for plotting in case facet by time horizon
      # Define the bin size
      bin_size <- input$time_horizon_duration
      
      # Calculate the minimum and maximum years
      min_year <- min(data_prm_combined$data$Year1)
      max_year <- max(data_prm_combined$data$Year1)
      
      # Create the bin labels
      bin_labels <- paste(seq(min_year, max_year, by = bin_size),
                          unique(c(seq(min_year + bin_size - 1, max_year, by = bin_size), max_year)),
                          sep = "-")
      
      # Create a new column with bin categories
      data.timehorizon <- data_prm_combined$data
      data.timehorizon$time.horizon <- tryCatch(error = function(cnd) NULL,
                                                      cut(data.timehorizon$Year1,
                                                          breaks = seq(min_year, max_year + bin_size, by = bin_size),
                                                          labels = bin_labels,
                                                          include.lowest = TRUE))
    
    
    #calculate reference period normalisation
    if(input$use_mean_boxplot == "yes"){
      
      #select reference year range
      ref.year = c(input$ref_period_boxplot[1],input$ref_period_boxplot[2])
      
      #select variable for grouping to calculate mean for normalisation
      xaxis.var = input$x_var_boxplot
      grouping.var = xaxis.var
      if(!is.null(input$col_var_boxplot)){
        grouping.var = union(grouping.var, input$col_var_boxplot)
      }
      if(!is.null(input$facet_var_boxplot)){
        grouping.var = union(grouping.var, input$facet_var_boxplot)
      }
      if(!is.null(input$facet_var_boxplot)){
        grouping.var = union(grouping.var, input$facet_var2_boxplot)
      }
      grouping.var = setdiff(grouping.var, "time.horizon") #not include time horizon as grouping variable
      
      #normalise data
      data.norm = tryCatch(error = function(cnd) data.timehorizon,
        data.timehorizon %>%
        group_by(across(all_of(grouping.var))) %>%
        mutate(across(where(is.numeric) & !starts_with(c("Day","Month","Year")), ~ 100*(.x - mean(.x[which(Year1 %in% c(ref.year[1]:ref.year[2]))], na.rm = TRUE))/mean(.x[which(Year1 %in% c(ref.year[1]:ref.year[2]))], na.rm = TRUE) )) %>%
        filter(! Year1 %in%  c(input$ref_period_boxplot[1]:input$ref_period_boxplot[2]))
      )
      
      return(data.norm)
    }else{
      return(data.timehorizon)
    }
  })
  
  #for downloading reference period normalised dataset
  output$download_ref_norm_dataset <- downloadHandler(
    filename = "reference_period_normalised_data.tsv",
    content = function(file) {
      write_tsv(data_prm_combined_plot_boxplot(), file)
    }
  )
  
  
  ###data for plotting
  data_prm_combined_plot_rename_boxplot <- reactiveValues()
  observe({req(data_prm_combined_plot_boxplot())
    data_prm_combined_plot_rename_boxplot$data <- data_prm_combined_plot_boxplot()})
  
  ###set y axis range in plot
  observeEvent(input$y_var_boxplot, {
    if(is.numeric(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])){
      min = min(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])
      max = max(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])
      updateSliderInput(inputId = "y_var_range_boxplot", min = 0, max = ceiling(max*1.5), value = c(min,max)) 
    }
  })
  observeEvent(data_prm_combined_plot_rename_boxplot$data, {
    req(data_prm_combined_plot_rename_boxplot$data, input$y_var_boxplot, input$x_var_boxplot)
    if(is.numeric(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])){
      min = min(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])
      max = max(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])
      updateSliderInput(inputId = "y_var_range_boxplot", min = 0, max = ceiling(max*1.5), value = c(min,max)) 
    }
  })

  ###option for renaming variable
  #create observe event module to monitor if user input select variable to rename
  #if variable selected, update the select input list for value choices of the selected variable
  observeEvent(input$rename_variable_boxplot, {
    choices <- unique(data_prm_combined_plot_rename_boxplot$data[[input$rename_variable_boxplot]])
    updateSelectInput(inputId = "rename_from_boxplot", choices = sort(choices)) 
  })
  #change value of selected variable to the value from user
  observeEvent(input$rename_button_boxplot, {
    if(input$rename_to_boxplot != ""){
      rename_df <- data_prm_combined_plot_rename_boxplot$data
      rename_df[input$rename_variable_boxplot][rename_df[input$rename_variable_boxplot] == input$rename_from_boxplot] <- input$rename_to_boxplot
      data_prm_combined_plot_rename_boxplot$data <- rename_df
    }
    choices <- unique(data_prm_combined_plot_rename_boxplot$data[[input$rename_variable_boxplot]])
    updateSelectInput(inputId = "rename_from_boxplot", choices = sort(choices)) 
  })
  
  ###option for reordering variable
  #create observe event module to monitor if user input select variable to reorder
  #if variable selected, update the select input list for value choices of the selected variable
  observeEvent(input$reorder_variable_boxplot, {
    choices <- unique(data_prm_combined_plot_rename_boxplot$data[[input$reorder_variable_boxplot]])
    updateSelectInput(inputId = "reorder_order_boxplot", choices = sort(choices)) 
  })
  #change value of selected variable to the value from user
  observeEvent(input$reorder_button_boxplot, {
    req(input$reorder_variable_boxplot, input$reorder_order_boxplot)
    new_order = c(input$reorder_order_boxplot, setdiff(unique(data_prm_combined_plot_rename_boxplot$data[[input$reorder_variable_boxplot]]),input$reorder_order_boxplot))
    
    reorder_df <- data_prm_combined_plot_rename_boxplot$data
    reorder_df[[input$reorder_variable_boxplot]] <- factor(reorder_df[[input$reorder_variable_boxplot]], levels = new_order)
    data_prm_combined_plot_rename_boxplot$data <- reorder_df
  })
  
  
  #set color palette
  custom_palette_boxplot <- reactive({
    default_palette <- c("#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
    
    #vector of available color choices to form custom palette
    color_choice_hex <- c("#000000","#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E41A1C","#A6D854")
    names(color_choice_hex) <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
    
    #make palette from custom colors selected from user
    if(length(input$col_palette_boxplot) > 0){
      palette <- c(color_choice_hex[input$col_palette_boxplot], setdiff(default_palette, color_choice_hex[input$col_palette_boxplot]))
    }else{
      palette <- default_palette
    }
    unname(palette)
  })
  
  
  #set legend direction
  legend_direction_boxplot <- reactive({
    if(input$legend_position_boxplot %in% c("top","bottom")){
      "horizontal"
    } else{
      "vertical"
    }
  })
     
  #boxplot
  ggplot_plugin_boxplot <- reactive({
    req(data_prm_combined_plot_rename_boxplot$data)
    showModal(modalDialog(title = "Processing data...", "Please wait while the data is being processed.", easyClose = FALSE, footer = NULL))
    
    #initial plot according to selected coloring and group variable
    if(length(input$col_var_boxplot) > 0){
      p <- ggplot(data = data_prm_combined_plot_rename_boxplot$data, aes(x = .data[[input$x_var_boxplot]], y = .data[[input$y_var_boxplot]], fill = .data[[input$col_var_boxplot]]))
    } else{
      p <- ggplot(data = data_prm_combined_plot_rename_boxplot$data, aes(x = .data[[input$x_var_boxplot]], y = .data[[input$y_var_boxplot]]))
    }

    #add boxplot
    p <- p + 
      #stat_boxplot(geom="errorbar", width = 0.2) + #add horizontal line to end of whiskers
      geom_boxplot(width = 0.5)

    #select facet variable
    if(length(input$facet_var_boxplot) == 1 & length(input$facet_var2_boxplot) == 1){
      p <- p + facet_grid(get(input$facet_var2_boxplot)~get(input$facet_var_boxplot))
    } else if(length(input$facet_var_boxplot) == 1){
      p <- p + facet_grid(.~get(input$facet_var_boxplot))
    } else if(length(input$facet_var2_boxplot) == 1){
      p <- p + facet_grid(get(input$facet_var2_boxplot)~.)
    } else {
      p <- p
    }
    
    #theme
    p <- p +
      theme(axis.title = element_text(size = as.numeric(input$font_size_axis_title_boxplot)), 
            axis.text = element_text(size = as.numeric(input$font_size_axis_text_boxplot)),
            #legend.title = element_text(size = as.numeric(input$font_size_legend_boxplot)),
            legend.title = element_text(size = as.numeric(input$font_size_legend_boxplot), face = "bold"),
            legend.text = element_text(size = as.numeric(input$font_size_legend_boxplot)),
            strip.text = element_text(size = as.numeric(input$font_size_facet_boxplot)),
            plot.title = element_text(size = as.numeric(input$font_size_plot_title_boxplot)),
            legend.position = paste(input$legend_position_boxplot),
          panel.background = element_rect(colour = "black", fill = "white"),
          plot.background = element_rect(colour = NA, fill = "white"),
          legend.direction = paste(legend_direction_boxplot()),
          axis.line = element_line(colour="black",size=0.1),
          axis.ticks = element_line(),
          axis.title.x = element_text(vjust = -2.5, face = "bold"),
          axis.title.y = element_text(vjust = +2.5, face="bold"),
          legend.key = element_rect(colour = NA, fill = NA),
          strip.background=element_rect(colour="black",fill="grey80"),
          plot.margin=unit(c(10,5,5,5),"mm"),
          legend.box = "vertical"
          )+
      scale_fill_manual(values=custom_palette_boxplot()) +
      guides(color = guide_legend(keywidth = 5, keyheight = 3)
      )
    
    #pretty scales for numeric variable
    if(is.numeric(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])){
      p <- p +
        scale_y_continuous(breaks = breaks_pretty(), limits = c(as.numeric(input$y_var_range_boxplot[1]),as.numeric(input$y_var_range_boxplot[2])))
    }
    if(is.Date(data_prm_combined_plot_rename_boxplot$data[[input$y_var_boxplot]])){
      p <- p +
        scale_y_date(breaks = breaks_pretty())
    }
    
    #show legend title
    if(input$show_legend_title_boxplot == "no"){
      p <-  p + theme(legend.title = element_blank())
    }
    
    #x axis text angle
    if(as.numeric(input$x_axis_label_angle_boxplot) > 0){
      p <- p + theme(axis.text.x = element_text(angle = as.numeric(input$x_axis_label_angle_boxplot), hjust = 1, vjust = 1))
    }
    
    #add custom text for axis label
    if(nchar(input$y_var_label_boxplot) > 0){
      p <- p + labs(y = paste(input$y_var_label_boxplot))
    }
    if(nchar(input$x_var_label_boxplot) > 0){
      p <- p + labs(x = paste(input$x_var_label_boxplot))
    }
    if(nchar(input$title_label_boxplot) > 0){
      p <- p + labs(title = paste(input$title_label_boxplot))
    }
    
    #reset custom title when axis choice changes
    observeEvent(input$x_var_boxplot,ignoreInit = T, {
      updateTextInput(inputId = "x_var_label_boxplot", value = "")
    })
    observeEvent(input$y_var_boxplot, ignoreInit = T, {
      updateTextInput(inputId = "y_var_label_boxplot", value = "")
    })
    
    removeModal()
    
    #plot
    print(p)
    
  })
  
  #adjust default plot size according to facets
  #select facet variable
  observeEvent(input$facet_var_boxplot, {
    if(length(input$facet_var_boxplot) == 1){
      if(length(unique(data_prm_combined_plot_rename_boxplot$data[[input$facet_var_boxplot]])) == 1){
        updateTextInput(session, "export_plot_width_boxplot", value = "19")
      }
      if(length(unique(data_prm_combined_plot_rename_boxplot$data[[input$facet_var_boxplot]])) == 2){
        updateTextInput(session, "export_plot_width_boxplot", value = "29")
      }
      if(length(unique(data_prm_combined_plot_rename_boxplot$data[[input$facet_var_boxplot]])) > 2){
        updateTextInput(session, "export_plot_width_boxplot", value = "39")
      }
    }
  })
  observeEvent(input$facet_var2_boxplot, {
    if(length(input$facet_var2_boxplot) == 1){
      if(length(unique(data_prm_combined_plot_rename_boxplot$data[[input$facet_var2_boxplot]])) == 1){
        updateTextInput(session,"export_plot_height_boxplot", value = "12")
      }
      if(length(unique(data_prm_combined_plot_rename_boxplot$data[[input$facet_var2_boxplot]])) == 2){
        updateTextInput(session,"export_plot_height_boxplot", value = "22")
      }
      if(length(unique(data_prm_combined_plot_rename_boxplot$data[[input$facet_var2_boxplot]])) > 2){
        updateTextInput(session,"export_plot_height_boxplot", value = "32")
      }
    }
  })
  
    
  #render ggplot display in app
  output$ggplot_plugin_display_boxplot <- renderPlot({
    if(isTRUE(plot_visibility_boxplot$value)){
      if(!is_null(input$x_var_boxplot) & !is_null(input$y_var_boxplot)){
        ggplot_plugin_boxplot()
      }
    }
  },width=exprToFunction(as.numeric(input$export_plot_width_boxplot)*36), height=exprToFunction(as.numeric(input$export_plot_height_boxplot)*36))
  
  #for downloading ggplot
  output$ggplot_plugin_download_boxplot <- downloadHandler(
    filename = function() {paste0("boxplot.", input$export_plot_format_boxplot)},
    content = function(file) {
      ggsave(file, plot = ggplot_plugin_boxplot(), device = {{input$export_plot_format_boxplot}} , width = as.numeric({{input$export_plot_width_boxplot}}), height = as.numeric({{input$export_plot_height_boxplot}}), units = "cm")
    }
  )
  
  
  ###### Analysis ####    
  
  ###if standard aquacrop mode selected, hide analysis tabs
  observeEvent(input$standard_vs_plugin_select, {
    if (input$standard_vs_plugin_select == "GUI") {
      hideTab("analysis_tabbox", "Time period window")
      hideTab("analysis_tabbox", "Stress duration")
      hideTab("analysis_tabbox", "Regression")
    } else{
      showTab("analysis_tabbox", "Time period window")
      showTab("analysis_tabbox", "Stress duration")
      showTab("analysis_tabbox", "Regression")
    }
  })
  
  ###create reactive dataset for analyses (take from seasonal data)
  data_prm_combined_analysis <- reactiveValues()
  observe({data_prm_combined_analysis$data <- data_prm_combined$data})
  
  ###time period window analysis
  
  #update window slider input for selecting year range to include in analysis, set min max according to data
  observe({
    req(input$upload_all_files)
    
    year.start <- min(data_prm_combined_analysis$data[["Year1"]])
    year.end <- max(data_prm_combined_analysis$data[["Year1"]]) 
    
    updateSliderInput(inputId = "time_range", min = year.start, max = year.end, value = c(year.start, year.end))
  })
  
  #update window slider input for year, set min max according to data
  observe({
    req(input$upload_all_files)
    
    year.range <- max(data_prm_combined_analysis$data[["Year1"]]) - min(data_prm_combined_analysis$data[["Year1"]])
    updateSliderInput(inputId = "time_period", min = 1, max = year.range)
  })
  #update variable choice
  observe({
    choices <- setdiff(colnames(data_prm_combined_analysis$data), c("name","RunNr","Day1","Month1","Year1","DayN","MonthN","YearN","prm.file"))
    choices <- setdiff(choices, colnames(upload_prm_combined_renamecol$data))
    updateSelectizeInput(inputId = "time_period_variable", choices = sort(choices))
  })
  #update grouping choice
  observe({
    group.choices <- setdiff(colnames(data_prm_combined_analysis$data), colnames(upload_data_combined()))
    if("Stage" %in% colnames(data_prm_combined_analysis$data)){
      group.choices <- c(group.choices, "Stage")
    }
    group.choices <- group.choices[which(!str_detect(group.choices, "\\.duration\\."))]
    
    updateSelectizeInput(inputId = "time_period_group", choices = sort(group.choices))
  })
  
  #calculate summary in time period selected
  data_prm_combined_timeperiod <- reactive({
    req(input$upload_all_files)
    req(input$time_period_variable)
    
    #cut time into windows
    column.select <- c("time.window", input$time_period_variable, input$time_period_group)
    column.group <- c(input$time_period_group, "time.window")
    
    #calculate summary
    data_prm_combined_analysis$data %>%
      filter(Year1 >= input$time_range[1] & Year1 <= input$time_range[2]) %>%
      mutate(time.window = cut(Year1,
                               unique(c(seq(min(Year1), max(Year1), input$time_period),min(Year1), max(Year1))),
                               include.lowest = TRUE, right = FALSE, dig.lab = 4)) %>%
      mutate(time.window = str_replace_all(time.window,"\\[|\\)","")) %>%
      separate(time.window, c("start", "end"), sep = ",") %>%
      mutate(end = ifelse(str_detect(end, "\\]"), as.numeric(str_replace(end, "\\]",""))+1, end))%>%
      mutate(time.window = paste0(start, "-", as.numeric(end)-1)) %>%
      select(all_of(column.select)) %>%
      group_by(across(all_of(column.group))) %>%
      summarise(mean = mean(.data[[input$time_period_variable]]),
                SD = sd(.data[[input$time_period_variable]]),
                coef.of.variation = SD/mean,
                n = n()) %>%
      mutate(mean = format(signif(mean, digits = 3)),
             SD = format(signif(SD, digits = 3)),
             coef.of.variation = format(signif(coef.of.variation, digits = 3))
      ) %>%
      rename_with(~paste0(input$time_period_variable, ".", .x), c(mean, SD, coef.of.variation))
  })
  
  #display table
  output$data_prm_combined_timeperiod_display <- renderDataTable(data_prm_combined_timeperiod(), options = list(scrollX = TRUE))
  
  #for downloading calculated dataset
  output$download_data_prm_combined_timeperiod <- downloadHandler(
    filename = "Aquacrop_time_window_analysis.tsv",
    content = function(file) {
      write_tsv(data_prm_combined_timeperiod(), file)
    }
  )
  
  ###stress, phenological stage analysis
  
  #update grouping choice
  observe({
    group.choices <- setdiff(colnames(daily_data_prm_combined$data), colnames(upload_daily_data_combined()))
    updateSelectizeInput(inputId = "stress_group", choices = sort(group.choices))
  })
  
  #calculate summary 
  daily_data_prm_combined_stress <- reactive({
    req(input$upload_all_files)
    req(daily_upload_check()) #require that daily data are uploaded
    
    #selecting variables
    
    if(input$by_phenological == "yes"){
      column.group <- c("Year", "Stage", "prm.file.name", input$stress_group)
      column.select <- c("Year","Stage", "StExp", "StSto", "StSen", "StTr","prm.file.name", input$stress_group)
      
    }else{
      column.group <- c("Year", "prm.file.name", input$stress_group)
      column.select <- c("Year", "StExp", "StSto", "StSen", "StTr","prm.file.name", input$stress_group)
      
    }
    
    #calculate summary
    daily_data_prm_combined$data %>%
      mutate(Stage = as.character(Stage)) %>%
      mutate(Stage = case_when(
        Stage == "0" ~ "0_before_after_cropping",
        Stage == "1" ~ "1_sowing_transplant",
        Stage == "2" ~ "2_vegetative",
        Stage == "3" ~ "3_flowering",
        Stage == "4" ~ "4_yield_ripening",
        Stage == "-9" ~ "-9_senescence",
        TRUE ~ Stage
      )) %>%
      select(all_of(column.select)) %>%
      group_by(across(all_of(column.group))) %>%
      summarise(StExp.duration.days = length(StExp[which(StExp >= as.numeric(input$StExp_threshold))]),
                StSto.duration.days = length(StSto[which(StSto >= as.numeric(input$StSto_threshold))]),
                StSen.duration.days = length(StSen[which(StSen >= as.numeric(input$StSen_threshold))]),
                StTr.duration.days = length(StTr[which(StTr >= as.numeric(input$StTr_threshold))]),
                StExp.duration.percent = length(StExp[which(StExp >= as.numeric(input$StExp_threshold))])/length(StExp[which(StExp >= 0)]),
                StSto.duration.percent = length(StSto[which(StSto >= as.numeric(input$StSto_threshold))])/length(StSto[which(StSto >= 0)]),
                StSen.duration.percent = length(StSen[which(StSen >= as.numeric(input$StSen_threshold))])/length(StSen[which(StSen >= 0)]),
                StTr.duration.percent = length(StTr[which(StTr >= as.numeric(input$StTr_threshold))])/length(StTr[which(StTr >= 0)])
      ) %>%
      mutate(StExp.duration.percent = format(signif(StExp.duration.percent*100, digits = 2)) %>% as.numeric(),
             StSto.duration.percent = format(signif(StSto.duration.percent*100, digits = 2)) %>% as.numeric(),
             StSen.duration.percent = format(signif(StSen.duration.percent*100, digits = 2)) %>% as.numeric(),
             StTr.duration.percent = format(signif(StTr.duration.percent*100, digits = 2)) %>% as.numeric()
      )
  })
  
  #display table
  output$daily_data_prm_combined_stress_display <- renderDataTable(daily_data_prm_combined_stress(), options = list(scrollX = TRUE))
  
  #for downloading calculated dataset
  output$download_daily_data_prm_combined_stress <- downloadHandler(
    filename = "Aquacrop_stress_analysis.tsv",
    content = function(file) {
      write_tsv(daily_data_prm_combined_stress(), file)
    }
  )
  
  ##append stress duration data to seasonal dataset for plotting and other analyses
  observeEvent(input$append_stress_data_button, {
    showModal(modalDialog(title = "Processing data...", "Please wait while the data is being processed.", easyClose = FALSE, footer = NULL))
    req(data_prm_combined$data, data_prm_combined_analysis$data, daily_data_prm_combined_stress())
    data_prm_combined_analysis$data <- left_join(data_prm_combined_analysis$data %>% select(all_of(setdiff(colnames(data_prm_combined_analysis$data),c("Stage", "StExp.duration.days", "StSto.duration.days", "StSen.duration.days", "StTr.duration.days", "StExp.duration.percent", "StSto.duration.percent", "StSen.duration.percent", "StTr.duration.percent")))) %>% distinct(),
                                                 daily_data_prm_combined_stress() %>% select("prm.file.name", "Year", "StExp.duration.days", "StSto.duration.days", "StSen.duration.days", "StTr.duration.days", "StExp.duration.percent", "StSto.duration.percent", "StSen.duration.percent", "StTr.duration.percent"),
                                                 by = c("prm.file.name" = "prm.file.name", "Year1"="Year"))
    
    data_prm_combined$data  <- left_join(data_prm_combined$data %>% select(all_of(setdiff(colnames(data_prm_combined$data),c("Stage", "StExp.duration.days", "StSto.duration.days", "StSen.duration.days", "StTr.duration.days", "StExp.duration.percent", "StSto.duration.percent", "StSen.duration.percent", "StTr.duration.percent")))) %>% distinct(),
                                         daily_data_prm_combined_stress() %>% select("prm.file.name", "Year", "StExp.duration.days", "StSto.duration.days", "StSen.duration.days", "StTr.duration.days", "StExp.duration.percent", "StSto.duration.percent", "StSen.duration.percent", "StTr.duration.percent"),
                                         by = c("prm.file.name" = "prm.file.name", "Year1"="Year"))
    removeModal()
  })

  
  ######regression
  
  #update options for data mode, if daily data not uploaded, remove daily option for mode
  observe({
    req(input$upload_all_files)
    if(daily_upload_check() == FALSE){
      updateSelectizeInput(inputId = "regression_mode", choices = c("seasonal"))
    }else{
      updateSelectizeInput(inputId = "regression_mode", choices = c("daily","seasonal"))
    }
  })
  
  ##select data mode daily or seasonal  
  data_mode_selected_regression <- reactive({
    req(input$regression_mode)
    req(input$upload_all_files)
    
    if(input$regression_mode == "daily"){
      #return data to use
      daily_data_prm_combined$data
    }else{
      #return data to use
      data_prm_combined_analysis$data 
    }
  })
  
  observe({
    req(input$regression_mode)
    req(input$upload_all_files)
    
    if(input$regression_mode == "daily"){
      #update choices for variables
      axis.choices = unique(colnames(daily_data_prm_combined$data))
      updateSelectInput(inputId = "regression_y_variable", choices = sort(axis.choices))
      updateSelectInput(inputId = "regression_x_variable", choices = sort(axis.choices))
      #update choices for grouping variable
      group.choices <- setdiff(colnames(daily_data_prm_combined$data), colnames(upload_daily_data_combined()))
      group.choices <- c(group.choices, "Stage", "Year","Month")
      group.choices <- setdiff(group.choices, "date")
      updateSelectizeInput(inputId = "regression_group", choices = sort(group.choices)) 
    }else{
      #update choices for variables
      axis.choices = unique(colnames(data_prm_combined_analysis$data))
      updateSelectInput(inputId = "regression_y_variable", choices = sort(axis.choices))
      updateSelectInput(inputId = "regression_x_variable", choices = sort(axis.choices))
      #update choices for grouping variable
      group.choices <- setdiff(colnames(data_prm_combined_analysis$data), colnames(upload_data_combined()))
      if("Stage" %in% colnames(data_prm_combined_analysis$data)){
        group.choices <- c(group.choices, "Stage")
      }
      group.choices <- group.choices[which(!str_detect(group.choices, "\\.duration\\."))]
      updateSelectizeInput(inputId = "regression_group", choices = sort(group.choices)) 
    }
  })
  
  #regression calculation
  data_regression <- reactive({
    req(input$regression_mode)
    req(input$regression_y_variable)
    req(input$regression_x_variable)
    req(input$upload_all_files)
    
    if(length(input$regression_group) > 0){
      column.group <- input$regression_group
      column.select <- c(input$regression_y_variable, input$regression_x_variable, input$regression_group)
    }else{
      column.group <- NULL
      column.select <- c(input$regression_y_variable, input$regression_x_variable)
    }
    
    data_mode_selected_regression() %>%
      select(all_of(column.select)) %>%
      group_by(across(all_of(column.group))) %>%
      nest() %>%
      mutate(.model = map(data, function(data){
        mod <- lm(data = data, as.formula(paste0("`",input$regression_y_variable,"`","~","`",input$regression_x_variable,"`")))
        
        r.squared <- glance(mod)[["r.squared"]] %>% signif(digits = 3) %>% format()
        slope <- tidy(mod)[["estimate"]][[2]] %>% signif(digits = 3) %>% format()
        p.value <- tidy(mod)[["p.value"]][[2]] %>% signif(digits = 3) %>% format() 
        
        summary <- data.frame(r.squared,slope,p.value)
      })) %>%
      select(-data) %>%
      unnest(.model)
  })
  
  #output datatable of the regression
  output$regression_display <- renderDataTable(datatable(data_regression(), 
                                                         options = list(scrollX = TRUE)))
  #for downloading regresion data
  output$download_regression <- downloadHandler(
    filename = "Aquacrop_regression.tsv",
    content = function(file) {
      write_tsv(data_regression(), file)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

