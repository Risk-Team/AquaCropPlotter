# p_load install the packages if not present
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,shinydashboard, tidyverse, DT, lubridate, shinyjs, shinyBS, furrr)

#sets of input variables to select for plotting
input_plot_x_variable <- c("Year1")
input_plot_y_variable <- c("Rain","ExpStr","E","ETo","Irri","StoStr","Yield","WPet")
input_group_variable <- c("climate","location","rcp","irrigation","crop","soil","sowing.date")
input_plot_variable_standard <- c("Biomass", "Date")
input_color_choice <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
input_plot_element_choice <- c("point", "line", "linear_trend", "linear_trend_error","grid_line")
input_legend_pos <- c("none","right","bottom","left","top")
input_shape_choice <- c("circle", "triangle", "rectangle", "diamond", "cross", "hollow_circle", "hollow_triangle", "hollow_rectangle", "hollow_diamond")

#define UI dashboard
ui <- dashboardPage(
    dashboardHeader(
      # Set height of dashboardHeader
      tags$li(class = "dropdown",
              tags$style(".main-header .logo {height: 130px}")
              ),
      #use image logo, link to github
      title = tags$a(href="https://github.com/Risk-Team/aquacrop_shiny",
              tags$img(src="shinyaquacrop_logo.png",height="121",width="181"))
      ),
    
    
    dashboardSidebar(collapsed = FALSE,
        # Adjust the sidebar padding to allow large logo in the heading
        tags$style(".left-side, .main-sidebar {padding-top: 130px}"),
        
        sidebarMenu(id = "menu_tabs",
            menuItem("Home", tabName = "tab_home", icon = icon("home")),
            menuItem("Standard", tabName = "aquacrop_standard", icon = icon("list-alt"), startExpanded = FALSE,
                     menuSubItem("Upload_data", tabName = "tab_upload_data_standard", icon = icon("caret-right")),
                     menuSubItem("Combined_data", tabName = "tab_combined_data_standard", icon = icon("caret-right")),
                     menuSubItem("Plot", tabName = "tab_plot_standard", icon = icon("caret-right")),
                     menuSubItem("Analysis", tabName = "tab_analysis_standard", icon = icon("caret-right"))
                     ),
            menuItem("Plug-in", tabName = "aquacrop_plugin", icon = icon("puzzle-piece"), startExpanded = FALSE,
                     menuSubItem("Upload_data", tabName = "tab_upload_data_plugin", icon = icon("caret-right")),
                     menuSubItem("Combined_data", tabName = "tab_combined_data_plugin", icon = icon("caret-right")),
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
                                    
                                    .skin-blue .main-header .logo {background-color: #ffffff;}
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
                        ,
                        # fluidRow(
                        #     box(title = "Aquacrop standard (single season)", status = "primary", solidHeader = TRUE,
                        #         actionButton("select_aquacrop_standard", "Select Aquacrop standard")
                        #     ),
                        #     box(title = "Aquacrop plug-in (multiple seasons)", status = "primary", solidHeader = TRUE,
                        #         actionButton("select_aquacrop_plugin", "Select Aquacrop plugin")
                        #     )
                        # )
                    )
            ),
            tabItem(tabName = "tab_upload_data_standard",
                    h2(
                        #display boxes for data and prm files upload
                        fluidRow(
                            box(title = "Data files and metadata", status = "primary", solidHeader = TRUE, width = 6,
                                #upload data file
                                fileInput("upload_data_files_standard", "Upload data files (.OUT)", multiple = TRUE, accept = ".OUT"),
                                textInput("upload_climate_std","climate"),
                                textInput("upload_location_std","location"),
                                textInput("upload_rcp_std","rcp"),
                                textInput("upload_irrigation_std","irrigation"),
                                textInput("upload_crop_std","crop"),
                                textInput("upload_soil_std","soil"),
                                textInput("upload_sowing_std","sowing date"),
                                textInput("upload_note_std","note"),
                                actionButton("upload_button_standard","Upload", icon = icon("upload"))
                            ),
                            box(title = "Uploaded data", status = "primary", solidHeader = TRUE, width = 6,
                              div(dataTableOutput("upload_standard_list"), style = "font-size: 75%; width: 100%")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_combined_data_standard",
                    h2(
                        ##daily data
                        #button for downloading data
                        downloadButton("download_data_standard_combined_daily", "Download combined daily dataset"),
                        #display combined daily data table
                        div(dataTableOutput("upload_data_standard_combined_daily_display"), style = "font-size: 75%; width: 100%"),
                        ##seasonal data
                        #button for downloading data
                        downloadButton("download_data_standard_combined_seasonal", "Download combined seasonal dataset"),
                        #display combined seasonal data table
                        div(dataTableOutput("upload_data_standard_combined_seasonal_display"), style = "font-size: 75%; width: 100%")
                    )
            ),
            tabItem(tabName = "tab_plot_standard", 
                    h2(
                      fluidRow(
                        box(title = "Select plotting variables",
                            width = 3,
                            height = "350px",
                            status = "primary",
                            solidHeader = TRUE,
                            selectInput("plot_mode_std", "Data type to plot", c("daily","seasonal")),
                            selectInput("y_var_std", "Variable to plot on Y axis", choices=NULL),
                            #selectInput("x_var", "Select variable to plot on X axis", input_plot_x_variable, selected = "Year1"),
                            div(style = "position:absolute;right:0.1em; bottom:0.1em;date",actionButton("plot_next1_std", "Next", icon = icon("chevron-right")))
                        ),
                        shinyjs::hidden(div(id = "hiddenbox1_std",
                                            box(title = "Calculate mean",
                                                width = 3,
                                                height = "350px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                selectInput("use_mean_std", 
                                                            label = tags$span("Plot mean values",   bsButton("plot_info1_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                            c("Yes", "No"), selected = "No"),
                                                bsPopover(id = "plot_info1_std", title = "", placement = "right", trigger = "hover"),
                                                conditionalPanel(condition = "input.use_mean_std == 'Yes'",
                                                                 selectizeInput("group_var_std", 
                                                                                label = tags$span("Select variable to group for calculating mean",  bsButton("plot_info2_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                                                input_group_variable,
                                                                                multiple = TRUE),
                                                                 bsPopover(id = "plot_info2_std", title = "", placement = "right", trigger = "hover")),
                                                div(style = "position:absolute;right:0.1em; bottom:0.1em;",actionButton("plot_next2_std", "Next", icon = icon("chevron-right")))
                                            )
                        )),
                        shinyjs::hidden(div(id = "hiddenbox2_std",
                                            box(title = "Select grouping variables",
                                                width = 3,
                                                height = "350px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                selectizeInput("col_var_std", 
                                                               label = tags$span("Variable to split into colors by",  bsButton("plot_info3_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                               input_group_variable,
                                                               multiple = TRUE, options = list(maxItems = 1)),
                                                bsPopover(id = "plot_info3_std", title = "", placement = "right", trigger = "hover"),
                                                selectizeInput("shape_var_std", 
                                                               label = tags$span("Variable to split into shapes by",  bsButton("plot_info7_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                               input_group_variable,
                                                               multiple = TRUE, options = list(maxItems = 1)),
                                                bsPopover(id = "plot_info7_std", title = "", placement = "right", trigger = "hover"),
                                                selectizeInput("facet_var_std", 
                                                               label = tags$span("Variable to split into subpanels by", bsButton("plot_info4_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                               choices = input_group_variable,
                                                               multiple = TRUE, options = list(maxItems = 2)),
                                                bsPopover(id = "plot_info4_std", title = "selected variable will be used to split plot into subplots. maximum 2 variables can be selected", placement = "right", trigger = "hover"),
                                                div(style = "position:absolute;right:0.1em; bottom:0.1em;",actionButton("plot_next3_std", "Next", icon = icon("chevron-right")))
                                            )
                        )),
                        shinyjs::hidden(div(id = "hiddenbox3_std",
                                            box(title = "Select plot elements",
                                                width = 3,
                                                height = "350px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                selectizeInput("plot_element_std", 
                                                               label = tags$span("Elements to plot", bsButton("plot_info5_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                               input_plot_element_choice, multiple = TRUE, selected = c("point", "linear_trend","grid_line")),
                                                bsPopover(id = "plot_info5_std", title = "", placement = "right", trigger = "hover"),
                                                div(style = "position:absolute;right:0.1em; bottom:0.1em;",actionButton("plot_next4_std", "Plot", icon = icon("chevron-right")))
                                            )
                        ))
                      ),
                      shinyjs::hidden(div(id = "hiddenbox5_std",
                                          fluidRow(
                                            box(title = "Customise plot",
                                                width = 2,
                                                height = "550px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                selectizeInput("col_palette_std", 
                                                               label = tags$span("color palette", bsButton("plot_info6_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                               input_color_choice, multiple = TRUE),
                                                bsPopover(id = "plot_info6_std", title = "Select the same number of colors as values of grouping variable, in order", placement = "right", trigger = "hover"),
                                                selectizeInput("shape_palette_std", 
                                                               label = tags$span("shape palette", bsButton("plot_info8_std", label = "", icon = icon("info"), size = "extra-small")), 
                                                               input_shape_choice, multiple = TRUE),
                                                bsPopover(id = "plot_info8_std", title = "Select the same number of shapes as values of grouping variable, in order", placement = "right", trigger = "hover"),
                                                selectInput("legend_position_std", "Legend position", input_legend_pos, selected = "bottom"),
                                                textInput("point_size_std", "point size", value = "2"),
                                            ),
                                            box(title = "Customise labels",
                                                width = 2,
                                                height = "550px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                textInput("title_label_std", "plot title"),
                                                textInput("y_var_label_std", "Y axis label"),
                                                textInput("x_var_label_std", "X axis label"),
                                                textInput("legend_label_std", "legend label"),
                                            ),
                                            box(title = "Customise font size",
                                                width = 2,
                                                height = "550px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                textInput("font_size_plot_title_std", "plot title", value = "16"),
                                                textInput("font_size_axis_text_std", "axis text", value = "16"),
                                                textInput("font_size_axis_title_std", "axis title", value = "16"),
                                                textInput("font_size_legend_std", "legend", value = "16"),
                                                textInput("font_size_facet_std", "subpanel label", value = "16")
                                            ),
                                            box(title = "Rename variables",
                                                width = 2,
                                                height = "550px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                selectizeInput("rename_variable_std", "Select variable to rename", input_group_variable, multiple = TRUE, options = list(maxItems = 1)),
                                                selectizeInput("rename_from_std", "Select value to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                                textInput("rename_to_std", "Rename to"),
                                                actionButton("rename_button_std", "Rename")
                                            ),
                                            box(title = "Export plot",
                                                width = 2,
                                                height = "550px",
                                                status = "primary",
                                                solidHeader = TRUE,
                                                textInput("export_plot_width_std", "Width (cm)", value = "19"),
                                                textInput("export_plot_height_std", "Height (cm)", value = "12"),
                                                selectInput("export_plot_format_std", "Format", c("pdf","png"), selected = "pdf"),
                                                downloadButton("ggplot_std_download", "Download"))
                                          )
                      )),
                      shinyjs::hidden(div(id = "hiddenbox4_std",
                                          fluidRow(
                                            tabBox(width = 12,
                                                   height = "900px",
                                                   id = "std_plot_tabbox",
                                                   tabPanel("Plot",
                                                            div(style = "position:absolute;right:1em; top:0.25em;",actionButton("plot_next5_std", "Customise & export plot")),
                                                            plotOutput("ggplot_std_display")
                                                   )
                                            ),
                                          )))
                    )
            ),
            tabItem(tabName = "tab_analysis_standard",
                    h2(
                      
                    )
            ),
            tabItem(tabName = "tab_upload_data_plugin",
                    h2(
                        #to display error when insufficient prm files are uploaded
                        fluidRow(
                        ),
                        #display boxes for data and prm files upload
                        fluidRow(
                            box(title = "Seasonal data files", status = "primary", solidHeader = TRUE, width = 4,
                                #upload data file
                                fileInput("upload_data_files", "Upload files (season.OUT)", multiple = TRUE, accept = ".OUT"),
                                div(dataTableOutput("upload_data_combined_display"), style = "font-size: 75%; width: 100%"),
                                dataTableOutput("missing_seasonal_file_error")
                            ),
                            box(title = "Daily data files", status = "primary", solidHeader = TRUE, width = 4,
                                #upload data file
                                fileInput("upload_daily_data_files", "Upload files (day.OUT)", multiple = TRUE, accept = ".OUT"),
                                div(dataTableOutput("upload_daily_data_combined_display"), style = "font-size: 75%; width: 100%")
                            ),
                            box(title = "Parameter files", status = "primary", solidHeader = TRUE, width = 4,
                                #upload parameter file
                                fileInput("upload_prm_files", "Upload files (.PRM)", multiple = TRUE, accept = ".PRM"),
                                div(dataTableOutput("upload_prm_combined_display"), style = "font-size: 75%; width: 100%"),
                                dataTableOutput("missing_prm_file_error")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_combined_data_plugin",
                    h2(
                        fluidRow(
                        #display combined data table
                        tabBox(width = 12,
                               tabPanel(title = "Seasonal dataset",
                            width = 12,
                            status = "primary",
                            solidHeader = FALSE,
                            #button for downloading all combined data
                            downloadButton("download_combined_dataset", "Download"),
                            #data table
                            div(dataTableOutput("data_prm_combined_display"), style = "font-size: 75%; width: 100%")
                        ),
                        tabPanel(title = "Daily dataset",
                            width = 12,
                            status = "primary",
                            solidHeader = FALSE,
                            #button for downloading all combined data
                            downloadButton("download_combined_daily_dataset", "Download"),
                            #data table
                            div(dataTableOutput("daily_data_prm_combined_display"), style = "font-size: 75%; width: 100%")
                        ),
                        tabPanel(title = "Parameter",
                            width = 12,
                            status = "primary",
                            solidHeader = FALSE,
                            #button for downloading all combined data
                            downloadButton("download_combined_prm", "Download"),
                            #data table
                            div(dataTableOutput("prm_combined_display"), style = "font-size: 75%; width: 100%")
                        )
                        ),
                        box(title = "Rename parameter column",
                            width = 4,
                            #height = "550px",
                            status = "primary",
                            solidHeader = TRUE,
                            selectizeInput("rename_col_from", "Select column to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                            textInput("rename_col_to", "Rename to"),
                            actionButton("rename_col_button", "Rename")
                            )
                      )
                    )
            ),
            tabItem(tabName = "tab_plot_plugin",
                    h2(
                        fluidRow(
                            box(title = "Select plotting variables",
                                width = 3,
                                height = "350px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectInput("plot_mode", "Data type to plot", c("daily","seasonal")),
                                selectInput("y_var", "Variable to plot on Y axis", input_plot_y_variable, selected = "Yield"),
                                selectInput("x_var", "Variable to plot on X axis", input_plot_x_variable, selected = "Year1"),
                                div(style = "position:absolute;right:0.1em; bottom:0.1em;date",actionButton("plot_next1", "Next", icon = icon("chevron-right")))
                                ),
                            shinyjs::hidden(div(id = "hiddenbox1",
                              box(title = "Calculate mean",
                                  width = 3,
                                  height = "350px",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  selectInput("use_mean", 
                                              label = tags$span("Plot mean values",   bsButton("plot_info1", label = "", icon = icon("info"), size = "extra-small")), 
                                              c("Yes", "No"), selected = "No"),
                                  bsPopover(id = "plot_info1", title = "", placement = "right", trigger = "hover"),
                                  conditionalPanel(condition = "input.use_mean == 'Yes'",
                                                  selectizeInput("group_var", 
                                                   label = tags$span("Select variable to group for calculating mean",  bsButton("plot_info2", label = "", icon = icon("info"), size = "extra-small")), 
                                                   input_group_variable,
                                                    multiple = TRUE),
                                    bsPopover(id = "plot_info2", title = "", placement = "right", trigger = "hover")),
                                  div(style = "position:absolute;right:0.1em; bottom:0.1em;",actionButton("plot_next2", "Next", icon = icon("chevron-right")))
                              )
                            )),
                            shinyjs::hidden(div(id = "hiddenbox2",
                              box(title = "Select grouping variables",
                                width = 3,
                                height = "350px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("col_var", 
                                            label = tags$span("Variable to split into colors by",  bsButton("plot_info3", label = "", icon = icon("info"), size = "extra-small")), 
                                            input_group_variable,
                                            multiple = TRUE, options = list(maxItems = 1)),
                                bsPopover(id = "plot_info3", title = "", placement = "right", trigger = "hover"),
                                selectizeInput("shape_var", 
                                               label = tags$span("Variable to split into shapes by",  bsButton("plot_info7", label = "", icon = icon("info"), size = "extra-small")), 
                                               input_group_variable,
                                               multiple = TRUE, options = list(maxItems = 1)),
                                bsPopover(id = "plot_info7", title = "", placement = "right", trigger = "hover"),
                                selectizeInput("facet_var", 
                                               label = tags$span("Variable to split into subpanels by", bsButton("plot_info4", label = "", icon = icon("info"), size = "extra-small")), 
                                               choices = input_group_variable,
                                               multiple = TRUE, options = list(maxItems = 2)),
                                bsPopover(id = "plot_info4", title = "selected variable will be used to split plot into subplots. maximum 2 variables can be selected", placement = "right", trigger = "hover"),
                                div(style = "position:absolute;right:0.1em; bottom:0.1em;",actionButton("plot_next3", "Next", icon = icon("chevron-right")))
                                )
                            )),
                             shinyjs::hidden(div(id = "hiddenbox3",
                            box(title = "Select plot elements",
                                width = 3,
                                height = "350px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("plot_element", 
                                               label = tags$span("Elements to plot", bsButton("plot_info5", label = "", icon = icon("info"), size = "extra-small")), 
                                               input_plot_element_choice, multiple = TRUE, selected = c("point", "linear_trend","grid_line")),
                                bsPopover(id = "plot_info5", title = "", placement = "right", trigger = "hover"),
                                div(style = "position:absolute;right:0.1em; bottom:0.1em;",actionButton("plot_next4", "Plot", icon = icon("chevron-right")))
                                )
                            ))
                        ),
                        shinyjs::hidden(div(id = "hiddenbox5",
                           fluidRow(
                            box(title = "Customise plot",
                                width = 2,
                                height = "550px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("col_palette", 
                                               label = tags$span("color palette", bsButton("plot_info6", label = "", icon = icon("info"), size = "extra-small")), 
                                               input_color_choice, multiple = TRUE),
                                bsPopover(id = "plot_info6", title = "Select the same number of colors as values of grouping variable, in order", placement = "right", trigger = "hover"),
                                selectizeInput("shape_palette", 
                                               label = tags$span("shape palette", bsButton("plot_info8", label = "", icon = icon("info"), size = "extra-small")), 
                                               input_shape_choice, multiple = TRUE),
                                bsPopover(id = "plot_info8", title = "Select the same number of shapes as values of grouping variable, in order", placement = "right", trigger = "hover"),
                                selectInput("legend_position", "Legend position", input_legend_pos, selected = "bottom"),
                                textInput("point_size", "point size", value = "2"),
                                ),
                            box(title = "Customise labels",
                                width = 2,
                                height = "550px",
                                status = "primary",
                                solidHeader = TRUE,
                                textInput("title_label", "plot title"),
                                textInput("y_var_label", "Y axis label"),
                                textInput("x_var_label", "X axis label"),
                                textInput("x_axis_label_angle", "X axis label angle", value = "0"),
                                textInput("legend_label", "legend label"),
                                ),
                            box(title = "Customise font size",
                                width = 2,
                                height = "550px",
                                status = "primary",
                                solidHeader = TRUE,
                                textInput("font_size_plot_title", "plot title", value = "16"),
                                textInput("font_size_axis_text", "axis text", value = "16"),
                                textInput("font_size_axis_title", "axis title", value = "16"),
                                textInput("font_size_legend", "legend", value = "16"),
                                textInput("font_size_facet", "subpanel label", value = "16")
                            ),
                            box(title = "Rename variables",
                                width = 2,
                                height = "550px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("rename_variable", "Select variable to rename", input_group_variable, multiple = TRUE, options = list(maxItems = 1)),
                                selectizeInput("rename_from", "Select value to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                textInput("rename_to", "Rename to"),
                                actionButton("rename_button", "Rename")
                            ),
                            box(title = "Export plot",
                                width = 2,
                                height = "550px",
                                status = "primary",
                                solidHeader = TRUE,
                                textInput("export_plot_width", "Width (cm)", value = "19"),
                                textInput("export_plot_height", "Height (cm)", value = "12"),
                                selectInput("export_plot_format", "Format", c("pdf","png"), selected = "pdf"),
                                downloadButton("ggplot_plugin_download", "Download"))
                        )
                        )),
                        shinyjs::hidden(div(id = "hiddenbox4",
                                            fluidRow(
                                              tabBox(width = 12,
                                                     height = "900px",
                                                     id = "plugin_plot_tabbox",
                                                     tabPanel("Plot",
                                                              div(style = "position:absolute;right:1em; top:0.25em;",actionButton("plot_next5", "Customise & export plot")),
                                                              plotOutput("ggplot_plugin_display")
                                                     )
                                              ),
                                            )))
                    )
            ),
            tabItem(tabName = "tab_analysis_plugin",
                    h2(
                      fluidRow(
                        tabBox(width = 12,
                               tabPanel(title = "Analysis",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        
                               ),
                               tabPanel(title = "Time period window",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        sliderInput("time_period", label = "Select time period window (years)", min = 1, max = 1, value = 1, step = 1),
                                        selectInput("time_period_variable", label = "Select variable to calculate summary", choices = NULL, selected = "Yield"),
                                        selectizeInput("time_period_group", label = "Select grouping variable", choices = NULL, multiple = TRUE),
                                        div(dataTableOutput("data_prm_combined_timeperiod_display"), style = "font-size: 75%; width: 100%"),
                                        downloadButton("download_data_prm_combined_timeperiod", "Download")
                               ),
                               tabPanel(title = "Phenological stages",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        
                               ),
                               tabPanel(title = "Correlation",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        
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
            )
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
  
  #allow upload file size max limit of 30 MB
  options(shiny.maxRequestSize=30*1024^2)
  
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
      read.csv("glossary.csv")
    })
    output$glossary_display <- renderDataTable(glossary())
    
    # ###select button to enter aquacrop standard or plugin
    # observeEvent(input$select_aquacrop_standard, {
    #     updateTabItems(session, "menu_tabs", "tab_upload_data_standard")
    # })
    # observeEvent(input$select_aquacrop_plugin, {
    #     updateTabItems(session, "menu_tabs", "tab_upload_data_plugin")
    # })
    
    ##########standard
    ###upload data

    #upload data  file and label text for the set
    #upload and append multiple sets
    upload_standard <- reactiveValues()
    observe({upload_standard$list <- NULL})
    observe({upload_standard$temp_list <- NULL})
    observeEvent(input$upload_button_standard, {
      req(input$upload_data_files_standard)
      upload_standard$temp_list <- input$upload_data_files_standard %>% 
        mutate(climate = input$upload_climate_std,
               location = input$upload_location_std,
               rcp = input$upload_rcp_std,
               irrigation = input$upload_irrigation_std,
               crop = input$upload_crop_std,
               soil = input$upload_soil_std,
               sowing_date = input$upload_sowing_std,
               note = input$upload_note_std)
    })

    observeEvent(input$upload_button_standard, {
      req(upload_standard$temp_list)
      old_list <- upload_standard$list
      new_list <- bind_rows(old_list, upload_standard$temp_list)
        #%>%distinct(name, .keep_all = TRUE)
      upload_standard$list <- new_list
    })
    
    #show list all uploaded file 
    observeEvent(input$upload_button_standard, {output$upload_standard_list <- renderDataTable(upload_standard$list %>% select(-c(size, type, datapath))
                                                   ,options = list(scrollX = TRUE)) })
    
    #read and clean data
    upload_data_standard_combined <- reactive({
            #require uploaded data files before evaluating
            req(input$upload_data_files_standard)
            req(upload_standard$list)
            #check to make sure uploaded files has the correct extension .OUT, return error if not 
            upload_data_files_standard_ext <- tools::file_ext(input$upload_data_files_standard$name)
            if(any(upload_data_files_standard_ext != "OUT")){
                validate("Invalid data file: Please upload only .OUT files")
            }

            #list of file extensions of all output files (9 files)
            file.extension = c("Clim.OUT","CompEC.OUT","CompWC.OUT","Crop.OUT","Inet.OUT","Prof.OUT","Run.OUT","Salt.OUT","Wabal.OUT")
            
            #read data
            #input$upload_data_files_standard %>% #for one upload  
            upload_standard$list %>% #for multiple upload
                #detect file extensions
                mutate(extension = str_extract(name, paste(file.extension, sep="|"))) %>%
                mutate(dataset = map2(datapath, extension, function(datapath, extension){
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
                    data = read_tsv(file = file.clean, skip = line.skip, n_max = line.n, col_names = heading) %>%
                        select(-1) #remove blank column at the start
                })) %>%
                select(-size, -type, -datapath)
        })

    #output datatable of the data file name being read
    output$upload_data_standard_combined_display <- renderDataTable(upload_data_standard_combined() %>%
                                                                        distinct(),
                                                                    options = list(scrollX = TRUE))
   
    ##combined all daily data  
    upload_data_standard_combined_daily <- reactive({
      #filter out Run.OUT file as data format (seasonal)  different from others (daily)
      upload_data_standard_combined() %>%
        filter(extension != "Run.OUT") %>%
        group_by(climate,location,rcp,irrigation,crop,soil,sowing_date,note) %>%
        nest() %>%
        mutate(all_data = map(data, function(data){
          data$dataset %>%
            reduce(left_join, by = c("Day", "Month", "Year", "DAP", "Stage"))
        })) %>%
        select(-data) %>%
        unnest(all_data) %>%
        mutate(date = dmy(paste(Day, Month, Year, sep="-")))
    })
      
    #render output display
    output$upload_data_standard_combined_daily_display <- renderDataTable(upload_data_standard_combined_daily(),
                                                                          options = list(scrollX = TRUE))
    #for downloading combined daily dataset
    output$download_data_standard_combined_daily <- downloadHandler(
        filename = "Aquacrop_standard_daily_combined_data.tsv",
        content = function(file) {
            write_tsv(upload_data_standard_combined_daily(), file)
        }
    )
    
    ##seasonal data
    upload_data_standard_combined_seasonal <- reactive({
      upload_data_standard_combined() %>%
        filter(extension == "Run.OUT") %>%
        unnest(dataset) 
    })
    output$upload_data_standard_combined_seasonal_display <- renderDataTable(upload_data_standard_combined_seasonal(),
                                                                             options = list(scrollX = TRUE))
    #for downloading seasonal dataset
    output$download_data_standard_combined_seasonal <- downloadHandler(
        filename = "Aquacrop_standard_seasonal_combined_data.tsv",
        content = function(file) {
            write_tsv(upload_data_standard_combined_seasonal(), file)
        }
    )

    ###ggplot _std
    #reactive for showing next boxes to input plotting instructions
    observeEvent(input$plot_next1_std, {
      shinyjs::show(id = "hiddenbox1_std")
    })
    observeEvent(input$plot_next2_std, {
      shinyjs::show(id = "hiddenbox2_std")
    })
    observeEvent(input$plot_next3_std, {
      shinyjs::show(id = "hiddenbox3_std")
    })
    observeEvent(input$plot_next4_std, {
      shinyjs::show(id = "hiddenbox4_std")
    })
    observeEvent(input$plot_next5_std, {
      shinyjs::show(id = "hiddenbox5_std")
    })
    
    #data for plotting
    ##if daily or seasonal data selected
    data_std_use <- reactive({
      if(input$plot_mode_std == "daily"){
        updateSelectInput(inputId = "y_var_std", choices = unique(colnames(upload_data_standard_combined_daily())))
        upload_data_standard_combined_daily()
      }else{
        updateSelectInput(inputId = "y_var_std", choices = unique(colnames(upload_data_standard_combined_seasonal())))
        upload_data_standard_combined_seasonal()        
      }
    })

    
    ## if plotting mean is selected, calculate mean based on grouping variable selected
    data_std_combined_plot <- reactive({
      if(input$use_mean_std == "Yes" & length(input$group_var_std) > 0){
        data_std_use() %>%
          group_by(across(all_of(c(input$group_var_std, "Year1", input$col_var_std, input$shape_var_std)))) %>%
          summarise(across(c("Rain","ETo","GD","CO2","Irri","Infilt","Runoff","Drain","Upflow","E","E/Ex","Tr","TrW","Tr/Trx","SaltIn","SaltOut","SaltUp","SaltProf","Cycle","SaltStr","FertStr","WeedStr","TempStr","ExpStr","StoStr","BioMass","Brelative","HI","Yield","WPet"),
                           mean))
      }else{
        data_std_use() 
      }
    })
    
    ###option for renaming variable
    #create observe event module to monitor if user input select variable to rename
    #if variable selected, update the select input list for value choices of the selected variable
    observeEvent(input$rename_variable_std, {
      choices <- unique(data_std_combined_plot_rename$data[[input$rename_variable_std]])
      updateSelectInput(inputId = "rename_from_std", choices = choices) 
    })
    #change value of selected variable to the value from user
    data_std_combined_plot_rename <- reactiveValues()
    observe({data_std_combined_plot_rename$data <- data_std_combined_plot()})
    observeEvent(input$rename_button_std, {
      if(input$rename_to_std != ""){
        rename_df <- data_std_combined_plot_rename$data
        rename_df[input$rename_variable_std][rename_df[input$rename_variable_std] == input$rename_from_std] <- input$rename_to_std
        data_std_combined_plot_rename$data <- rename_df
      }
      choices <- unique(data_std_combined_plot_rename$data[[input$rename_variable_std]])
      updateSelectInput(inputId = "rename_from_std", choices = choices) 
    })
    #output datatable of the combined data and parameters
    output$data_std_combined_plot_rename_display <- renderDataTable(datatable(data_std_combined_plot_rename$data, 
                                                                              options = list(scrollX = TRUE)))
    
    
    #set color palette
    custom_palette_std <- reactive({
      default_palette_std <- c("#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
      
      #vector of available color choices to form custom palette
      color_choice_hex_std <- c("#000000","#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E41A1C","#A6D854")
      names(color_choice_hex_std) <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
      
      #make palette from custom colors selected from user
      if(length(input$col_palette_std) > 0){
        palette <- color_choice_hex_std[input$col_palette_std]
      }else{
        palette <- default_palette_std
      }
      unname(palette)
    })
    
    #set shape palette
    custom_shape_std <- reactive({
      default_shape_std <- c(16,17,15,18,4,1,2,0,5)
      
      #vector of available shape choices to form custom palette
      shape_choice_std <- c(16,17,15,18,4,1,2,0,5)
      names(shape_choice_std) <- c("circle", "triangle", "rectangle", "diamond", "cross", "hollow_circle", "hollow_triangle", "hollow_rectangle", "hollow_diamond")
      
      
      #make palette from custom shapes selected from user
      if(length(input$shape_palette_std) > 0){
        shape.palette <- shape_choice_std[input$shape_palette_std]
      }else{
        shape.palette <- default_shape_std
      }
      unname(as.numeric(shape.palette)) 
    })
    
    #set legend direction
    legend_direction_std <- reactive({
      if(input$legend_position_std %in% c("top","bottom")){
        "horizontal"
      } else{
        "vertical"
      }
    })
    
    ggplot_std <- reactive({
      
      #select x axis according to data mode
        if(input$plot_mode_std == "daily"){
          x_var_plot = "date"
        }else{
          x_var_plot = "Year1"
        }
        
      #initial plot according to selected coloring and group variable
      if(length(input$shape_var_std) > 0 & length(input$col_var_std) > 0){
        p <- ggplot(data = data_std_combined_plot_rename$data, aes(x = .data[[x_var_plot]], y = .data[[input$y_var_std]], group = interaction(.data[[input$shape_var_std]], .data[[input$col_var_std]]), col = .data[[input$col_var_std]], shape = .data[[input$shape_var_std]]))
      }
      else if(length(input$col_var_std) > 0){
        p <- ggplot(data = data_std_combined_plot_rename$data, aes(x = .data[[x_var_plot]], y = .data[[input$y_var_std]], group = .data[[input$col_var_std]], col = .data[[input$col_var_std]]))
      }
      else if(length(input$shape_var_std) > 0){
        p <- ggplot(data = data_std_combined_plot_rename$data, aes(x = .data[[x_var_plot]], y = .data[[input$y_var_std]], group = .data[[input$shape_var_std]], shape = .data[[input$shape_var_std]]))
      }
      else{
        p <- ggplot(data = data_std_combined_plot_rename$data, aes(x = .data[[x_var_plot]], y = .data[[input$y_var_std]]))
      }
      #add plot
      p <- p +
        theme(axis.title = element_text(size = as.numeric(input$font_size_axis_title_std)), 
              axis.text = element_text(size = as.numeric(input$font_size_axis_text_std)),
              legend.title = element_text(size = as.numeric(input$font_size_legend_std)),
              legend.text = element_text(size = as.numeric(input$font_size_legend_std)),
              strip.text = element_text(size = as.numeric(input$font_size_facet_std)),
              plot.title = element_text(size = as.numeric(input$font_size_plot_title_std)),
              legend.position = paste(input$legend_position_std),
              legend.direction = paste(legend_direction_std()),
              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
              panel.background = element_rect(colour = "black", fill = "white"),
              plot.background = element_rect(colour = NA, fill = "white"),
              axis.line = element_line(colour="black",size=0.1),
              axis.ticks = element_line(),
              axis.title.x = element_text(vjust = -2.5),
              axis.title.y = element_text(vjust = +2.5),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size= unit(0.75, "cm"),
              strip.background=element_rect(colour="#000000",fill=NA),
              plot.margin=unit(c(10,5,5,5),"mm")
        ) +
        scale_color_manual(values=custom_palette_std()) +
        scale_shape_manual(values=custom_shape_std()) +
        guides(color = guide_legend(override.aes = list(size=3)))
      
      
      #select facet variable
      if(length(input$facet_var_std) == 1){
        p <- p + facet_wrap(~get(input$facet_var_std[1]))
      } else if(length(input$facet_var_std) == 2){
        p <- p + facet_grid(get(input$facet_var_std[2])~get(input$facet_var_std[1]))
      } else {
        p <- p
      }
      
      #select plotting elements (geom)
      if("point" %in% input$plot_element_std){
        p <- p + geom_point(size = as.numeric(input$point_size_std))
      }     
      if("line" %in% input$plot_element_std){
        p <- p + geom_line()
      }
      if("linear_trend" %in% input$plot_element_std){
        p <- p + geom_smooth(method="lm", se = F, show.legend = FALSE)
      }
      if("linear_trend_error" %in% input$plot_element_std){
        p <- p + geom_smooth(method="lm", se = T, show.legend = FALSE)
      }
      if("grid_line" %in% input$plot_element_std){
        p <- p + theme(panel.grid.major = element_line(colour="#f0f0f0"),
                       panel.grid.minor = element_blank())
      }else{
        p <- p + theme(panel.grid.major = element_blank())
      }
      if(length(input$plot_element_std) == 0){
        p <- p
      }
      
      #add custom text for axis label
      if(nchar(input$y_var_label_std) > 0){
        p <- p + labs(y = paste(input$y_var_label_std))
      }
      if(nchar(input$x_var_label_std) > 0){
        p <- p + labs(x = paste(input$x_var_label_std))
      }
      if(nchar(input$title_label_std) > 0){
        p <- p + labs(title = paste(input$title_label_std))
      }
      if(nchar(input$legend_label_std) > 0){
        p <- p + labs(title = paste(input$legend_label_std))
      }
      print(p)
    })
    
    #adjust default plot size according to facets
    #select facet variable
    observeEvent(input$facet_var_std, {
      if(length(input$facet_var_std) == 1){
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) == 1){
          updateTextInput(session, "export_plot_width_std", value = "19")
          updateTextInput(session,"export_plot_height_std", value = "12")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) == 2){
          updateTextInput(session, "export_plot_width_std", value = "29")
          updateTextInput(session,"export_plot_height_std", value = "12")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) > 2){
          updateTextInput(session, "export_plot_width_std", value = "39")
          updateTextInput(session,"export_plot_height_std", value = "12")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) > 3){
          updateTextInput(session, "export_plot_width_std", value = "39")
          updateTextInput(session,"export_plot_height_std", value = "23")
        }
      } 
      if(length(input$facet_var_std) == 2){
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) == 1){
          updateTextInput(session,"export_plot_width_std", value = "19")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) == 2){
          updateTextInput(session,"export_plot_width_std", value = "29")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[1]]])) > 2){
          updateTextInput(session,"export_plot_width_std", value = "39")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[2]]])) == 1){
          updateTextInput(session,"export_plot_height_std", value = "12")
        }
        if(length(unique(data_std_combined_plot_rename$data[[input$facet_var_std[2]]])) > 1){
          updateTextInput(session,"export_plot_height_std", value = "23")
        }
      }
    })
    
    
    #render ggplot display in app
    output$ggplot_std_display <- renderPlot({
      ggplot_std()
    },width=exprToFunction(as.numeric(input$export_plot_width_std)*36), height=exprToFunction(as.numeric(input$export_plot_height_std)*36))
    
    
    #for downloading ggplot
    output$ggplot_std_download <- downloadHandler(
      filename = function() {"plot_std"},
      content = function(file) {
        ggsave(file, plot = ggplot_std(), device = {{input$export_plot_format_std}} , width = as.numeric({{input$export_plot_width_std}}), height = as.numeric({{input$export_plot_height_std}}), units = "cm")
      }
    )

    
###################################################################################
    ##########plugin
    ###seasonal data
    ###read upload data files and combine all data into dataframe
    upload_data_combined <-
        reactive({
            #require uploaded data files before evaluating
            req(input$upload_data_files)
            #check to make sure uploaded files has the correct extension .OUT, return error if not 
            upload_data_files_ext <- str_detect(input$upload_data_files$name, "season\\.OUT$")
            if(!any(upload_data_files_ext)){
              validate("Invalid input for seasonal data: season.OUT files needed")
            }
            
            #set up parallel processing for future_map function
            plan(multisession, workers = 2) 
            
            #get a list of file paths from uploaded files
            data.df = input$upload_data_files %>%
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
                    data = read_tsv(file = file.clean, skip = 4, col_names = heading) %>%
                        select(-1) #remove blank column in the first position
                })) %>%
                unnest(dataset) %>%
                select(-size, -type, -datapath)
        })
    #output datatable of the combined data
    output$upload_data_combined_display <- renderDataTable(upload_data_combined() %>%
                                                           select(name) %>%
                                                           distinct(),
                                                           options = list(scrollX = TRUE))
    
    ###read uploaded parameter files and combine
    upload_prm_combined <-
        reactive({
            #require uploaded prm files before evaluating
            req(input$upload_prm_files)
            #check to make sure at least some uploaded files has the correct extension .prm, return error if not 
            upload_prm_files_ext <- str_detect(input$upload_prm_files$name, "\\.PRM$")
            if(!any(upload_prm_files_ext)){
              validate("Invalid input for parameter data: .PRM files needed")
            }
            
            #set up parallel processing for future_map function
            plan(multisession, workers = 2) 
            
            #get a list of prm file path from uploaded
            prm.df = input$upload_prm_files %>%
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
                    #put parameters together in a table
                    param.table = data.frame(climate.file, temperature.file, reference.ET.file, rain.file, co2.file, crop.file, irrigation.file, field.management.file, soil.file, groundwater.table.file, initial.condition.file, offseason.condition.file)
                })) %>%
                unnest(parameter) %>%
                select(-size, -type, -datapath) %>%
                mutate(irrigation.file = ifelse(is.na(irrigation.file), "rainfed", irrigation.file)) %>%
                mutate(name.variable = str_replace(name, "\\.PRM$","")) %>%
                rename(prm.file.name = name)

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
                                                          options = list(scrollX = TRUE))
    
    #option for renaming column name
    #create observe event module to monitor if user input select variable to rename
    #if variable selected, update the select input list for value choices of the selected variable
    upload_prm_combined_renamecol <- reactiveValues()
    observe({upload_prm_combined_renamecol$data <- upload_prm_combined()})
    observe({
      choices <- colnames(upload_prm_combined_renamecol$data)
      choices <- choices[! choices %in% c("name.variable")]
      updateSelectInput(inputId = "rename_col_from", choices = choices) 
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
      updateSelectInput(inputId = "rename_col_from", choices = choices) 
    })
    
    #output datatable of the combined parameters
    output$prm_combined_display <- renderDataTable(datatable(upload_prm_combined_renamecol$data, 
                                                                  options = list(scrollX = TRUE)))
    #for downloading combined prm
    output$download_combined_prm <- downloadHandler(
      filename = "Aquacrop_combined_parameter.tsv",
      content = function(file) {
        write_tsv(upload_prm_combined_renamecol$data, file)
      }
    )
    
    ####add parameters to the output dataset
    data_prm_combined <- reactive({
      req(input$upload_prm_files)
      
        upload_data_combined() %>%
        mutate(sowing.dmy = dmy(paste(Day1, Month1, Year1, sep="-"))) %>%
        mutate(sowing.date = paste(day(sowing.dmy), month(sowing.dmy, label = T), sep = "_")) %>%
        mutate(name.variable = str_replace(name, "PRMseason.OUT$","")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
        })
    
   
    #output datatable of the combined data and parameters
    output$data_prm_combined_display <- renderDataTable(datatable(data_prm_combined(), 
                                                                  options = list(scrollX = TRUE)))
    #for downloading combined dataset
    output$download_combined_dataset <- downloadHandler(
      filename = "Aquacrop_combined_data.tsv",
      content = function(file) {
        write_tsv(data_prm_combined(), file)
      }
    )
    
    ###daily data
    upload_daily_data_combined <-
      reactive({
        #require uploaded data files before evaluating
        req(input$upload_daily_data_files)
        #check to make sure uploaded files has the correct extension .OUT, return error if not 
        upload_daily_data_files_ext <- str_detect(input$upload_daily_data_files$name, "day\\.OUT$")
        if(!any(upload_daily_data_files_ext)){
          validate("Invalid input for daily data: day.OUT files needed")
        }
        
        #set up parallel processing for future_map function
        plan(multisession, workers = 2) 
        
        #get a list of file paths from uploaded files
        data.df = input$upload_daily_data_files %>%
          #filter to read only seasonal.out files
          filter(str_detect(name, "day\\.OUT$")) %>% 
          #import dataset and clean up, format into dataframe
          mutate(dataset = future_map(datapath, function(datapath){
            ###read in one output file for daily data 
            #read in data as lines, clean up spaces to allow reading as tsv
            file.clean = read_lines(datapath) %>%
              str_replace_all(" +?(?=\\S)","\t") 
            
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
            
            #read in data as tsv
            data = read_tsv(file = file.clean, col_names = heading) %>%
              select(-1) #remove blank column at the start
            
            #remove duplicated column
            colnames(data) = str_replace(colnames(data),"\\.\\.\\..+$","")
            data = data[!duplicated(colnames(data))]
              
          })) %>%
          unnest(dataset) %>%
          select(-size, -type, -datapath)
          })
    
    #output datatable of the combined data
    output$upload_daily_data_combined_display <- renderDataTable(upload_daily_data_combined() %>%
                                                             select(name) %>%
                                                             distinct(),
                                                           options = list(scrollX = TRUE))
    
    
    
    ####add parameters to the daily dataset
    daily_data_prm_combined <- reactive({
      req(input$upload_prm_files)

      upload_daily_data_combined() %>%    
      mutate(date = dmy(paste(Day, Month, Year, sep="-"))) %>% 
      mutate(name.variable = str_replace(name, "PRMday.OUT$","")) %>%
      left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    })
    #output datatable of the combined daily data and parameters
    output$daily_data_prm_combined_display <- renderDataTable(datatable(daily_data_prm_combined(), 
                                                                  options = list(scrollX = TRUE)))
    #for downloading combined daily dataset
    output$download_combined_daily_dataset <- downloadHandler(
      filename = "Aquacrop_combined_daily_data.tsv",
      content = function(file) {
        write_tsv(daily_data_prm_combined(), file)
      }
    )
    
    ###check if all parameter .PRM files are uploaded as needed for all .OUT datasets
    #find a list of any missing prm file required in the data files
    missing_prm_file <- reactive({
      req(input$upload_data_files)
      req(input$upload_prm_files)
      req(input$upload_daily_data_files)
      
      bind_rows(upload_data_combined() %>%
        mutate(name.variable = str_replace(name, "PRMseason.OUT$","")) %>%
        select(name.variable), 
      upload_daily_data_combined() %>%
        mutate(name.variable = str_replace(name, "PRMday.OUT$","")) %>%
        select(name.variable)) %>%
        distinct() %>%
        anti_join(upload_prm_combined(), by = "name.variable") %>% 
        as.vector()
    })
    #return error if there is any missing prm files
    output$missing_prm_file_error <- reactive({
      req(input$upload_data_files)
      req(input$upload_prm_files)
      req(input$upload_daily_data_files)
      
      if(length(missing_prm_file()) > 0){
        validate(paste0("The following .PRM files are missing:\n", paste(missing_prm_file()[["name.variable"]], collapse=", ")))
      }
    })
    
############### ggplot ###############

    #reactive for showing next boxes to input plotting instructions
    observeEvent(input$plot_next1, {
      shinyjs::show(id = "hiddenbox1")
    })
    observeEvent(input$plot_next2, {
      shinyjs::show(id = "hiddenbox2")
    })
    observeEvent(input$plot_next3, {
      shinyjs::show(id = "hiddenbox3")
    })
    observeEvent(input$plot_next4, {
      shinyjs::show(id = "hiddenbox4")
    })
    observeEvent(input$plot_next5, {
      shinyjs::toggle(id = "hiddenbox5")
    })
    
    ###data for plotting
    ##select data mode daily or seasonal  
    data_mode_selected <- reactive({
      req(input$upload_data_files)
      req(input$upload_prm_files)
      req(input$upload_daily_data_files)
      
      if(input$plot_mode == "daily"){
        #update choices for plotting axis
        axis.choices = unique(colnames(daily_data_prm_combined()))
        updateSelectInput(inputId = "y_var", choices = axis.choices)
        updateSelectInput(inputId = "x_var", choices = axis.choices)
        #update choices for grouping variable
        group.choices <- setdiff(colnames(daily_data_prm_combined()), colnames(upload_daily_data_combined()))
        updateSelectizeInput(inputId = "group_var", choices = group.choices) 
        updateSelectizeInput(inputId = "col_var", choices = group.choices) 
        updateSelectizeInput(inputId = "shape_var", choices = group.choices) 
        updateSelectizeInput(inputId = "facet_var", choices = group.choices) 
        #return data to use
        daily_data_prm_combined()
      }else{
        #update choices for plotting axis
        axis.choices = unique(colnames(data_prm_combined()))
        updateSelectInput(inputId = "y_var", choices = axis.choices)
        updateSelectInput(inputId = "x_var", choices = axis.choices)
        #update choices for grouping variable
        group.choices <- setdiff(colnames(data_prm_combined()), colnames(upload_data_combined()))
        updateSelectizeInput(inputId = "group_var", choices = group.choices) 
        updateSelectizeInput(inputId = "col_var", choices = group.choices) 
        updateSelectizeInput(inputId = "shape_var", choices = group.choices) 
        updateSelectizeInput(inputId = "facet_var", choices = group.choices) 
        #return data to use
        data_prm_combined()     
      }
    })

    #when changes made to dataframe, update list of variables for axis and grouping
    observeEvent(daily_data_prm_combined(),{
      #update choices for plotting axis
      axis.choices = unique(colnames(daily_data_prm_combined()))
      updateSelectInput(inputId = "y_var", choices = axis.choices)
      updateSelectInput(inputId = "x_var", choices = axis.choices)
      #update choices for grouping variable
      group.choices <- setdiff(colnames(daily_data_prm_combined()), colnames(upload_daily_data_combined()))
      updateSelectizeInput(inputId = "group_var", choices = group.choices) 
      updateSelectizeInput(inputId = "col_var", choices = group.choices) 
      updateSelectizeInput(inputId = "shape_var", choices = group.choices) 
      updateSelectizeInput(inputId = "facet_var", choices = group.choices) 
    })
    observeEvent(data_prm_combined(),{
      #update choices for plotting axis
      axis.choices = unique(colnames(data_prm_combined()))
      updateSelectInput(inputId = "y_var", choices = axis.choices)
      updateSelectInput(inputId = "x_var", choices = axis.choices)
      #update choices for grouping variable
      group.choices <- setdiff(colnames(data_prm_combined()), colnames(upload_data_combined()))
      updateSelectizeInput(inputId = "group_var", choices = group.choices) 
      updateSelectizeInput(inputId = "col_var", choices = group.choices) 
      updateSelectizeInput(inputId = "shape_var", choices = group.choices) 
      updateSelectizeInput(inputId = "facet_var", choices = group.choices) 
    })
    
    
    ## if plotting mean is selected, calculate mean based on grouping variable selected
      data_prm_combined_plot <- reactive({
        req(input$upload_data_files)
        req(input$upload_prm_files)
        req(input$upload_daily_data_files)
        
        if(input$use_mean == "Yes" & length(input$group_var) > 0){
          data_mode_selected() %>%
            group_by(across(all_of(c(input$group_var, input$x_var, input$col_var, input$shape_var)))) %>%
            summarise(across(where(is.numeric),  ~ mean(.x, na.rm = TRUE)))
        }else{
          data_mode_selected()
        }
      })
      
      ###option for renaming variable
      #create observe event module to monitor if user input select variable to rename
      #if variable selected, update the select input list for value choices of the selected variable
      observeEvent(input$rename_variable, {
        choices <- unique(data_prm_combined_plot_rename$data[[input$rename_variable]])
        updateSelectInput(inputId = "rename_from", choices = choices) 
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
        updateSelectInput(inputId = "rename_from", choices = choices) 
      })
      #output datatable of the combined data and parameters
      output$data_prm_combined_plot_rename_display <- renderDataTable(datatable(data_prm_combined_plot_rename$data, 
                                                                    options = list(scrollX = TRUE)))
      
      
    #set color palette
    custom_palette <- reactive({
      default_palette <- c("#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
      
      #vector of available color choices to form custom palette
      color_choice_hex <- c("#000000","#999999", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#E41A1C","#A6D854")
      names(color_choice_hex) <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
      
      #make palette from custom colors selected from user
      if(length(input$col_palette) > 0){
        palette <- color_choice_hex[input$col_palette]
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
        shape.palette <- shape_choice[input$shape_palette]
      }else{
        shape.palette <- default_shape
      }
      unname(as.numeric(shape.palette)) 
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
      #initial plot according to selected coloring and group variable
      if(length(input$shape_var) > 0 & length(input$col_var) > 0){
        p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = interaction(.data[[input$shape_var]], .data[[input$col_var]]), col = .data[[input$col_var]], shape = .data[[input$shape_var]]))
      }
      else if(length(input$col_var) > 0){
        p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]]))
      }
      else if(length(input$shape_var) > 0){
        p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$shape_var]], shape = .data[[input$shape_var]]))
      }
      else{
        p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]]))
      }
      #add plot
      p <- p +
        theme(axis.title = element_text(size = as.numeric(input$font_size_axis_title)), 
              axis.text = element_text(size = as.numeric(input$font_size_axis_text)),
              legend.title = element_blank(),
              #legend.title = element_text(size = as.numeric(input$font_size_legend)),
              legend.text = element_text(size = as.numeric(input$font_size_legend)),
              strip.text = element_text(size = as.numeric(input$font_size_facet)),
              plot.title = element_text(size = as.numeric(input$font_size_plot_title)),
              legend.position = paste(input$legend_position),
              legend.direction = paste(legend_direction()),
              axis.text.x = element_text(angle = as.numeric(input$x_axis_label_angle), hjust = 1, vjust = 1),
              panel.background = element_rect(colour = "black", fill = "white"),
              plot.background = element_rect(colour = NA, fill = "white"),
              axis.line = element_line(colour="black",size=0.1),
              axis.ticks = element_line(),
              axis.title.x = element_text(vjust = -2.5),
              axis.title.y = element_text(vjust = +2.5),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size= unit(0.75, "cm"),
              strip.background=element_rect(colour="#000000",fill=NA),
              plot.margin=unit(c(10,5,5,5),"mm")
              ) +
        scale_color_manual(values=custom_palette()) +
        scale_shape_manual(values=custom_shape()) +
        guides(color = guide_legend(override.aes = list(size=3)))+
        xlab("Year")
      
      
      #select facet variable
      if(length(input$facet_var) == 1){
        p <- p + facet_wrap(~get(input$facet_var[1]))
      } else if(length(input$facet_var) == 2){
        p <- p + facet_grid(get(input$facet_var[2])~get(input$facet_var[1]))
      } else {
        p <- p
      }
      
      #select plotting elements (geom)
      if("point" %in% input$plot_element){
        p <- p + geom_point(size = as.numeric(input$point_size))
      }     
      if("line" %in% input$plot_element){
        p <- p + geom_line()
      }
      if("linear_trend" %in% input$plot_element){
        p <- p + geom_smooth(method="lm", se = F, show.legend = FALSE)
      }
      if("linear_trend_error" %in% input$plot_element){
        p <- p + geom_smooth(method="lm", se = T, show.legend = FALSE)
      }
      if("grid_line" %in% input$plot_element){
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
      if(nchar(input$legend_label) > 0){
        p <- p + labs(title = paste(input$legend_label))
      }
      print(p)
    })
    
    #adjust default plot size according to facets
    #select facet variable
    observeEvent(input$facet_var, {
      if(length(input$facet_var) == 1){
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) == 1){
          updateTextInput(session, "export_plot_width", value = "19")
          updateTextInput(session,"export_plot_height", value = "12")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) == 2){
          updateTextInput(session, "export_plot_width", value = "29")
          updateTextInput(session,"export_plot_height", value = "12")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) > 2){
          updateTextInput(session, "export_plot_width", value = "39")
          updateTextInput(session,"export_plot_height", value = "12")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) > 3){
          updateTextInput(session, "export_plot_width", value = "39")
          updateTextInput(session,"export_plot_height", value = "23")
        }
      } 
      if(length(input$facet_var) == 2){
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) == 1){
          updateTextInput(session,"export_plot_width", value = "19")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) == 2){
          updateTextInput(session,"export_plot_width", value = "29")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[1]]])) > 2){
          updateTextInput(session,"export_plot_width", value = "39")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[2]]])) == 1){
          updateTextInput(session,"export_plot_height", value = "12")
        }
        if(length(unique(data_prm_combined_plot_rename$data[[input$facet_var[2]]])) > 1){
          updateTextInput(session,"export_plot_height", value = "23")
        }
      }
    })

    
    #render ggplot display in app
    output$ggplot_plugin_display <- renderPlot({
      ggplot_plugin()
    },width=exprToFunction(as.numeric(input$export_plot_width)*36), height=exprToFunction(as.numeric(input$export_plot_height)*36))
    
    
    #for downloading ggplot
    output$ggplot_plugin_download <- downloadHandler(
      filename = function() {"plot_plugin"},
      content = function(file) {
        ggsave(file, plot = ggplot_plugin(), device = {{input$export_plot_format}} , width = as.numeric({{input$export_plot_width}}), height = as.numeric({{input$export_plot_height}}), units = "cm")
      }
    )
    
    
###### Analysis ####    
    
###time period window analysis
    
    #update window slider input for year, set min max according to data
    observeEvent(data_prm_combined(), {
      year.range <- max(data_prm_combined()[["Year1"]]) - min(data_prm_combined()[["Year1"]])
      updateSliderInput(inputId = "time_period", min = 1, max = year.range)
    })
    #update variable choice
    observeEvent(data_prm_combined(), {
      choices <- setdiff(colnames(upload_data_combined()), c("name","RunNr","Day1","Month1","Year1","DayN","MonthN","YearN","prm.file"))
      updateSelectInput(inputId = "time_period_variable", choices = choices)
    })
    #update grouping choice
    observeEvent(data_prm_combined(), {
      group.choices <- setdiff(colnames(data_prm_combined()), colnames(upload_data_combined()))
      updateSelectInput(inputId = "time_period_group", choices = group.choices)
    })
    
    #calculate summary in time period selected
    data_prm_combined_timeperiod <- reactive({
      req(input$upload_data_files)
      req(input$upload_prm_files)
      req(input$upload_daily_data_files)

      #cut time into windows
      column.select <- c("time.window", input$time_period_variable, input$time_period_group)
      column.group <- c(input$time_period_group, "time.window")
      
      #calculate summary
      data_prm_combined() %>%
        mutate(time.window = cut(Year1,
                                 unique(c(seq(min(Year1), max(Year1), input$time_period),min(Year1), max(Year1))),
                                 include.lowest = TRUE, right = FALSE, dig.lab = 4)) %>%
        mutate(time.window = str_replace_all(time.window,"\\[|\\)","")) %>%
        separate(time.window, c("start", "end"), sep = ",") %>%
        mutate(end = ifelse(str_detect(end, "\\]"), as.numeric(str_replace(end, "\\]",""))+1, end))%>%
        mutate(time.window = paste0(start, "-", as.numeric(end)-1)) %>%
        select(all_of(column.select)) %>%
        group_by(across(all_of(column.group))) %>%
        summarise(mean = mean(.data[[input$time_period_variable]]) %>% round(3),
                  SD = sd(.data[[input$time_period_variable]]) %>% round(3),
                  n = n()) %>%
        rename_with(~paste0(input$time_period_variable, ".", .x), c(mean, SD))
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

 
}

# Run the application 
shinyApp(ui = ui, server = server)

