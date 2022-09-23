# p_load install the packages if not present
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,shinydashboard, tidyverse, DT, lubridate, shinyjs, shinyBS, furrr, broom)

#sets of input variables to select for plotting
input_plot_x_variable <- c("Year1")
input_plot_y_variable <- c("Rain","ExpStr","E","ETo","Irri","StoStr","Yield","WPet")
input_group_variable <- c("climate","location","rcp","irrigation","crop","soil","sowing.date")
input_plot_variable_standard <- c("Biomass", "Date")
input_color_choice <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
input_plot_element_choice <- c("point", "line", "linear_trend", "linear_trend_error","background_grid")
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
            menuItem("Workflow", tabName = "aquacrop_plugin", icon = icon("list-alt"), startExpanded = TRUE,
                     menuSubItem("Upload_data", tabName = "tab_upload_data", icon = icon("caret-right")),
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
                        selectizeInput("standard_vs_plugin_select", "AquaCrop programme used", choices = c("standard","plugin"), multiple = TRUE, options = list(maxItems = 1)),
                        
                        #show upload data according to mode selected
                        #plugin
                        fluidRow(
                          conditionalPanel(condition = "input.standard_vs_plugin_select == 'plugin'",
                                           #display boxes for data and prm files upload
                                             box(title = "Batch upload all files", status = "primary", solidHeader = TRUE, width = 12,
                                                 fileInput("upload_all_files", "Upload all files (season.OUT, day.OUT, .PRM)", multiple = TRUE),
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
                                             ),
                                             box(title = "Parameter files", status = "primary", solidHeader = TRUE, width = 4,
                                                 #upload parameter file
                                                 #fileInput("upload_prm_files", "Add more files (.PRM)", multiple = TRUE, accept = ".PRM"),
                                                 div(dataTableOutput("upload_prm_combined_display"), style = "font-size: 75%; width: 100%"),
                                                 dataTableOutput("missing_prm_file_error")
                                             )
                                           )
                        ),
                        #standard
                        fluidRow(
                          conditionalPanel(condition = "input.standard_vs_plugin_select == 'standard'",
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
                        ),
                        tabPanel(title = "Parameter",
                            width = 12,
                            status = "primary",
                            solidHeader = FALSE,
                            #button for downloading all combined data
                            downloadButton("download_combined_prm", "Download", style = "margin-bottom: 15px; "),
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
                                selectizeInput("plot_mode", "Data type to plot", c("daily","seasonal"), multiple = TRUE, options = list(maxItems = 1)),
                                selectizeInput("y_var", "Variable to plot on Y axis", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                selectizeInput("x_var", "Variable to plot on X axis", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
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
                                  bsPopover(id = "plot_info1", title = "If Yes, Plot will use only mean values summarised from all data points within the grouping variable", placement = "right", trigger = "hover"),
                                  conditionalPanel(condition = "input.use_mean == 'Yes'",
                                                  selectizeInput("group_var", 
                                                   label = tags$span("Select variable to group for calculating mean",  bsButton("plot_info2", label = "", icon = icon("info"), size = "extra-small")), 
                                                   choices = NULL,
                                                   multiple = TRUE),
                                    bsPopover(id = "plot_info2", title = "select any number of variables for grouping data before summarising as mean", placement = "right", trigger = "hover")),
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
                                            choices = NULL,
                                            multiple = TRUE, options = list(maxItems = 1)),
                                bsPopover(id = "plot_info3", title = "Each value of the selected variable will be plotted in different shape", placement = "right", trigger = "hover"),
                                selectizeInput("shape_var", 
                                               label = tags$span("Variable to split into shapes by",  bsButton("plot_info7", label = "", icon = icon("info"), size = "extra-small")), 
                                               choices = NULL,
                                               multiple = TRUE, options = list(maxItems = 1)),
                                bsPopover(id = "plot_info7", title = "Each value of the selected variable will be plotted in different shape", placement = "right", trigger = "hover"),
                                selectizeInput("facet_var", 
                                               label = tags$span("Variable to split into subpanels by", bsButton("plot_info4", label = "", icon = icon("info"), size = "extra-small")), 
                                               choices = NULL,
                                               multiple = TRUE, options = list(maxItems = 2)),
                                bsPopover(id = "plot_info4", title = "Selected variable will be used to split plot into subplots. maximum of 2 variables can be selected", placement = "right", trigger = "hover"),
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
                                               label = tags$span("Components of plot to show", bsButton("plot_info5", label = "", icon = icon("info"), size = "extra-small")), 
                                               input_plot_element_choice, multiple = TRUE, selected = c("point", "linear_trend","background_grid")),
                                bsPopover(id = "plot_info5", title = "Select or delete any number of components to show in the plot", placement = "right", trigger = "hover"),
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
                                bsPopover(id = "plot_info6", title = "Select the same number of colors as the number of values within the selected variable, in order", placement = "right", trigger = "hover"),
                                selectizeInput("shape_palette", 
                                               label = tags$span("shape palette", bsButton("plot_info8", label = "", icon = icon("info"), size = "extra-small")), 
                                               input_shape_choice, multiple = TRUE),
                                bsPopover(id = "plot_info8", title = "Select the same number of shapes as the number of values within the selected variable, in order", placement = "right", trigger = "hover"),
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
                                textInput("x_axis_label_angle", "X axis label angle", value = "0")
                                ),
                            box(title = "Rename variables",
                                width = 2,
                                height = "550px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("rename_variable", "Select variable to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                selectizeInput("rename_from", "Select value to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                textInput("rename_to", "Rename to"),
                                actionButton("rename_button", "Rename")
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
                               tabPanel(title = "Time period window",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        sliderInput("time_period", label = "Select time period window (years)", min = 1, max = 1, value = 1, step = 1),
                                        selectizeInput("time_period_variable", label = "Select variable to calculate summary", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                        selectizeInput("time_period_group", label = "Select grouping variable", choices = NULL, multiple = TRUE),
                                        div(dataTableOutput("data_prm_combined_timeperiod_display"), style = "font-size: 75%; width: 100%"),
                                        downloadButton("download_data_prm_combined_timeperiod", "Download")
                               ),
                               tabPanel(title = "Stress duration",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        fluidRow(
                                          column(3, sliderInput("StExp_threshold", label = "Threshold of water stress reducing leaf expansion (StExp)", min = 1, max = 100, value = 1, step = 1, ticks = FALSE)),
                                          column(3, sliderInput("StSto_threshold", label = "Threshold of water stress inducing stomatal closure (StSto)", min = 1, max = 100, value = 1, step = 1, ticks = FALSE)),
                                          column(3, sliderInput("StSen_threshold", label = "Threshold of water stress triggering early senescence (StSen)", min = 1, max = 100, value = 1, step = 1, ticks = FALSE)),
                                          column(3, sliderInput("StTr_threshold", label = "Threshold of temperature stress affecting transpiration (StTr)", min = 1, max = 100, value = 1, step = 1, ticks = FALSE))
                                        ),
                                        selectizeInput("stress_group", label = "Select grouping variable", choices = NULL, multiple = TRUE),
                                        selectInput("by_phenological", label = "Separate by phenological stages", choices = c("yes","no"), selected = "no"),
                                        div(dataTableOutput("daily_data_prm_combined_stress_display"), style = "font-size: 75%; width: 100%"),
                                        downloadButton("download_daily_data_prm_combined_stress", "Download"),
                                        actionButton("append_stress_data_button", "Append data to Seasonal dataset for plotting and other analyses", icon = icon("share-square")),

                               ),
                               tabPanel(title = "Regression",
                                        width = 12,
                                        status = "primary",
                                        solidHeader = FALSE,
                                        selectizeInput("regression_mode", "Select data type", c("daily","seasonal"), multiple = TRUE, options = list(maxItems = 1)),
                                        selectizeInput("regression_x_variable", label = "Select independent (X) variable", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                        selectizeInput("regression_y_variable", label = "Select dependent (Y) variable", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
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
            data = read_tsv(file = file.clean, skip = line.skip, n_max = line.n, col_names = heading) %>%
              select(-1) #remove blank column at the start
          })) %>%
          select(-size, -type, -datapath) 
        
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
    
    
    ####add parameters to the seasonal dataset
    data_prm_standard_combined_seasonal <- reactive({
      req(input$upload_data_files_standard)
      
      upload_data_standard_combined_seasonal() %>%
        mutate(sowing.dmy = dmy(paste(Day1, Month1, Year1, sep="-"))) %>%
        mutate(sowing.date = paste(day(sowing.dmy), month(sowing.dmy, label = T), sep = "_")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
    })
    
    
    ####add parameters to the daily dataset
    data_prm_standard_combined_daily <- reactive({
      req(input$upload_data_files_standard)
      
      upload_data_standard_combined_daily() %>%    
        mutate(date = dmy(paste(Day, Month, Year, sep="-"))) %>% 
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
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
            prm.df = input$upload_all_files %>%
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
    
    observe({
      req(input$standard_vs_plugin_select)
      #select if standard or plugin mode used
      if(input$standard_vs_plugin_select == "standard"){
        upload_prm_combined_renamecol$data <- upload_prm_standard_combined()
      }else{
        upload_prm_combined_renamecol$data <- upload_prm_combined()
      }
    })
    
    observe({
      choices <- colnames(upload_prm_combined_renamecol$data)
      choices <- choices[! choices %in% c("name.variable")]
      updateSelectizeInput(inputId = "rename_col_from", choices = choices) 
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
      updateSelectizeInput(inputId = "rename_col_from", choices = choices) 
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
      req(input$standard_vs_plugin_select)
      #select if standard or plugin mode used
      if(input$standard_vs_plugin_select == "standard"){
        data_prm_standard_combined_seasonal()
      }else{
      req(input$upload_all_files)
        upload_data_combined() %>%
        mutate(sowing.dmy = dmy(paste(Day1, Month1, Year1, sep="-"))) %>%
        mutate(sowing.date = paste(day(sowing.dmy), month(sowing.dmy, label = T), sep = "_")) %>%
        mutate(name.variable = str_replace(name, "PRMseason.OUT$","")) %>%
        left_join(upload_prm_combined_renamecol$data, by = "name.variable")
      }
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
        data.df = input$upload_all_files %>%
          #filter to read only day.out files
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
          select(-size, -type, -datapath) %>%
          mutate(Stage = as.factor(Stage))
        })
    
    #output datatable of the combined data
    output$upload_daily_data_combined_display <- renderDataTable(upload_daily_data_combined() %>%
                                                             select(name) %>%
                                                             distinct(),
                                                           options = list(scrollX = TRUE))
    
    
    
    ####add parameters to the daily dataset
    daily_data_prm_combined <- reactive({
      req(input$standard_vs_plugin_select)
      #select if standard or plugin mode used
      if(input$standard_vs_plugin_select == "standard"){
        data_prm_standard_combined_daily()
      }else{
        req(input$upload_all_files)
        
        upload_daily_data_combined() %>%    
          mutate(date = dmy(paste(Day, Month, Year, sep="-"))) %>% 
          mutate(name.variable = str_replace(name, "PRMday.OUT$","")) %>%
          left_join(upload_prm_combined_renamecol$data, by = "name.variable")
      }
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
      req(input$upload_all_files)
      
      bind_rows(upload_data_combined() %>%
        mutate(name.variable = str_replace(name, "PRMseason.OUT$","")) %>%
        select(name.variable), 
      upload_daily_data_combined() %>%
        mutate(name.variable = str_replace(name, "PRMday.OUT$","")) %>%
        select(name.variable)) %>%
        distinct() %>%
        anti_join(upload_prm_combined(), by = "name.variable")
    })
    #return error if there is any missing prm files
    output$missing_prm_file_error <- reactive({
      req(input$upload_all_files)

      
      if(nrow(missing_prm_file()) > 0){
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
      req(input$plot_mode)
      req(input$upload_all_files)
      
      if(input$plot_mode == "daily"){
        #update choices for plotting axis
        axis.choices = unique(colnames(daily_data_prm_combined()))
        updateSelectInput(inputId = "y_var", choices = axis.choices)
        updateSelectInput(inputId = "x_var", choices = axis.choices)
        #update choices for grouping variable
        group.choices <- setdiff(colnames(daily_data_prm_combined()), colnames(upload_daily_data_combined()))
        group.choices <- c(group.choices, "Stage")
        updateSelectizeInput(inputId = "group_var", choices = group.choices) 
        updateSelectizeInput(inputId = "col_var", choices = group.choices) 
        updateSelectizeInput(inputId = "shape_var", choices = group.choices) 
        updateSelectizeInput(inputId = "facet_var", choices = group.choices) 
        updateSelectizeInput(inputId = "rename_variable", choices = group.choices) 
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
        updateSelectizeInput(inputId = "rename_variable", choices = group.choices) 
        #return data to use
        data_prm_combined()     
      }
    })

    observe({
      req(input$plot_mode)
      req(input$upload_all_files)
      
      #update choices for plotting axis
      axis.choices = unique(colnames(data_prm_combined_plot_rename$data))
      updateSelectInput(inputId = "y_var", choices = axis.choices, selected = tail(plot_var_select_cache$y_var, 1))
      updateSelectInput(inputId = "x_var", choices = axis.choices, selected = tail(plot_var_select_cache$x_var, 1))
      #update choices for grouping variable
      if(input$plot_mode == "daily"){
        group.choices <- setdiff(colnames(data_prm_combined_plot_rename$data), colnames(upload_daily_data_combined()))
      }else{
        group.choices <- setdiff(colnames(data_prm_combined_plot_rename$data), colnames(upload_data_combined()))
      }
      updateSelectizeInput(inputId = "group_var", choices = group.choices, selected = tail(plot_var_select_cache$group_var, 1)) 
      updateSelectizeInput(inputId = "col_var", choices = group.choices, selected = tail(plot_var_select_cache$col_var, 1)) 
      updateSelectizeInput(inputId = "shape_var", choices = group.choices, selected = tail(plot_var_select_cache$shape_var, 1)) 
      updateSelectizeInput(inputId = "facet_var", choices = group.choices, selected = tail(plot_var_select_cache$facet_var, 1)) 
      updateSelectizeInput(inputId = "rename_variable", choices = group.choices) 
    })
    
    ## if plotting mean is selected, calculate mean based on grouping variable selected
      data_prm_combined_plot <- reactive({
        req(input$plot_mode)
        req(input$upload_all_files)
        
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
      
     #remember variable set for plotting, so can be recovered after making change to dataframe and plotting engine reactively update, reinitialise
      plot_var_select_cache <- reactiveValues() 
      observe({
        req(input$y_var,input$x_var,input$group_var,input$col_var,input$shape_var,input$facet_var)
        
        plot_var_select_cache$y_var <- input$y_var
        plot_var_select_cache$x_var <- input$x_var
        plot_var_select_cache$group_var <- input$group_var
        plot_var_select_cache$col_var <- input$col_var
        plot_var_select_cache$shape_var <- input$shape_var
        plot_var_select_cache$facet_var <- input$facet_var
      })
      #record everytime that the value change, keep only the most recent previous value and current value
      observeEvent(input$y_var,{plot_var_select_cache$y_var <- c(tail(plot_var_select_cache$y_var, 1), input$y_var)})
      observeEvent(input$x_var,{plot_var_select_cache$x_var <- c(tail(plot_var_select_cache$x_var, 1), input$x_var)})
      observeEvent(input$group_var,{plot_var_select_cache$group_var <- c(tail(plot_var_select_cache$group_var, 1), input$group_var)})
      observeEvent(input$col_var,{plot_var_select_cache$col_var <- c(tail(plot_var_select_cache$col_var, 1), input$col_var)})
      observeEvent(input$shape_var,{plot_var_select_cache$shape_var <- c(tail(plot_var_select_cache$shape_var, 1), input$shape_var)})
      observeEvent(input$facet_var,{plot_var_select_cache$facet_var <- c(tail(plot_var_select_cache$facet_var, 1), input$facet_var)})


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
        p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = interaction(.data[[input$shape_var]], .data[[input$col_var]]), col = .data[[input$col_var]], fill = .data[[input$col_var]], shape = .data[[input$shape_var]]))
      }
      else if(length(input$col_var) > 0){
        p <- ggplot(data = data_prm_combined_plot_rename$data, aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]], fill = .data[[input$col_var]]))
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
              panel.background = element_rect(colour = "black", fill = "white"),
              plot.background = element_rect(colour = NA, fill = "white"),
              axis.line = element_line(colour="black",size=0.1),
              axis.ticks = element_line(),
              axis.title.x = element_text(vjust = -2.5, face = "bold"),
              axis.title.y = element_text(vjust = +2.5, face="bold"),
              legend.key = element_rect(colour = NA, fill = NA),
              legend.key.size= unit(0.75, "cm"),
              strip.background=element_rect(colour="#000000",fill=NA),
              plot.margin=unit(c(10,5,5,5),"mm")
              ) +
        scale_color_manual(values=custom_palette()) +
        scale_shape_manual(values=custom_shape()) +
        guides(color = guide_legend(override.aes = list(size=3)))
      
      #x axis text angle
      if(as.numeric(input$x_axis_label_angle) > 0){
        p <- p + theme(axis.text.x = element_text(angle = as.numeric(input$x_axis_label_angle), hjust = 1, vjust = 1))
      }
      
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
    
###create reactive dataset for analyses (take from seasonal data)
    data_prm_combined_analysis <- reactiveValues()
    observe({data_prm_combined_analysis$data <- data_prm_combined()})
    
###time period window analysis
    
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
      updateSelectizeInput(inputId = "time_period_variable", choices = choices)
    })
    #update grouping choice
    observe({
      group.choices <- setdiff(colnames(data_prm_combined_analysis$data), colnames(upload_data_combined()))
      updateSelectizeInput(inputId = "time_period_group", choices = group.choices)
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

###stress, phenological stage analysis
    
    #update grouping choice
    observe({
      group.choices <- setdiff(colnames(daily_data_prm_combined()), colnames(upload_daily_data_combined()))
      updateSelectizeInput(inputId = "stress_group", choices = group.choices)
    })

    #calculate summary 
    daily_data_prm_combined_stress <- reactive({
      req(input$upload_all_files)
      
      #selecting variables
      
      if(input$by_phenological == "yes"){
        column.group <- c("Year", "Stage", "prm.file.name", input$stress_group)
        column.select <- c("Year","Stage", "StExp", "StSto", "StSen", "StTr","prm.file.name", input$stress_group)
        
      }else{
        column.group <- c("Year", "prm.file.name", input$stress_group)
        column.select <- c("Year", "StExp", "StSto", "StSen", "StTr","prm.file.name", input$stress_group)
        
      }

      #calculate summary
      daily_data_prm_combined() %>%
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
                  StTr.duration.days = length(StTr[which(StTr >= as.numeric(input$StTr_threshold))])
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
      data_prm_combined_analysis$data <- left_join(data_prm_combined_analysis$data %>% select(all_of(setdiff(colnames(data_prm_combined_analysis$data),c("StExp.duration", "StSto.duration", "StSen.duration", "StTr.duration")))),
                                          daily_data_prm_combined_stress() %>% select("prm.file.name", "Year", "StExp.duration", "StSto.duration", "StSen.duration", "StTr.duration"),
                                          by = c("prm.file.name" = "prm.file.name", "Year1"="Year"))
      
      updateSelectizeInput(inputId = "plot_mode", selected = "seasonal")
      data_prm_combined_plot_rename$data  <- left_join(data_prm_combined_plot_rename$data %>% select(all_of(setdiff(colnames(data_prm_combined_plot_rename$data),c("StExp.duration", "StSto.duration", "StSen.duration", "StTr.duration")))),
                                                       daily_data_prm_combined_stress() %>% select("prm.file.name", "Year", "StExp.duration", "StSto.duration", "StSen.duration", "StTr.duration"),
                                                       by = c("prm.file.name" = "prm.file.name", "Year1"="Year"))
    })

######regression
    
    ##select data mode daily or seasonal  
    data_mode_selected_regression <- reactive({
      req(input$regression_mode)
      req(input$upload_all_files)
      
      if(input$regression_mode == "daily"){
        #return data to use
        daily_data_prm_combined()
      }else{
        #return data to use
        data_prm_combined()     
      }
    })
    
    observe({
      req(input$regression_mode)
      req(input$upload_all_files)
      
      if(input$regression_mode == "daily"){
        #update choices for variables
        axis.choices = unique(colnames(daily_data_prm_combined()))
        updateSelectInput(inputId = "regression_y_variable", choices = axis.choices)
        updateSelectInput(inputId = "regression_x_variable", choices = axis.choices)
        #update choices for grouping variable
        group.choices <- setdiff(colnames(daily_data_prm_combined()), colnames(upload_daily_data_combined()))
        group.choices <- c(group.choices, "Stage")
        updateSelectizeInput(inputId = "regression_group", choices = group.choices) 
      }else{
        #update choices for variables
        axis.choices = unique(colnames(data_prm_combined()))
        updateSelectInput(inputId = "regression_y_variable", choices = axis.choices)
        updateSelectInput(inputId = "regression_x_variable", choices = axis.choices)
        #update choices for grouping variable
        group.choices <- setdiff(colnames(data_prm_combined()), colnames(upload_data_combined()))
        updateSelectizeInput(inputId = "regression_group", choices = group.choices) 
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
        mutate(model = map(data, function(data){
          mod <- lm(data = data, as.formula(paste0(input$regression_y_variable,"~",input$regression_x_variable)))
          
          model.r.squared <- glance(mod)[["r.squared"]] %>% signif(digits = 4) %>% format()
          model.p.value <- glance(mod)[["p.value"]] %>% signif(digits = 4) %>% format()
          slope <- tidy(mod)[["estimate"]][[2]] %>% signif(digits = 4) %>% format()
          slope.p.value <- tidy(mod)[["p.value"]][[2]] %>% signif(digits = 4) %>% format() 
          
          summary <- data.frame(model.p.value, model.r.squared,slope,slope.p.value)
        })) %>%
        select(-data) %>%
        unnest(model)
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

