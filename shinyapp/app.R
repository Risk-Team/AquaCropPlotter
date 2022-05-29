library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(shinyjs)
library(shinyBS)

#sets of input variables to select for plotting
input_plot_x_variable <- c("Year1")
input_plot_y_variable <- c("Rain","ExpStr","E","ETo","Irri","StoStr","Yield","WPet")
input_group_variable <- c("climate","location","rcp","irrigation","crop","soil","sowing_date")
input_plot_variable_standard <- c("Biomass", "Date")
input_color_choice <- c("black","grey", "skyblue","orange","green","yellow","blue","vermillion","purple", "red","lightgreen")
input_plot_element_choice <- c("point", "line", "linear_trend", "linear_trend_error","grid_line")
input_legend_pos = c("none","left","right","top","bottom")

#define UI dashboard
ui <- dashboardPage(
    dashboardHeader(title = "ShinyAquaCrop"),
    
    dashboardSidebar(collapsed = FALSE,
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
            menuItem("Legend", tabName = "aquacrop_legend", icon = icon("book"))
        )
    ),
    
    dashboardBody(
        #use shinyjs library
        useShinyjs(),
        #customise fonts and colors in the header and sidebar 
        tags$head(tags$style(HTML(".main-header .logo {font-weight: bold; font-size: 24px;}
                                    .main-sidebar {font-weight: bold; font-size: 22px;}
                                    .treeview-menu>li>a {font-weight: bold; font-size: 22px!important;}
                                    
                                    .skin-blue .main-header .logo {background-color: #416D96;}
                                    .skin-blue .main-header .navbar {background-color: #f2f2f2;}
                                    .skin-blue .main-sidebar {background-color: #9AB7D2; color: #9AB7D2;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #9AB7D2; color: #414042;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #5792c9!important; color: #000000!important;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #5792c9; color: #ffffff;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu .menu-open .active a{background-color: #9AB7D2!important; color: #414042!important;}
                                    .box.box-solid.box-primary>.box-header {background-color: #5792c9;}
                                    .content-wrapper, .right-side {background-color: #f2f2f2;}
                                    
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
        tags$style(type='text/css', ".selectize-input { font-size: 22px; line-height: 22px; width:80%; } 
                                      .selectize-dropdown { font-size: 22px; line-height: 22px; width:80%; }
                                      .control-label { font-size: 22px; line-height: 22px; }
                                      .btn { font-size: 20px; }
                                      .form-control { font-size: 20px; line-height: 20px; height:42px; width:80%; }
                                      .box-title { font-size: 24px!important; line-height: 32px; font-weight:bold; }
                                      .nav-tabs { font-size: 22px; line-height: 22px; font-weight:bold; }
                   "),
        
        tabItems(
            tabItem(tabName = "tab_home",
                    #h2(
                        #fluidRow(
                            imageOutput("aquacrop_logo")
                        #)
                        ,
                        # fluidRow(
                        #     box(title = "Aquacrop standard (single season)", status = "primary", solidHeader = TRUE,
                        #         actionButton("select_aquacrop_standard", "Select Aquacrop standard")
                        #     ),
                        #     box(title = "Aquacrop plug-in (multiple seasons)", status = "primary", solidHeader = TRUE,
                        #         actionButton("select_aquacrop_plugin", "Select Aquacrop plugin")
                        #     )
                        # )
                    #)
            ),
            tabItem(tabName = "tab_upload_data_standard",
                    h2(
                        #display boxes for data and prm files upload
                        fluidRow(
                            box(title = "Data files", status = "primary",
                                #upload data file
                                fileInput("upload_data_files_standard", "Upload data files (.OUT)", multiple = TRUE, accept = ".OUT"),
                                div(dataTableOutput("upload_data_standard_combined_display"), style = "font-size: 75%; width: 100%")
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
                            width = 4,
                            selectInput("y_var_standard", "Select variable to plot on y axis", input_plot_variable_standard, selected = "Biomass"),
                            selectInput("x_var_standard", "Select variable to plot on x axis", input_plot_variable_standard, selected = "Date"))
                      ),
                      fluidRow(
                        downloadButton("ggplot_standard_download", "Download plot"),
                        plotOutput("ggplot_standard_display"),
                        plotlyOutput("ggplotly_standard_display") 
                      )
                    )
            ),
            tabItem(tabName = "tab_upload_data_plugin",
                    h2(
                        #to display error when insufficient prm files are uploaded
                        fluidRow(
                        dataTableOutput("missing_prm_file_error"),
                        tableOutput("missing_prm_file_display")
                        ),
                        #display boxes for data and prm files upload
                        fluidRow(
                            box(title = "Data files", status = "primary", solidHeader = TRUE,
                                #upload data file
                                fileInput("upload_data_files", "Upload files (.OUT)", multiple = TRUE, accept = ".OUT"),
                                div(dataTableOutput("upload_data_combined_display"), style = "font-size: 75%; width: 100%")
                            ),
                            box(title = "Parameter files", status = "primary", solidHeader = TRUE,
                                #upload parameter file
                                fileInput("upload_prm_files", "Upload files (.PRM)", multiple = TRUE, accept = ".PRM"),
                                div(dataTableOutput("upload_prm_combined_display"), style = "font-size: 75%; width: 100%")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_combined_data_plugin",
                    h2(
                        #button for downloading all combined data
                        downloadButton("download_combined_dataset", "Download combined dataset"),
                        #display combined data table
                        div(dataTableOutput("data_prm_combined_display"), style = "font-size: 75%; width: 100%"),
                        #if mean calculated data was used for plotting
                        conditionalPanel(condition = "input.use_mean == 'Yes'", downloadButton("download_combined_plot_dataset", "Download combined dataset with mean")),
                        div(conditionalPanel(condition = "input.use_mean == 'Yes'",dataTableOutput("data_prm_combined_plot_display"), style = "font-size: 75%; width: 100%")),
                        #show renamed data
                        conditionalPanel(condition = "input.rename_to != ''", downloadButton("download_combined_plot_dataset_rename", "Download combined dataset with renamed variable")),
                        div(conditionalPanel(condition = "input.rename_to != ''",dataTableOutput("data_prm_combined_rename_display"), style = "font-size: 75%; width: 100%"))
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
                                selectInput("y_var", "Variable to plot on Y axis", input_plot_y_variable, selected = "Yield"),
                                #selectInput("x_var", "Select variable to plot on X axis", input_plot_x_variable, selected = "Year1"),
                                div(style = "position:absolute;right:1em; bottom:1em;date",actionButton("plot_next1", "Next", icon = icon("chevron-right")))
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
                                  div(style = "position:absolute;right:1em; bottom:1em;",actionButton("plot_next2", "Next", icon = icon("chevron-right")))
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
                                selectizeInput("facet_var", 
                                               label = tags$span("Variable to split into subpanels by", bsButton("plot_info4", label = "", icon = icon("info"), size = "extra-small")), 
                                               choices = input_group_variable,
                                               multiple = TRUE, options = list(maxItems = 2)),
                                bsPopover(id = "plot_info4", title = "selected variable will be used to split plot into subplots. maximum 2 variables can be selected", placement = "right", trigger = "hover"),
                                div(style = "position:absolute;right:1em; bottom:1em;",actionButton("plot_next3", "Next", icon = icon("chevron-right")))
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
                                               input_plot_element_choice, multiple = TRUE, selected = c("point", "linear_trend")),
                                bsPopover(id = "plot_info5", title = "", placement = "right", trigger = "hover"),
                                div(style = "position:absolute;right:1em; bottom:1em;",actionButton("plot_next4", "Plot", icon = icon("chevron-right")))
                                )
                            ))
                        ),
                        shinyjs::hidden(div(id = "hiddenbox4",
                          fluidRow(
                            tabBox(width = 12,
                                   height = "1400px",
                                   id = "plugin_plot_tabbox",
                                   tabPanel("Standard plot",
                                            plotOutput("ggplot_plugin_display")
                                            ),
                                   tabPanel("Interactive plot",
                                            plotlyOutput("ggplotly_plugin_display")
                                            )
                                   ),
                            box(title = "Customise plot",
                                width = 3,
                                height = "450px",
                                status = "primary",
                                solidHeader = TRUE,
                                textInput("y_var_label", "Y axis label"),
                                textInput("x_var_label", "X axis label"),
                                selectizeInput("col_palette", "Color palette", input_color_choice, multiple = TRUE),
                                selectInput("legend_position", "Legend position", input_legend_pos, selected = "right")
                                ),
                            box(title = "Customise font size",
                                width = 3,
                                height = "450px",
                                status = "primary",
                                solidHeader = TRUE,
                                textInput("font_size_axis_text", "axis text", value = "16"),
                                textInput("font_size_axis_title", "axis title", value = "16"),
                                textInput("font_size_legend", "legend", value = "16"),
                            ),
                            box(title = "Rename variables",
                                width = 3,
                                height = "450px",
                                status = "primary",
                                solidHeader = TRUE,
                                selectizeInput("rename_variable", "Select variable to rename", input_group_variable, multiple = TRUE, options = list(maxItems = 1)),
                                selectizeInput("rename_from", "Select value to rename", choices = NULL, multiple = TRUE, options = list(maxItems = 1)),
                                textInput("rename_to", "Rename to"),
                            ),
                            box(title = "Export plot",
                                width = 3,
                                height = "450px",
                                status = "primary",
                                solidHeader = TRUE,
                                textInput("export_plot_width", "Width (cm)", value = "42"),
                                textInput("export_plot_height", "Height (cm)", value = "26"),
                                selectInput("export_plot_format", "Format", c("pdf","png"), selected = "pdf"),
                                downloadButton("ggplot_plugin_download", "Download"))
                        )
                        ))
                    )
            ),
            tabItem(tabName = "aquacrop_legend",
                    h2(fluidRow(
                      box(width = 12 ,
                          div(dataTableOutput("legend_display"), style = "font-size: 75%; width: 100%")
                          )
                    ))
            )
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
    ##image logo display in home tab, Photo by @glenncarstenspeters on Unsplash
    output$aquacrop_logo <- renderImage({
        list(
            src = file.path("www/homepage_photo.jpg"),
            contentType = "image/jpg",
            width = "100%",
            height = "900px"
        )
    }, deleteFile = FALSE)
    
    ##legend for varaible names
    legend <- reactive({
      read.csv("legend.csv")
    })
    output$legend_display <- renderDataTable(legend())
    
    ###select button to enter aquacrop standard or plugin
    observeEvent(input$select_aquacrop_standard, {
        updateTabItems(session, "menu_tabs", "tab_upload_data_standard")
    })
    observeEvent(input$select_aquacrop_plugin, {
        updateTabItems(session, "menu_tabs", "tab_upload_data_plugin")
    })
    
    ##########standard
    ###upload data
    upload_data_standard_combined <- reactive({
            #require uploaded data files before evaluating
            req(input$upload_data_files_standard)
            #check to make sure uploaded files has the correct extension .OUT, return error if not 
            upload_data_files_standard_ext <- tools::file_ext(input$upload_data_files_standard$name)
            if(any(upload_data_files_standard_ext != "OUT")){
                validate("Invalid data file: Please upload only .OUT files")
            }

            #list of file extensions of all output files (9 files)
            file.extension = c("Clim.OUT","CompEC.OUT","CompWC.OUT","Crop.OUT","Inet.OUT","Prof.OUT","Run.OUT","Salt.OUT","Wabal.OUT")
            
            #read data
            input$upload_data_files_standard %>%            
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
                                                                        select(name) %>%
                                                                        distinct(),
                                                                    options = list(scrollX = TRUE))
   
    ##combined all daily data  
    upload_data_standard_combined_daily <- reactive({reduce(upload_data_standard_combined()$dataset[upload_data_standard_combined()$extension != "Run.OUT"], #filter out Run.OUT file as data format (seasonal)  different from others (daily)
                                                                    left_join, by = c("Day", "Month", "Year", "DAP", "Stage")) %>%
                                                      mutate(Date = dmy(paste(Day, Month, Year, sep="-")))
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
    upload_data_standard_combined_seasonal <- reactive({as.data.frame(upload_data_standard_combined()$dataset[upload_data_standard_combined()$extension == "Run.OUT"])
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

    ###ggplot standard daily data
    cbPalette <- c("#808080", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    ggplot_standard <- reactive({
      ggplot(data = upload_data_standard_combined_daily(), aes(x = .data[[input$x_var_standard]], y = .data[[input$y_var_standard]]))+
        geom_point()+
        theme_light()+
        scale_color_manual(values=cbPalette)
    })
    #render ggplot display in app
    output$ggplot_standard_display <- renderPlot({
      ggplot_standard()
    })
    #for downloading ggplot
    output$ggplot_standard_download <- downloadHandler(
      filename = function() {"plot_standard.pdf"},
      content = function(file) {
        ggsave(file, plot = ggplot_standard(), device = "pdf")
      }
    )
    
    ###ggplotly
    output$ggplotly_standard_display <- renderPlotly({
      ggplotly(ggplot_standard())
    })
    
    ##########plugin
    ###read upload data files and combine all data into dataframe
    upload_data_combined <-
        reactive({
            #require uploaded data files before evaluating
            req(input$upload_data_files)
            #check to make sure uploaded files has the correct extension .OUT, return error if not 
            upload_data_files_ext <- tools::file_ext(input$upload_data_files$name)
            if(any(upload_data_files_ext != "OUT")){
                validate("Invalid data file: Please upload only .OUT files")
            }
            #get a list of file paths from uploaded files
            input$upload_data_files %>%
                #import dataset and clean up, format into dataframe
                mutate(dataset = map(datapath, function(datapath){
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
                select(-size, -type, -datapath) %>%
                mutate(sowing_dmy = dmy(paste(Day1, Month1, Year1))) %>%
                mutate(sowing_date = paste(day(sowing_dmy), month(sowing_dmy, label = T), sep = "_"))
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
            #check to make sure uploaded files has the correct extension .prm, return error if not 
            upload_prm_files_ext <- tools::file_ext(input$upload_prm_files$name)
            if(any(upload_prm_files_ext != "PRM")){
                validate("Invalid parameter file: Please upload only .PRM files")
            }
            #get a list of prm file path from uploaded
            input$upload_prm_files %>%
                #read in each prm file from the list and extract parameter of interest
                mutate(parameter = map(datapath, function(datapath){
                    #read in each prm file from the list
                    prm.file = read_file(paste0(datapath))
                    #extract parameter from each param file 
                    #in this case we use str_extract to get parameters of the first time point (should be the same for all time points)
                    #get climate file
                    climate = str_extract(prm.file, "(?<=\\s).+?(?=\\.CLI\\r\\n)") %>%
                        unlist() %>%
                        str_replace_all("\\s","")
                    #from climate file, get location, rcp
                    location = str_extract(climate, regex(".+(?=\\(rcp)", ignore_case = TRUE))
                    rcp = str_extract(climate, regex("rcp[0-9]{2}", ignore_case = TRUE))
                    #get crop
                    crop = str_extract(prm.file, "(?<=\\s).+?(?=\\.CRO\\r\\n)") %>%
                        unlist() %>%
                        str_replace_all("\\s","")
                    #get irrigation
                    irrigation = str_extract(prm.file, "(?<=\\s).+?(?=\\.IRR\\r\\n)") %>%
                        unlist() %>%
                        str_replace_all("\\s","")
                    #get soil
                    soil = str_extract(prm.file, "(?<=\\s).+?(?=\\.SOL\\r\\n)") %>%
                        unlist() %>%
                        str_replace_all("\\s","")
                    #put parameters together in a table
                    param.table = data.frame(climate, location, rcp, crop, irrigation, soil)
                })) %>%
                unnest(parameter) %>%
                select(-size, -type, -datapath) %>%
              mutate(irrigation = ifelse(is.na(irrigation), "rainfed", irrigation))
        })
    #output datatable of the combined parameters
    output$upload_prm_combined_display <- renderDataTable(upload_prm_combined() %>%
                                                          select(name) %>%
                                                          distinct(),
                                                          options = list(scrollX = TRUE))
    
    ###check if all parameter .PRM files are uploaded as needed for all .OUT datasets
    #find a list of any missing prm file required in the data files
    missing_prm_file <- reactive({
        req(input$upload_data_files)
        req(input$upload_prm_files)
        
        upload_data_combined() %>%
            select(prm.file) %>%
            distinct() %>%
            anti_join(upload_prm_combined(), by = c("prm.file" = "name")) %>%
            rename(missing.prm.file = prm.file)
    })
    #return error if there is any missing prm files
    output$missing_prm_file_error <- reactive({
        req(input$upload_data_files)
        req(input$upload_prm_files)
        
        if(nrow(missing_prm_file()) > 0){
            validate("Missing .PRM files: Please upload the following .PRM files")
            }
    })
    #display missing prm files
    output$missing_prm_file_display <- renderTable(
        if(nrow(missing_prm_file()) > 0){
            missing_prm_file()
        }
    )

    ####add parameters to the output dataset
    data_prm_combined <- reactive({
            upload_data_combined() %>%
                left_join(upload_prm_combined(), by = c("prm.file" = "name"))
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
    
    ###option for renaming variable
    #create observe event module to monitor if user input select variable to rename
    #if variable selected, update the select input list for value choices of the selected variable
    renaming_variable <- reactive({
      input$rename_variable
    })
    observeEvent(renaming_variable(), {
      choices <- unique(data_prm_combined()[[input$rename_variable]])
      updateSelectInput(inputId = "rename_from", choices = choices) 
    })
    #change value of selected variable to the value from user
    data_prm_combined_rename <- reactive({
    rename_df <- data_prm_combined()
    rename_df[input$rename_variable][rename_df[input$rename_variable] == input$rename_from] <- input$rename_to
    rename_df
    })
    #render output table
    output$data_prm_combined_rename_display <- renderDataTable(datatable(data_prm_combined_rename(), 
                                                                       options = list(scrollX = TRUE)))
    
    ###ggplot
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
    
    #data for plotting
      ## f plotting mean is selected, calculate mean based on grouping variable selected
      data_prm_combined_plot <- reactive({
        if(input$use_mean == "Yes" & length(input$group_var) > 0){
          data_prm_combined() %>%
            group_by(across(all_of(c(input$group_var, "Year1", input$col_var)))) %>%
            summarise(across(c("Rain","ETo","GD","CO2","Irri","Infilt","Runoff","Drain","Upflow","E","E/Ex","Tr","TrW","Tr/Trx","SaltIn","SaltOut","SaltUp","SaltProf","Cycle","SaltStr","FertStr","WeedStr","TempStr","ExpStr","StoStr","BioMass","Brelative","HI","Yield","WPet"),
                             mean))
        }else{
          data_prm_combined() 
        }

      })
      
      #output datatable of the combined data and parameters with mean calculated
      output$data_prm_combined_plot_display <- renderDataTable(datatable(data_prm_combined_plot(), 
                                                                    options = list(scrollX = TRUE)))
      #for download
      output$download_combined_plot_dataset <- downloadHandler(
        filename = "Aquacrop_combined_mean_data.tsv",
        content = function(file) {
          write_tsv(data_prm_combined_plot(), file)
        }
      )

    
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
    
    ggplot_plugin <- reactive({
      #initial plot according to selected coloring and group variable
      if(length(input$col_var) > 0){
        p <- ggplot(data = data_prm_combined_plot(), aes(x = Year1, y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]]))+
          theme_light()+
          theme(axis.title = element_text(size = as.numeric(input$font_size_axis_title)), 
                axis.text = element_text(size = as.numeric(input$font_size_axis_text)),
                legend.title = element_text(size = as.numeric(input$font_size_legend)),
                legend.text = element_text(size = as.numeric(input$font_size_legend)))+
          scale_color_manual(values=custom_palette()) +
          theme(legend.position = paste(input$legend_position))
      }else{
        p <- ggplot(data = data_prm_combined_plot(), aes(x = Year1, y = .data[[input$y_var]]))+
          theme_light()+
          theme(axis.title = element_text(size = as.numeric(input$font_size_axis_title)), 
                axis.text = element_text(size = as.numeric(input$font_size_axis_text)),
                legend.title = element_text(size = as.numeric(input$font_size_legend)),
                legend.text = element_text(size = as.numeric(input$font_size_legend)))+
          theme(legend.position = paste(input$legend_position))
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
        p <- p + geom_point()
      }     
      if("line" %in% input$plot_element){
        p <- p + geom_line()
      }
      if("linear_trend" %in% input$plot_element){
        p <- p + geom_smooth(method="lm", se = F)
      }
      if("linear_trend_error" %in% input$plot_element){
        p <- p + geom_smooth(method="lm", se = T)
      }
      if(!("grid_line" %in% input$plot_element)){
        p <- p + theme(panel.grid = element_blank())
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
      print(p)
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
    
    ###ggplotly
    output$ggplotly_plugin_display <- renderPlotly({
        ggplotly(ggplot_plugin())
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)

