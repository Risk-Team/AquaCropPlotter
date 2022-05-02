library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

#sets of input variables to select for plotting
input_plot_variable = c("Day1","Month1","Year1","Rain","ETo","GD","CO2","Irri","Infilt","Runoff","Drain","Upflow","E","E/Ex","Tr","TrW","Tr/Trx","SaltIn","SaltOut","SaltUp","SaltProf","Cycle","SaltStr","FertStr","WeedStr","TempStr","ExpStr","StoStr","BioMass","Brelative","HI","Yield","WPet","DayN","MonthN","YearN")
input_group_variable = c("climate.model","location","rcp","irrigation","crop","soil")

#define UI dashboard
ui <- dashboardPage(
    dashboardHeader(title = "ShinyAquacrop"),
    
    dashboardSidebar(
        sidebarMenu(id = "menu_tabs",
            menuItem("Home", tabName = "tab_home", icon = icon("home")),
            menuItem("Standard", tabName = "aquacrop_standard", icon = icon("list-alt"), startExpanded = TRUE,
                     menuSubItem("upload_data", tabName = "tab_upload_data_standard", icon = icon("caret-right")),
                     menuSubItem("combined_data", tabName = "tab_combined_data_standard", icon = icon("caret-right")),
                     menuSubItem("plot", tabName = "tab_plot_standard", icon = icon("caret-right"))),
            menuItem("Plug-in", tabName = "aquacrop_plugin", icon = icon("puzzle-piece"), startExpanded = TRUE,
                     menuSubItem("upload_data", tabName = "tab_upload_data_plugin", icon = icon("caret-right")),
                     menuSubItem("combined_data", tabName = "tab_combined_data_plugin", icon = icon("caret-right")),
                     menuSubItem("plot", tabName = "tab_plot_plugin", icon = icon("caret-right"))
                     )
        )
    ),
    
    dashboardBody(
        #customise fonts and colors in the header and sidebar 
        tags$head(tags$style(HTML(".main-header .logo {font-weight: bold; font-size: 24px;}
                                    .main-sidebar {font-weight: bold; font-size: 20px;}
                                    .treeview-menu>li>a {font-weight: bold; font-size: 20px!important;}
                                    
                                    .skin-blue .main-header .logo {background-color: #5792c9;}
                                    .skin-blue .main-header .navbar {background-color: #5792c9;}
                                    .skin-blue .main-sidebar {background-color: #9AB7D2; color: #9AB7D2;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu a{background-color: #9AB7D2; color: #414042;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #5792c9; color: #000000;}
                                    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #5792c9; color: #ffffff;}
                                    .box.box-solid.box-primary>.box-header {background-color: #416D96;}
                                  "))),
        tabItems(
            tabItem(tabName = "tab_home",
                    h2(
                        fluidRow(
                            imageOutput("aquacrop_logo")
                        ),
                        fluidRow(
                            box(title = "Aquacrop standard (single season)", status = "primary", solidHeader = TRUE,
                                actionButton("select_aquacrop_standard", "Select Aquacrop standard")
                            ),
                            box(title = "Aquacrop plug-in (multiple seasons)", status = "primary", solidHeader = TRUE,
                                actionButton("select_aquacrop_plugin", "Select Aquacrop plugin")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_upload_data_standard",
                    h2(
                        #display boxes for data and prm files upload
                        fluidRow(
                            box(title = "Data files", status = "primary",
                                #upload data file
                                fileInput("upload_data_files_standard", "Upload data files (.OUT)", multiple = TRUE, accept = ".OUT"),
                                dataTableOutput("upload_data_standard_combined_display")
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
                        dataTableOutput("upload_data_standard_combined_daily_display"),
                        ##seasonal data
                        #button for downloading data
                        downloadButton("download_data_standard_combined_seasonal", "Download combined seasonal dataset"),
                        #display combined seasonal data table
                        dataTableOutput("upload_data_standard_combined_seasonal_display")
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
                            box(title = "Data files", status = "primary",
                                #upload data file
                                fileInput("upload_data_files", "Upload data files (.OUT)", multiple = TRUE, accept = ".OUT"),
                                dataTableOutput("upload_data_combined_display")
                            ),
                            box(title = "Parameter files", status = "primary",
                                #upload parameter file
                                fileInput("upload_prm_files", "Upload parameter files (.PRM)", multiple = TRUE, accept = ".PRM"),
                                dataTableOutput("upload_prm_combined_display")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_combined_data_plugin",
                    h2(
                        #button for downloading all combined data
                        downloadButton("download_combined_dataset", "Download combined dataset"),
                        #display combined data table
                        dataTableOutput("data_prm_combined_display")
                    )
            ),
            tabItem(tabName = "tab_plot_plugin",
                    h2(
                        fluidRow(
                            box(title = "Select plotting variables",
                                width = 4,
                                selectInput("y_var", "Select variable to plot on y axis", input_plot_variable, selected = "Yield"),
                                selectInput("x_var", "Select variable to plot on x axis", input_plot_variable, selected = "Year1")),
                            box(title = "Select grouping variables",
                                width = 4,
                                selectInput("col_var", "Select variable to group in color", input_group_variable, selected = "rcp")),
                            box(title = "Select facetting variables",
                                width = 4,
                                selectizeInput("facet_var", "Select variable to group in facet", input_group_variable, selected = c("crop","location"),
                                               multiple = TRUE, options = list(maxItems = 2)))
                        ),
                        fluidRow(
                            plotOutput("ggplot_display"),
                            plotlyOutput("ggplotly_display") 
                        ),
                        fluidRow(
                            selectizeInput("group_var", "Select variable to group for calculating mean", input_group_variable, selected = c("crop","location"),
                                           multiple = TRUE)
                        ),
                        fluidRow(
                            dataTableOutput("data_prm_combined_mean_display"),
                            plotOutput("ggplot_mean_display")
                        )
                    )
            )
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
    ##image logo display in home tab
    output$aquacrop_logo <- renderImage({
        list(
            src = file.path("www/aquacrop_logo.png"),
            contentType = "image/png",
            width = 300,
            height = 300
        )
    }, deleteFile = FALSE)
    
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
                                                                    left_join, by = c("Day", "Month", "Year", "DAP", "Stage"))
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
                    #get climate model
                    climate.model = str_extract(prm.file, "(?<=\\s).+?(?=\\.CLI\\r\\n)") %>%
                        unlist() %>%
                        str_replace_all("\\s","")
                    #from climate model, get location, rcp
                    location = str_extract(climate.model, regex(".+(?=\\(rcp)", ignore_case = TRUE))
                    rcp = str_extract(climate.model, regex("rcp[0-9]{2}", ignore_case = TRUE))
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
                    param.table = data.frame(climate.model, location, rcp, crop, irrigation, soil)
                })) %>%
                unnest(parameter) %>%
                select(-size, -type, -datapath)
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
    

    ###ggplot
    output$ggplot_display <- renderPlot({
        ggplot(data = data_prm_combined(), aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]]))+
            geom_point()+
            geom_smooth(method="lm")+
            facet_grid(get(input$facet_var[2])~get(input$facet_var[1]))
    })
    
    ###ggplotly
    output$ggplotly_display <- renderPlotly({
        pp <- ggplot(data = data_prm_combined(), aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]]))+
            geom_point()+
            geom_smooth(method="lm")+
            facet_grid(get(input$facet_var[2])~get(input$facet_var[1]))
        ggplotly(pp)
    })
    
    ### calculate mean based on grouping variable
    data_prm_combined_mean <- reactive({
        data_prm_combined() %>%
            group_by(across(all_of(c(input$group_var, input$x_var, input$col_var)))) %>%
            summarise(across(c("Rain","ETo","GD","CO2","Irri","Infilt","Runoff","Drain","Upflow","E","E/Ex","Tr","TrW","Tr/Trx","SaltIn","SaltOut","SaltUp","SaltProf","Cycle","SaltStr","FertStr","WeedStr","TempStr","ExpStr","StoStr","BioMass","Brelative","HI","Yield","WPet"),
                             mean))
    })
    #output display data mean aggregated
    output$data_prm_combined_mean_display <- renderDataTable(datatable(data_prm_combined_mean(), 
                                                                  options = list(scrollX = TRUE)))
    #ggplot
    output$ggplot_mean_display <- renderPlot({
        ggplot(data = data_prm_combined_mean(), aes(x = .data[[input$x_var]], y = .data[[input$y_var]], group = .data[[input$col_var]], col = .data[[input$col_var]]))+
            geom_point()+
            geom_line()+
            facet_grid(get(input$facet_var[2])~get(input$facet_var[1]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

