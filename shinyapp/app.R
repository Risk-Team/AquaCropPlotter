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
    dashboardHeader(title = "Aquacrop"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "tab_home"),
            menuItem("upload_data", tabName = "tab_upload_data"),
            menuItem("combined_data", tabName = "tab_combined_data"),
            menuItem("plot", tabName = "tab_plot")
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "tab_home",
                    h2(
                        fluidRow(
                            imageOutput("aquacrop_logo")
                        ),
                        fluidRow(
                            box(title = "Aquacrop standard (single model)", status = "primary", solidHeader = TRUE,
                                actionButton("select_aquacrop_standard", "Aquacrop standard (single model)")
                            ),
                            box(title = "Aquacrop plug-in (multiple models)", status = "primary", solidHeader = TRUE,
                                actionButton("select_aquacrop_plugin", "Aquacrop plug-in (multiple models)")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_upload_data",
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
                                fileInput("upload_data_files", "Upload data files (.OUT)", multiple = TRUE, accept = ".OUT"),
                                dataTableOutput("upload_data_combined_display")
                            ),
                            box(title = "Parameter files", status = "primary", solidHeader = TRUE,
                                #upload parameter file
                                fileInput("upload_prm_files", "Upload parameter files (.PRM)", multiple = TRUE, accept = ".PRM"),
                                dataTableOutput("upload_prm_combined_display")
                            )
                        )
                    )
            ),
            tabItem(tabName = "tab_combined_data",
                    h2(
                        #button for downloading all combined data
                        downloadButton("download_combined_dataset", "Download combined dataset"),
                        #display combined data table
                        dataTableOutput("data_prm_combined_display")
                    )
            ),
            tabItem(tabName = "tab_plot",
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
    
    ###read upload data files andcombine all data into dataframe
    upload_data_combined <-
        reactive({
            #require uploaded data files before evaluating
            req(input$upload_data_files)
            #check to make sure uploaded files has the correct extension .OUT, return error if not 
            upload_data_files_ext <- tools::file_ext(input$upload_data_files$name)
            if(any(upload_data_files_ext != "OUT")){
                validate("Invalid file: Please upload only .OUT files")
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
                unnest(dataset) 
        })
    #output datatable of the combined data
    output$upload_data_combined_display <- renderDataTable(upload_data_combined() 
                                                           %>% select(name)  
                                                           %>% distinct())
    
    ###read uploaded parameter files and combine
    upload_prm_combined <-
        reactive({
            #require uploaded prm files before evaluating
            req(input$upload_prm_files)
            #check to make sure uploaded files has the correct extension .prm, return error if not 
            upload_prm_files_ext <- tools::file_ext(input$upload_prm_files$name)
            if(any(upload_prm_files_ext != "PRM")){
                validate("Invalid file: Please upload only .PRM files")
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
                unnest(parameter)
        })
    #output datatable of the combined parameters
    output$upload_prm_combined_display <- renderDataTable(upload_prm_combined() 
                                                          %>% select(name))
    
    ###check if all parameter .PRM files are uploaded as needed for all .OUT datasets
    #find a list of any missing prm file required in the data files
    missing_prm_file <- reactive({
        upload_data_combined() %>%
            select(prm.file) %>%
            distinct() %>%
            anti_join(upload_prm_combined(), by = c("prm.file" = "name")) %>%
            rename(missing.prm.file = prm.file)
    })
    #return error if there is any missing prm files
    output$missing_prm_file_error <- reactive({
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

