library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)

#define UI dashboard
ui <- dashboardPage(
    dashboardHeader(title = "Aquacrop"),
    
    dashboardSidebar(
        #upload data file
        fileInput("upload_data_files", "Upload data files (.OUT)", multiple = TRUE, accept = ".OUT"),
        
        #upload parameter file
        fileInput("upload_prm_files", "Upload parameter files (.PRM)", multiple = TRUE, accept = ".PRM"),
        
        #download all combined data
        downloadButton("download_combined_dataset", "Download combined dataset")
    ),
    
    dashboardBody(
        dataTableOutput("upload_data_combined_display"),
        dataTableOutput("upload_prm_combined_display"),
        dataTableOutput("data_prm_combined_display"),
        dataTableOutput("missing_prm_file_error"),
        dataTableOutput("missing_prm_file_display")
        
    )
)

# Define server logic 
server <- function(input, output, session) {
    
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
    output$upload_data_combined_display <- renderDataTable(upload_data_combined())
    
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
                    #in this case we use str_extract to get parameters of the first time point
                    #if we use str_extract_all to get parameters of all simulation time points (should be all the same, but year of sowing date)
                    #get sowing date
                    sowing.date = str_extract(prm.file,"(?<=First day of simulation period - ).+(?=\\r\\n)") %>%
                        unlist() %>%
                        dmy()
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
                    param.table = data.frame(sowing.date, climate.model, location, rcp, crop, irrigation, soil)
                })) %>%
                unnest(parameter)
        })
    #output datatable of the combined parameters
    output$upload_prm_combined_display <- renderDataTable(upload_prm_combined())
    
    ###check if all parameter .PRM files are uploaded as needed for all .OUT datasets
    #find a list of any missing prm file required in the data files
    missing_prm_file <- reactive({
        upload_data_combined() %>%
            select(prm.file) %>%
            distinct() %>%
            anti_join(upload_prm_combined(), by = c("prm.file" = "name"))
    })
    #return error if there is any missing prm files
    output$missing_prm_file_error <- reactive({
        if(nrow(missing_prm_file()) > 0){
            validate("Missing .PRM files: Please upload the following .PRM files")
        }
    })
    #display missing prm files
    output$missing_prm_file_display <- renderDataTable(
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
    output$data_prm_combined_display <- renderDataTable(data_prm_combined())
    #for downloading combined dataset
    output$download_combined_dataset <- downloadHandler(
        filename = "Aquacrop_combined_data.tsv",
        content = function(file) {
            write_tsv(data_prm_combined(), file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

