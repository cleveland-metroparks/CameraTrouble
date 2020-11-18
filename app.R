#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Based partly on 
#  - https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# and
#  - https://github.com/cleveland-metroparks/nr_coyote_form
#
# To get other camera names I exported  to .csv, imported into r, and used
# other_names = unique(Wildlife.camera.check.data.11.01.2020$Other.project.camera.names)
# other_camera_names = data.frame(camera_id = other_names[order(other_names)])[-c(1, 5),]
# Then write.csv(other_camera_names, "other_camera_names.csv", row.names = F)
# Then import into table in db where camera_id was set as primary key


library(shiny)
library(shinyTime)
library(shinyjs)
library(shinymanager)
library(dplyr)
library(dbplyr)
library(DBI)

db_table_focus = "camera_trap_pcap"
db_table_other = "other_project_cameras"
db_table_out = "rshiny_test_form_3"
db_uploads_table_out = "rshiny_test_form_4"
# un-comment this and sections for saving to file locally for /testing/debugging
# responsesDir <- file.path("responses")
file_uploadsDir = file.path("file_uploads")

# For larger uploads of 7zip files, we will need to do something like:
max_mb = 77000 # This would give 7.7Gb limit
# options(shiny.maxRequestSize = max_mb*1024^2/1000)


# These need to be changed whenever fields are added/subtracted from ui
fieldsSimple = c("names", "email", "clock_updated", "project", "camera_choices",
                 "other_camera_note", "action_items", "image_count", 
                 "covid_human_impacts", "comments", "battery_status", 
                 "batteries_changed")

source("loginparams_shiny.R")
source("cred_pass.R")

con = dbConnect(
    drv = RPostgres::Postgres(),
    # drv = RPostgreSQL::PostgreSQL(),
    dbname = DBname,
    host = Host,
    user = User,
    password = Password,
    port=Port
)
onStop(function() {
    dbDisconnect(con)
    rm(User, Password, passphrase, pos = ".GlobalEnv")
})

# Set up marking for mandatory fields
# Camera name may be other and require note which has to be handled separately
#  in the mandatoryFilled function in server
fieldsMandatory = c("names", 
                    "date", 
                    "project", 
                    "image_count", 
                    "covid_related_impact",
                    "battery_status")

labelMandatory = function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )
}
appCSS <-
    ".mandatory_star { color: red; }
     #error { color: red; }"

# Define UI for application that draws a histogram
ui <- secure_app(navbarPage("Cleveland Metroparks Wildlife Cameras",
                 id = "nav",
                 tabPanel("Camera check data",
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
                          div(id = "form",
                          h4("This is where you can enter data about the camera you have checked."),
                          "Mandatory information marked with red star",
                          labelMandatory(" "),
                          " must be entered before you can submit.", br(), br(),
                          
                          textInput("names", labelMandatory("Names"),
                                   placeholder = "Names of camera card volunteers"),
                          textInput("email", "Email",
                                    placeholder = "At least one email of team for follow up"),
                          dateInput("date", labelMandatory("Date")),
                          selectInput("clock_updated", "Clock moved back/forward 1 hour?",
                                      choices = c("Choose one option" = "",
                                                  "No", "Yes")),
                          selectInput("project", labelMandatory("Camera project"),
                                      choices = c("Choose one option" = "",
                                                  "Focus on Wildlife wildlife camera project" = 
                                                      "focus", 
                                                  "Other wildlife camera projects \n(e.g., West Creek trails, S. Chagrin climate, Timberlane plots)" = 
                                                      "other")),
                          conditionalPanel(condition = 'input.project != ""',
                                           uiOutput("ui_camera_choices")),
                          conditionalPanel(condition = 'input.project != ""',
                                           tags$span(style="color:red", 
                                                     textOutput("camera_entered")), br()),
                          conditionalPanel(condition = 'input.project != "" &&
                                           input.camera_choices == "other"',
                                           textInput("other_camera_note",
                                                     labelMandatory("If your camera was not 
                                                         in the list, enter it here (add any 
                                                        notes in action_items)."))),
                          textAreaInput("action_items",
                                        "Action items needed (if any)"),
                          numericInput("image_count", labelMandatory("Number of pictures on SD Card (click box and type number)"),
                                       value = NULL,
                                       min = 0),
                          labelMandatory(tags$strong("COVID-related impact?: ")),"Did you 
                          observe any NEW significant human activity at or near this 
                          camera that may be related to extra park use during the COVID-19 
                          period? (e.g., much higher human activity, vandalism, decorations 
                          or constructions, high amounts of trash)", br(),
                          selectInput("covid_related_impact", "",
                                      choices = c("Choose one option" = "",
                                                  "No",
                                                  "Yes")),
                          conditionalPanel(condition = "input.covid_related_impact == 'Yes'",
                                           textAreaInput("covid_human_impacts","COVID-related human impacts?")),
                          textAreaInput("comments", "Comments"),
                          numericInput("battery_status", labelMandatory("Battery status  (from camera display; 0-3)"),
                                      value = NULL, min = 0, max = 3),
                          selectInput("batteries_changed","Batteries changed?",
                                      choices = c("Choose one option" = "",
                                                  "No",
                                                  "Yes")),
                          conditionalPanel(condition = "input.batteries_changed == 'Yes'",
                                           uiOutput('up_date')),
# May want to include battery readings here

                        "Mandatory information marked with red star",
                        labelMandatory(" "),
                        " must be entered before you can submit.", br(), br(),
                        actionButton("submit", "Submit", class = "btn-primary"), br(), br(),
                        shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                                        div(id = "error",
                                            div(
                                                br(),
                                                tags$b("Error: "),
                                                span(id = "error_msg"),
                                                br(), br()
                                            )))),
                        shinyjs::hidden(div(
                            id = "thankyou_msg",
                            h3(
                                "Thanks, your response was submitted successfully! You may be contacted for further information."
                            ),
                            actionLink("submit_another", "Submit another response")
                        ))

                 ),
                 tabPanel("Camera card upload",
                          id = "uploader",
                          shinyjs::useShinyjs(),
                          shinyjs::inlineCSS(appCSS),
                          div(id = "form2",
                          h4("Here you can upload a compressed file containing images from one camera card."),
                              "Enter the following information to identify the upload and then an upload button will appear.",
                          uiOutput("ui_project2"),
                          conditionalPanel(condition = 'input.project2 != ""',
                                           uiOutput("ui_camera_choices2")),
                          conditionalPanel(condition = 'input.project2 != ""',
                                           tags$span(style="color:red",
                                                     textOutput("camera_entered2")), br()),
                          conditionalPanel(condition = 'input.project2 != "" &&
                                           input.camera_choices2 == ""',
                                           uiOutput("ui_other_camera_note2")),
                          "Mandatory information marked with red star",
                          labelMandatory(" "),
                          " must be entered before you can upload.", br(), br(),
                          uiOutput("ui_upload_file"),
                          shinyjs::hidden(span(id = "submit_msg2", "Submitting..."),
                                          div(id = "error2",
                                              div(
                                                  br(),
                                                  tags$b("Error: "),
                                                  span(id = "error_msg2"),
                                                  br(), br()
                                              )))),
                          shinyjs::hidden(div(
                              id = "thankyou_msg2",
                              h3(
                                  "Thanks, your upload was successful! You may be contacted for further information."
                              ),
                              actionLink("submit_another2", "Submit another upload")
                          )),
# Debug code to check on upload and copy
                          # h5("Once file upload is complete a verification with file, size (Kilobytes), and type will appear here (may take a while):"),
                          # tableOutput("files"),
                          # h5("Once your file is copied into storage on the server, a message will appear here:"),
                          # # textOutput("file_copied"), br(),
                          # tableOutput("file_uploaded_name"),
                 )
), enable_admin = TRUE)

server <- function(session, input, output) {
    # check_credentials directly on sqlite db
    res_auth <- secure_server(
        check_credentials = check_credentials(
            db = "database.sqlite",
            # passphrase = Sys.getenv("R_shinymanager_key")
            passphrase = passphrase
        )
    )
    
# Warning if no project was entered yet
    observe({
        if((is.null(input$camera_choices) || input$camera_choices == "") &&
           (is.null(input$other_camera_note) || input$other_camera_note == "")
           ) {
            output$camera_entered = renderText("No camera name entered. Choose one 
                                               from dropdown above or enter in 
                                               space provided below.")
        } else {
            output$camera_entered = renderText("")
        }
    })
    
    observe({
        if((is.null(input$camera_choices2) || input$camera_choices2 == "") &&
           (is.null(input$other_camera_note2) || input$other_camera_note2 == "")
        ) {
            output$camera_entered2 = renderText("No camera name entered. Choose one
                                               from dropdown above or enter in
                                               space provided below.")
        } else {
            output$camera_entered2 = renderText("")
        }
    })
    
    observe({
        mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                       class(input[[x]]) == "Date" ||
                           (!is.null(input[[x]]) &&  input[[x]] != "")
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)
        camera_id_filled = (!is.null(input$camera_choices) &&
                                input$camera_choices != "") ||
        (input$camera_choices == "other" &&
            (!is.null(input$other_camera_note) &&
                 input$other_camera_note != ""))
        
        shinyjs::toggleState(id = "submit", 
                             condition = mandatoryFilled && camera_id_filled)
    })

    output$ui_upload_file = renderUI({
        if((!is.null(input$camera_choices2) &&
            input$camera_choices2 != "") ||
           (!is.null(input$other_camera_note2) &&
            input$other_camera_note2 != "")){
            fileInput("file_upload",
                      "Upload the 7zip file with images in it",
                      accept = c(".jpg", ".png"))
        } else {
                "Upload button will appear here once project and camera are defined."
        }
    })

    sqlOutputCameras = reactive({
        if(input$project == "focus"){
            sqlInputCameras = paste("select distinct camera_id_llnnnn from ",
                                    Schema, ".", db_table_focus,
                                    " order by camera_id_llnnnn;", sep="")
        } else {
            sqlInputCameras<- paste("select distinct camera_id from ",
                                    Schema, ".", db_table_other,
                                    " order by camera_id;", sep="")
        }
        dbGetQuery(con, sqlInputCameras)
    })
    
    sqlOutputCameras2 = reactive({
        if(isTruthy(input$project2) && input$project2 == "focus"){
            sqlInputCameras = paste("select distinct camera_id_llnnnn from ",
                                    Schema, ".", db_table_focus,
                                    " order by camera_id_llnnnn;", sep="")
        } else {
            sqlInputCameras<- paste("select distinct camera_id from ",
                                    Schema, ".", db_table_other,
                                    " order by camera_id;", sep="")
        }
        dbGetQuery(con, sqlInputCameras)
    })
    
    output$ui_camera_choices <- renderUI({
        selectInput('camera_choices',
                    label =labelMandatory('Wildlife camera name'),
                    choices=append(
                        append(
                            sqlOutputCameras(),
                            c("Choose one camera" = ""), after = 0),
                        c("Other" = "other")),
                    selected = NULL, multiple = FALSE, width="450px")
    })
    
# Set up to use previous entry as default entry here
    output$ui_project2 = renderUI({
        selectInput("project2", labelMandatory("Camera project"),
                choices = c("Choose one option" = "",
                            "Focus on Wildlife wildlife camera project" =
                                "focus",
                            "Other wildlife camera projects \n(e.g., West Creek trails, S. Chagrin climate, Timberlane plots)" =
                                "other"),
                selected = input$project)
    })
    
    output$ui_camera_choices2 <- renderUI({
        selectInput('camera_choices2',
                    label =labelMandatory('Wildlife camera name'),
                    choices=append(
                        append(sqlOutputCameras2(),
                                   c("Choose one camera" = ""), after = 0),
                        c("Other" = "other")),
                    selected = input$camera_choices,
                    multiple = FALSE, width="450px")
    })

    output$ui_other_camera_note2 = renderUI({
        textInput("other_camera_note2",
                  labelMandatory("If your camera was not in the list, 
                  enter it here (add any notes in action_items)."),
                  value = input$other_camera_note)
    })
    
    output$up_date = renderUI({
        if(input$batteries_changed == 'Yes') {
            dateInput("battery_change_date",
                  "Batteries changed date",
                  value = input$date)
        }
    })
    
    entry_dt = Sys.time()
    humanTime <- function(x) format(x, "%Y%m%d-%H%M%OS")
    
    recordID = reactive({
        sprintf(
            # Comment out one of these depending on if you are saving a file locally or not
            "%s_%s_%s", # If not saving file locally
            # "%s_%s_%s.csv", # or "%s_%s_%s_%s.csv" if saving locally
            input$project,
            paste(input$camera_choices,
                   input$other_camera_note, sep = "_"),
            humanTime(entry_dt)
            # use line below if you worry about same camera name/same second 
            #  collisions or want a nice unique key. Also change format of sprintf above
            # digest::digest(formData())
        )
    })
 
    formData <- reactive({
        data <- sapply(fieldsSimple, function(x) input[[x]])
        data <- c(record_id = recordID(),
                  card_retrival_date = as.character(input$date),
                  entry_datetime = as.character(entry_dt),
                  data,
                  battery_change_date = as.character(input$battery_change_date))
        data <- t(data)
        data
    })

    table_id = Id(schema = Schema, 
                  table = db_table_out)
    
    saveData <- function(data) {
        # Keeping this for debugging. Also change format of recordID above
        # write.csv(x = data, file = file.path(responsesDir, recordID()),
        #           row.names = FALSE, quote = TRUE)
        dbAppendTable(con, table_id, value = data.frame(data))
    }

    # action to take when submit button is pressed
    observeEvent(input$submit, {
        shinyjs::disable("submit")
        shinyjs::show("submit_msg")
        shinyjs::hide("error")
        tryCatch({
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thankyou_msg")
        },
        error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error", 
                          anim = TRUE, 
                          animType = "fade")
        },
        finally = {
            shinyjs::enable("submit")
            shinyjs::hide("submit_msg")
        })
    })
    
    observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
    })
    
# File upload tab
    observeEvent(input$project2, {
        ufp = file.path(file_uploadsDir, input$project2)
        if(!dir.exists(ufp))
            dir_created = dir.create(ufp, showWarnings = F)
    })

    fileID = reactive({
        sprintf(
            "%s_%s_%s.jpg", # or "%s_%s_%s_%s.jpg" if saving locally
            input$project2,
            paste(
                input$camera_choices2,
                input$other_camera_note2, sep = "_"),
            humanTime(entry_dt)
            # use line below if you worry about same camera name/same second
            #  collisions or want a nice unique key. Also change format of sprintf above
            # digest::digest(input$file_upload) # not sure about this using input for a hash
        )
    })

    file_uploaded = reactive({
        req({input$file_upload})
    })

    file_copied = reactive({
        upload_value = file_uploaded()
        luv = length(upload_value)
        # cat("File ", upload_value[1,1], " copied as ", fileID(), "\n") # Debug code
        req(upload_value,
            file.copy(upload_value$datapath,
                      file.path(file_uploadsDir,
                                ifelse(dir.exists(
                                    file.path(file_uploadsDir, input$project2)),
                                    input$project2,
                                    ""),
                                fileID()))
            )
        if(luv > 1)
            upload_value$file_copied_to = fileID()
        upload_value[,-4] # Item 4 is tmp dir path
    })
    
# action to take when upload and copy are completed
    table_id2 = Id(schema = Schema, 
                  table = db_uploads_table_out)
    
    saveData2 <- function(data) {
        # Keeping this for debugging. Also change format of recordID above
        # write.csv(x = data, file = file.path(responsesDir, 
        #                                      paste0(strsplit(fileID(), "\\.")[[1]][1],
        #                                             ".csv")),
        #           row.names = FALSE, quote = TRUE)
        dbAppendTable(con, table_id2, value = data.frame(data))
    }

        # upload_success = FALSE # Debug code
    observeEvent(file_uploaded(), {
        shinyjs::disable("ui_upload_file")
        shinyjs::show("submit_msg2")
        shinyjs::hide("error2")
        tryCatch({
            fc = file_copied()
            saveData2(fc)
            shinyjs::reset("form2")
            shinyjs::hide("form2")
            shinyjs::show("thankyou_msg2")
        },
            error = function(err) {
                shinyjs::html("error_msg2", err$message)
                shinyjs::show(id = "error2", 
                              anim = TRUE, 
                              animType = "fade")
            },
            finally = {
                shinyjs::enable("ui_upload_file")
                shinyjs::hide("submit_msg2")
        })
    })
    
    observeEvent(input$submit_another2, {
        shinyjs::show("form2")
        shinyjs::hide("thankyou_msg2")
    })
    
# Debug code to check on upload and copy
    # output$files <- renderTable({
    #     fu = file_uploaded()
    #     fu$size = fu$size/1000
    #     fu[,-4]
    # })
    # 
    # output$file_uploaded_name = renderText({
    #     fc = file_copied()
    #     upload_success <<- TRUE
    #     paste("File was named", fc$file_copied_to, 
    #           "and was successfully copied to the image storage space.")
    # })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
