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
library(dplyr)
library(dbplyr)
library(DBI)

db_table_focus = "camera_trap_pcap"
db_table_other = "other_project_cameras"
db_table_out = "rshiny_test_form_3"
# un-comment this and sections for saving to file locally for /testing/debugging
responsesDir <- file.path("responses")

# formData reactive in server block must be changed whenever form is modified

source("loginparams_shiny.R")

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
    rm(User, Password, pos = ".GlobalEnv")
})

epochTime <- function() {
    as.integer(Sys.time())
}

# Set up marking for mandatory fields
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
ui <- navbarPage("Cleveland Metroparks Wildlife Cameras", 
                 id = "nav",
                 tabPanel("Camera check data",
                          id = "form",
                          "This is where you can enter data on the camera you have checked.",
                          br(),
                          
                          "Mandatory information marked with red star",
                          labelMandatory(" "),
                          " must be entered before you can submit.", br(),
                          
                          textInput("names", labelMandatory("Names"),
                                   placeholder = "Names of camera card volunteers"),
                          textInput("email", "Email",
                                    placeholder = "at least one of team for follow up"),
                          dateInput("date", labelMandatory("Date")),
                          selectInput("clock_updated", "Clock moved back/forward 1 hour?",
                                      choices = c("Choose one option" = "",
                                                  "Yes", "No")),
                          selectInput("project", labelMandatory("Camera project"),
                                      choices = c("Choose one option" = "",
                                                  "Focus on Wildlife wildlife camera project" = 
                                                      "focus", 
                                                  "Other wildlife camera projects \n(e.g., West Creek trails, S. Chagrin climate, Timberlane plots)" = 
                                                      "other")),
                          conditionalPanel(condition = "input.project == 'focus'",
                                           uiOutput("ui_focus_camera_name")),
                          conditionalPanel(condition = "input.project == 'other'",
                                           uiOutput("ui_other_camera_name")),
                          textAreaInput("other_camera",
                                        "If your camera was not in the list, enter it here with any notes."),
                          textAreaInput("action_items",
                                        "Action items needed (if any)"),
                          numericInput("image_count", labelMandatory("Number of pictures on SD Card")),
                          "Did you observe any NEW significant human activity at or near this camera that may be related to extra park use during the COVID-19 period? (e.g., much higher human activity, vandalism, decorations or constructions, high amounts of trash)",
                          selectInput("covid_related_impact",labelMandatory("COVID-related impact?"),
                                      choices = c("Choose one option" = "",
                                                  "Yes",
                                                  "No")),
                          conditionalPanel(condition = "input.covid_related_impact == 'Yes'",
                                           textAreaInput("covid_human_impacts","COVID-related human impacts?")),
                          textAreaInput("comments", "Comments"),
                          numericInput("battery_status", labelMandatory("Battery status  (from camera display)"),
                                      min = 0, max = 3),
                          selectInput("batteries_changed","Batteries changed?",
                                      choices = c("Choose one option" = "",
                                                  "Yes",
                                                  "No")),
                          dateInput("battery_change_date", "Batteries changed date"),
# May want to include battery readings here

                        "Mandatory information marked with red star",
                        labelMandatory(" "),
                        " must be entered before you can submit.", br(),

                 ),
                 tabPanel("Camera card upload",
                          id = "upload",
                          "Here you can upload a compressed file containing images from one camera card."
                 ),
                actionButton("submit", "Submit", class = "btn-primary"), br(),
                shinyjs::hidden(
                    span(id = "submit_msg", "Submitting..."),
                    div(id = "error",
                        div(br(), 
                            tags$b("Error: "), 
                            span(id = "error_msg"), 
                            br()))
                ),
                shinyjs::hidden(
                    div(
                        id = "thankyou_msg",
                        h3("Thanks, your response was submitted successfully! You may be contacted for further information."),
                        actionLink("submit_another", "Submit another response")
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Need reactive for
    #  focus_camera_choices
    #  other_camera_choices
    focus_table_id = Id(schema = Schema, 
                  table = db_table_focus)
    other_table_id = Id(schema = Schema, 
                        table = db_table_other)
    
    sqlOutputFocusCameras = reactive({
        
        sqlInputFocusCameras = paste("select distinct camera_id_llnnnn from focus_table_id", sep="")
        
        dbGetQuery(con, sqlInputFocusCameras)
    })
    
    sqlOutputOtherCameras <- reactive({
        
        sqlInputOtherCameras<- paste("select distinct camera_id from other_table_id", sep="")
        
        dbGetQuery(con, sqlInputOtherCameras)
    })
    
    
    output$ui_focus_camera_choices <- renderUI({
        selectInput('focus_camera_choices',
                    label ='Focus on Wildlife camera name',
                    choices=sqlOutputFocusCameras(),
                    selected = NULL, multiple = FALSE, width="450px")
    })
    
    output$ui_other_camera_choices <- renderUI({
        selectInput('other_camera_choices',
                    label ='Other project camera names',
                    choices=sqlOutputOtherCameras(),
                    selected = NULL, multiple = FALSE, width="450px")
    })

    humanTime <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
    
    saveData <- function(data) {
        # Keeping this for debugging. Also change format of recordID above
        write.csv(x = data, file = file.path(responsesDir, recordID()),
                  row.names = FALSE, quote = TRUE)
        # dbAppendTable(con, table_id, value = data.frame(data))
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
}

# Run the application 
shinyApp(ui = ui, server = server)
