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
# These need to be changed whenever fields are added/subtracted from ui
fieldsSimple = c("names", "email", "clock_updated", "project", "focus_camera_choices",
                 "other_camera_choices", "other_camera_note", "action_items",
                 "image_count", "covid_human_impacts", "comments", "battery_status", 
                 "batteries_changed")

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
                                    placeholder = "at least one of team for follow up"),
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
                          conditionalPanel(condition = "input.project == 'focus'",
                                           uiOutput("ui_focus_camera_choices")),
                          conditionalPanel(condition = "input.project == 'other'",
                                           uiOutput("ui_other_camera_choices")),
                          conditionalPanel(condition = 'input.project != ""',
                                           tags$span(style="color:red", 
                                                     textOutput("camera_entered")), br()),
                          conditionalPanel(condition = 'input.project != "" &&
                                           (input.focus_camera_choices == "" ||
                                           input.focus_camera_choices == "")',
                                           textAreaInput("other_camera_note",
                                                         labelMandatory("If your camera was not 
                                                         in the list,enter it here (add any 
                                                        notes in action_items)."))),
                          textInput("action_items",
                                        "Action items needed (if any)"),
                          numericInput("image_count", labelMandatory("Number of pictures on SD Card (click box and type number)"),
                                       value = NULL,
                                       min = 0),
                          labelMandatory(tags$strong("COVID-related impact?: ")),"Did you observe any NEW significant human activity at or near this 
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
                          id = "upload",
                          "Here you can upload a compressed file containing images from one camera card."
                 )
)

server <- function(input, output) {
    cam_entered = FALSE
    
    observe({
        if((is.null(input$focus_camera_choices) || input$focus_camera_choices == "") &&
           (is.null(input$other_camera_choices) || input$other_camera_choices == "") &&
           (is.null(input$other_camera_note) || input$other_camera_note == "")
           ) {
            cam_entered = FALSE
            output$camera_entered = renderText("No camera id entered. Choose one 
                                               from dropdown above or enter in 
                                               space provided below.")
        } else {
            cam_entered = TRUE
            output$camera_entered = renderText("")
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
        
        shinyjs::toggleState(id = "submit", 
                             condition = mandatoryFilled)
    })
    
    sqlOutputFocusCameras = reactive({
        sqlInputFocusCameras = paste("select distinct camera_id_llnnnn from ", 
                                     Schema, ".", db_table_focus, 
                                     " order by camera_id_llnnnn;", sep="")
        dbGetQuery(con, sqlInputFocusCameras)
    })
    
    sqlOutputOtherCameras <- reactive({
        sqlInputOtherCameras<- paste("select distinct camera_id from ", 
                                     Schema, ".", db_table_other, 
                                     " order by camera_id;", sep="")
        dbGetQuery(con, sqlInputOtherCameras)
    })
    
    output$ui_focus_camera_choices <- renderUI({
        selectInput('focus_camera_choices',
                    label =labelMandatory('Focus on Wildlife camera name'),
                    choices=append(sqlOutputFocusCameras(), 
                                   c("Choose one camera" = ""), 
                                   after = 0),
                    selected = NULL, multiple = FALSE, width="450px")
    })
    
    output$ui_other_camera_choices <- renderUI({
        selectInput('other_camera_choices',
                    label =labelMandatory('Other project camera names'),
                    choices=append(sqlOutputOtherCameras(),
                                   c("Choose one camera" = ""),
                                   after = 0),
                    selected = NULL, multiple = FALSE, width="450px")
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
            # "%s_%s_%s", # If not saving file locally
            "%s_%s_%s.csv", # or "%s_%s_%s_%s.csv" if saving locally
            input$project,
            coalesce(na_if(input$focus_camera_choices, ""),
                     na_if(input$other_camera_choices, ""),
                     na_if(input$other_camera_note, "")),
            humanTime(entry_dt)
            # use line below if you worry about same username/same second 
            #  collisions or want a nice unique key. Also change format of sprintf above
            # digest::digest(data)
        )
    })
 
    formData <- reactive({
        data <- sapply(fieldsSimple, function(x) input[[x]])
        data <- c(record_id = recordID(),
                  card_retrival_date = as.character(input$date),
                  entry_datetime = as.character(entry_dt),
                  data)
        data <- t(data)
        data
    })

# Alternate approach    
    # formData = reactive({
    #     data = c(record_id = recordID(),
    #              entry_datetime = Sys.time(),
    #              names = input$names,
    #              email = input$email,
    #              card_retrival_date = input$date,
    #              clock_updated = input$clock_updated,
    #              project = input$project,
    #              focus_camera_choices = input$focus_camera_choices,
    #              other_camera_choices = input$other_camera_choices,
    #              other_camera_note = input$other_camera_note,
    #              action_items = input$action_items,
    #              image_count = input$image_count,
    #              covid_related_impact = input$covid_related_impact,
    #           ...
    #     )
    #     data = t(data)
    #     data
    # })
    
    table_id = Id(schema = Schema, 
                  table = db_table_out)
    
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
