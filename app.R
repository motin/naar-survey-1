if (!require(shiny)) install.packages("shiny", dependencies = TRUE);
if (!require(shiny.i18n)) {
  if (!require(devtools)) install.packages("devtools", dependencies = TRUE);
  devtools::install_github("Appsilon/shiny.i18n", dependencies = TRUE);
}
if (!require(shinythemes)) install.packages("shinythemes", dependencies = TRUE);
if (!require(rvest)) install.packages("rvest", dependencies = TRUE);
if (!require(digest)) install.packages("digest", dependencies = TRUE);
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE);

library(shiny)
library(shiny.i18n)
library(shinythemes)
library(rvest)
library(digest) # digest() Create hash function digests for R objects
library(dplyr)

formName <- "survey-results"
resultsDir <- file.path("data", formName)
dir.create(resultsDir, recursive = TRUE, showWarnings = FALSE)

loadAddonsData <- function() {
  addons <- readRDS("addons.rds")
}

getAddonsThatMatchesAddonGuids <- function(addons, addonGuids) {
  selectionOfAddons <- addons %>%
    filter(guid %in% addonGuids)
}

strip_html <- function(s) {
  rvest::html_text(xml2::read_html(paste0("<p>",as.character(s),"</p>",sep="")))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
getFormattedTimestamp <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

getFormValues <- function(input) {
  t(sapply(names(input), function(x) x = input[[x]]))
}

translator <- Translator$new(translation_csvs_path = "translations")

# load add-on data
addons <- loadAddonsData()

# https://github.com/daattali/advanced-shiny/tree/master/reactive-dedupe
dedupe <- function(r) {
  makeReactiveBinding("val")
  observe(val <<- r(), priority = 10)
  reactive(val)
}

ui <- fluidPage(theme = shinytheme("cerulean"), 
                #tags$head(includeHTML(("og-metadata.html"))),
                #tags$head(includeHTML(("google-analytics.html"))),
                
                #includeCSS("style.css"),
                
                conditionalPanel(
                  # only show this form before the form is submitted
                  condition = "output.phase != 'done'",
                  uiOutput("header")
                ),
                conditionalPanel(
                  # only show this form before the form is submitted
                  condition = "output.phase == 'part1'",
                  uiOutput("surveyPart1")
                ),
                conditionalPanel(
                  # only show this form before the form is submitted
                  condition = "output.phase == 'part2'",
                  uiOutput("surveyPart2")
                ),
                conditionalPanel(
                  # thank you screen after form is submitted
                  condition = "output.phase == 'done'",
                  h3("Thank you for your participation!")
                )
                #textInput("name", "Name"),
                #numericInput("age", "Age", 25)
)

server <- function(input, output, session) {
  
  # Use to restore hard-coded form data
  restored <- data.frame(id_voting=as.character(c(
  )), vote = as.character(c(
  )))
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['name']])) {
      updateTextInput(session, "name", value = query[['name']])
    }
    if (!is.null(query[['age']])) {
      updateNumericInput(session, "age", value = query[['age']])
    }
  })
  
  getSelectionOfAddons <- reactive({
    result <- input$selectionOfAddons
    # or rather only via query string
    query <- parseQueryString(session$clientData$url_search)
    message("querystring parsed:")
    str(query)
    
    # select the ones relevant for this run
    # some random guids
    addonGuids <- c(
      "{c45c406e-ab73-11d8-be73-000a95be3b12}", 
      "DoesAmazonShipTo@usefulhelper.com", 
      "{a949831f-d9c0-45ae-8c60-91c2a86fbfb6}", 
      "jid0-1goESrxnxB5zVAoHJkD3TYqF6lB@jetpack",
      "{5f27f390-2663-4c7d-addb-a924ba085966}"
    )
    # motin
    addonGuids <- c(
      "{c2c003ee-bd69-42a2-b0e9-6f34222cb046}",
      "{02274e0c-d135-45f0-8a9c-32b35110e10d}",
      "support@lastpass.com",
      "extension@one-tab.com",
      "taarexpv3@shield.mozilla.org",
      "{443830f0-1fff-4f9a-aa1e-444bafbc7319}",
      # extra example add-ons
      "{a949831f-d9c0-45ae-8c60-91c2a86fbfb6}", 
      "jid0-1goESrxnxB5zVAoHJkD3TYqF6lB@jetpack",
      "{5f27f390-2663-4c7d-addb-a924ba085966}"
    )
    
    addonsThatMatchesAddonGuids <- getAddonsThatMatchesAddonGuids(addons, addonGuids)
    
  })
  
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$languages) {
    } else {
      message("No valid language selected yet. Defaulting to en")
      selected <- "en"
    }
    translator$set_translation_language(selected)
    translator
  })
  
  extractAddonUserMotivationsFromInput <- dedupe(reactive({
    if (is.null(input$continueButton) || input$continueButton < 1) return(NULL)
    #message(" === extractAddonUserMotivationsFromInput ===")
    addonGuidColumn <- character()
    userMotivationsColumn <- character()
    selectionOfAddons <- getSelectionOfAddons()
    for (addonGuid in selectionOfAddons$guid){
      addon <- selectionOfAddons[selectionOfAddons$guid == addonGuid,]
      inputIdBase <- paste0("motivations[",addon$guid,"]", sep="")
      motivationInputId <- paste0(inputIdBase, "-motivation", sep="")
      motivationNaInputId <- paste0(inputIdBase, "-motivation-na", sep="")
      motivation <- isolate(input[[motivationInputId]])
      motivationNa <- isolate(input[[motivationNaInputId]])
      #message("motivation:")
      #str(motivation)
      #str(addonGuid)
      #str(motivationInputId)
      if (!is.null(motivation) && motivation != "" && (is.null(motivationNa) || motivationNa == FALSE)) {
        addonGuidColumn <- append(addonGuidColumn, addonGuid)
        userMotivationsColumn <- append(userMotivationsColumn, motivation)
      }
    }
    addonUserMotivations <- data.frame(addonGuid=addonGuidColumn,userMotivation=as.character(userMotivationsColumn))
    # addonUserMotivations <- addonUserMotivations %>% distinct(userMotivation, .keep_all = TRUE) # removes duplicates in relation to the userMotivation column
  }))
  
  getInputValue <- function(input, inputId, defaultValue) {
    #message("inputId")
    #print(inputId)
    inputValue <- defaultValue
    if (inputId %in% isolate(names(input))) {
      inputValue <- isolate(input[[inputId]])
    }
    inputValue
  }
  
  output$header <- renderUI({
    
    languageInput <- selectInput("selected_language",
                                 i18n()$t("change-language"),
                                 choices = translator$languages[! translator$languages %in% c("refs")],
                                 selected = translator$translation_language)
    
    ui <- tagList(
      fluidRow(column(9, h1(i18n()$t("title")))#,
               #column(3, languageInput)
      ),
      fluidRow(column(12, p(i18n()$t("instructions-html"))))
    )
    
  })
  
  output$surveyPart1 <- renderUI({
    
    ui <- tagList(
      fluidRow(column(12, h2(i18n()$t("part-one-title"))))
    )
    
    # The first part of the survey lists the add-ons and asks what the user was trying to remedy/achieve by choosing the add-on. The user either fill in a free-text field as answer or tick a box for “I don’t know/remember”, and then click continue.
    # for each survey-included-addon:
    # {Add-on Title}
    # What were you trying to remedy/achieve by choosing this add-on?
    # textarea
    # [_] I don’t know/remember
    # ...
    
    selectionOfAddons <- getSelectionOfAddons()
    message("selectionOfAddons - part 1:")
    str(selectionOfAddons)
    
    if (length(selectionOfAddons$guid) > 0) {
      
      for (addonGuid in selectionOfAddons$guid){
        addon <- selectionOfAddons[selectionOfAddons$guid == addonGuid,]
        
        inputIdBase <- paste0("motivations[",addon$guid,"]", sep="")
        #message("Addon: ")
        #str(addon)
        #str(addon$guid)
        
        motivationInputId <- paste0(inputIdBase, "-motivation", sep="")
        motivationNaInputId <- paste0(inputIdBase, "-motivation-na", sep="")
        
        motivationInput <- textAreaInput(motivationInputId, "What were you trying to remedy/achieve by choosing this add-on?", value = getInputValue(input, motivationInputId, ""), width="100%")
        motivationNaInput <- checkboxInput(motivationNaInputId, label = "I don’t know/remember", value = getInputValue(input, motivationNaInputId, FALSE))
        
        description <- strip_html(paste("<p>", as.character(addon$description_en_us), "</p>"))
        row <- fluidRow(column(1, 
                               tags$img(src = addon$icon_url, width = "100%")), 
                        column(11, 
                               h4(addon$name_en_us), 
                               #p(description),
                               motivationInput,
                               motivationNaInput)
        )
        ui <- tagAppendChildren(ui, hr(), row)
        
      }
      
    } else {
      
      ui <- tagAppendChildren(ui,
                              fluidRow(column(12, h2(i18n()$t("part-one-empty-html"))))
      )
      
    }
    
    ui <- tagAppendChildren(ui, 
                            hr(), 
                            actionButton(inputId = "continueButton", label = "Continue")
    )
    
  })
  
  output$surveyPart2 <- renderUI({
    
    ui <- tagList(
      fluidRow(column(12, h2(i18n()$t("part-two-title")))),
      actionButton(inputId = "backButton", label = "Go back to Part 1")
    )
    
    # The second part of the survey is generated from the answers they gave in part one, now asking the user how important each motivation is and how satisfied they were with stock Firefox (without their add-on) and how satisfied they are now with their add-on.
    # You wrote “I wanted to prevent myself from finding myself on Facebook again and again when I tried to work”
    # How important is this to you?
    #   1 - Not at all important
    #   2 - Somewhat important
    #   3 - Important
    #   4 - Very important
    #   5 - Extremely important
    # How satisfied were you in regards to this without “LeechBlock NG"?
    #   1 - Not at all satisfied
    #   2 - Somewhat satisfied
    #   3 - Satisfied
    #   4 - Very satisfied
    #   5 - Extremely satisfied
    # How satisfied are you in regards to this now with “LeechBlock NG"?
    #   1 - Not at all satisfied
    #   2 - Somewhat satisfied
    #   3 - Satisfied
    #   4 - Very satisfied
    #   5 - Extremely satisfied
    
    addonUserMotivations <- extractAddonUserMotivationsFromInput()
    #message("addonUserMotivations: ")
    #str(addonUserMotivations)
    #str(length(addonUserMotivations$addonGuid))
    
    selectionOfAddons <- getSelectionOfAddons()
    #message("selectionOfAddons - part 2:")
    #str(selectionOfAddons)
    
    if (length(addonUserMotivations$addonGuid) > 0) {
      
      # if part 1 is filled, render part 2:
      output$phase <- reactive({ "part2" })
      
      for (addonGuid in addonUserMotivations$addonGuid) {
        addon <- selectionOfAddons[selectionOfAddons$guid == addonGuid,]
        addonUserMotivation <- addonUserMotivations[addonUserMotivations$addonGuid == addonGuid,]
        userMotivation <- addonUserMotivation$userMotivation
        
        inputIdBase <- paste0("ratings[",addonGuid,"]", sep="")
        
        importanceRatingInputId <- paste0(inputIdBase, "-importance", sep="")
        satisfactionWithoutAddonInputId <- paste0(inputIdBase, "-satisfactionWithoutAddon", sep="")
        satisfactionWithAddonInputId <- paste0(inputIdBase, "-satisfactionWithAddon", sep="")
        
        ratingChoices <- c(1,2,3,4,5)
        importanceRatingChoices <- ratingChoices
        names(importanceRatingChoices) <- c("Not at all important", "Somewhat important", "Important", "Very important", "Extremely important")
        satisfactionRatingChoices <- ratingChoices
        names(satisfactionRatingChoices) <- c("Not at all satisfied", "Somewhat satisfied", "Satisfied", "Very satisfied", "Extremely satisfied")
        
        inline <- FALSE
        importanceRatingInput <- radioButtons(importanceRatingInputId, "How important is this to you?", 
                                              choices = importanceRatingChoices,
                                              selected = getInputValue(input, importanceRatingInputId, NULL), inline = inline, width = "100%")
        satisfactionWithoutAddonInput <- radioButtons(satisfactionWithoutAddonInputId, paste0("How satisfied were you in regards to this without \"", strip_html(addon$name_en_us), "\"?", sep=""), 
                                                      choices = satisfactionRatingChoices,
                                                      selected = getInputValue(input, satisfactionWithoutAddonInputId, NULL), inline = inline, width = "100%")
        satisfactionWithAddonInput <- radioButtons(satisfactionWithAddonInputId, paste0("How satisfied are you in regards to this now with \"", strip_html(addon$name_en_us), "\"?", sep=""), 
                                                   choices = satisfactionRatingChoices,
                                                   selected = getInputValue(input, satisfactionWithAddonInputId, NULL), inline = inline, width = "100%")
        
        # You wrote “I wanted to prevent myself from finding myself on Facebook again and again when I tried to work”
        # "For \"",strip_html(addon$name_en_us),"\", y
        text <- paste0("You wrote \"",strip_html(userMotivation),"\"")
        
        ui <- tagAppendChildren(ui, hr(), 
                                fluidRow(column(12,h4(text))),
                                fluidRow(column(12,importanceRatingInput)),
                                fluidRow(column(12,satisfactionWithoutAddonInput)),
                                fluidRow(column(12,satisfactionWithAddonInput))
        )
        
      }
      
    } else {
      
      #ui <- tagAppendChildren(ui,
      #                        fluidRow(column(12, h2(i18n()$t("part-two-empty-html"))))
      #)
      
    }
    
    # Finally, the user is able to supply optional comments before submitting the information and ending the study.
    
    commentsInputId <- "comments"
    ui <- tagAppendChildren(ui, 
                            hr(), 
                            fluidRow(column(12, h4(i18n()$t("part-two-end-html")))),
                            fluidRow(column(12,textAreaInput(commentsInputId, "Any comments you want to share before we are done? (optional)", value = getInputValue(input, commentsInputId, ""), width="100%"))),
                            hr(), 
                            actionButton(inputId = "submitButton", label = "Submit")
    )
    
    # (Analasis is simply ODI analysis with the needs being the ones listed and rated upon in Part 2)
    
    ui
    
  })
  
  # we need to have a quasi-variable flag to indicate when the form was submitted
  output$phase <- reactive({ "part1" })
  outputOptions(output, 'phase', suspendWhenHidden = FALSE)
  
  # advance the form
  observeEvent(input$continueButton, {
    message("input$continueButton pressed")
    #str(input$continueButton)
    output$phase <- reactive({ "part2" })
  })
  
  # go back in the form
  observeEvent(input$backButton, {
    message("input$backButton pressed")
    #str(input$backButton)
    output$phase <- reactive({ "part1" })
  })
  
  # submit the form  
  observeEvent(input$submitButton, {
    message("input$submitButton: ")
    #str(input$submitButton)
    
    # read the form data into a dataframe
    formValues <- isolate(getFormValues(input))
    
    # generate a file name based on timestamp, user name, and form contents
    isolate(
      fileName <- paste0(
        paste(
          getFormattedTimestamp(),
          input$lastName,
          input$firstName,
          digest(formValues, algo = "md5"),
          sep = "_"
        ),
        ".csv"
      )
    )
    
    # write out the results
    ### This code chunk writes a response and will have to change
    ### based on where we store persistent data
    write.csv(x = formValues, file = file.path(resultsDir, fileName),
              row.names = FALSE)
    ### End of writing data
    
    # indicate the the form was submitted to show a thank you page so that the
    # user knows they're done
    output$phase <- reactive({ "done" })
    
    message("Response saved")
    
  })
  
}

shinyApp(ui = ui, server = server)
