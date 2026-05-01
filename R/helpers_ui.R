# R/helpers_ui.R

# 1) Blue "info" callout used all over
info_callout <- function(title, text = NULL, md_file = NULL) {
  body <- if (!is.null(md_file)) includeMarkdown(md_file) else text
  div(
    style = "margin-top:20px;padding:10px;background-color:#f8f9fa;border-left:3px solid #007bff;border-radius:4px;",
    h5(title, style = "margin-top:0;color:#007bff;"),
    div(style = "margin-bottom:0;font-size:14px;line-height:1.4;", body)
  )
}

dataUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("files"), "Choose files", multiple = TRUE,
              accept = c(".csv",".tsv",".xlsx",".xls")),
    fluidRow(
      column(6,
             radioButtons(ns("source_format"), "Source format:",
                          choices = c("Pilcomayo.net"="pilco", "By Parameter"="by_param"),
                          inline = TRUE, selected = "pilco")
      ),
      column(6,
             radioButtons(ns("current_lang"), "Current language in file(s):",
                          choices = c("English" = "en", "Español" = "es"),
                          inline = TRUE, selected = "es")  # default to your usual raw language
      )
    ),
    fluidRow(
      column(6,
             radioButtons(ns("media_type"), "Media included (select one):",
                          choices = c("Sediment"="sediment", "Water"="water"),
                          inline = TRUE, selected = "water")
      ),
      column(6,
             radioButtons(ns("translate_to"), "Translate to:",
                          choices = c("English"="en","Español"="es"),
                          inline = TRUE, selected = "en")
      ),
      column(6, 
             actionButton(ns("upload_data"), "Upload data file")
             )
    ), 
    tags$hr(),
    tableOutput(ns("files_table"))
    )
}