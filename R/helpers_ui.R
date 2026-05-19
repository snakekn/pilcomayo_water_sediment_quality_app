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
    fileInput(ns("files"), span("Choose files", `data-i18n`="upload_choose_files"),
              multiple = TRUE, accept = c(".csv",".tsv",".xlsx",".xls")),
    fluidRow(
      column(6,
             radioButtons(ns("source_format"),
                          span("Source format:", `data-i18n`="upload_format_label"),
                          choices = c("Pilcomayo.net"="pilco", "By Parameter"="by_param"),
                          inline = TRUE, selected = "pilco")
      ),
      column(6,
             radioButtons(ns("current_lang"),
                          span("Current language in file(s):", `data-i18n`="upload_lang_label"),
                          choices = c("English" = "en", "Español" = "es"),
                          inline = TRUE, selected = "es")
      )
    ),
    fluidRow(
      column(6,
             radioButtons(ns("media_type"),
                          span("Media included (select one):", `data-i18n`="upload_media_label"),
                          choices = c("Sediment"="sediment", "Water"="water"),
                          inline = TRUE, selected = "water")
      ),
      column(6,
             radioButtons(ns("translate_to"),
                          span("Translate to:", `data-i18n`="upload_translate_label"),
                          choices = c("English"="en","Español"="es"),
                          inline = TRUE, selected = "en")
      ),
      column(6,
             actionButton(ns("upload_data"), span("Upload data file", `data-i18n`="upload_btn"))
             )
    ),
    tags$hr(),
    tableOutput(ns("files_table"))
    )
}