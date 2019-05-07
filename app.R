#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(shiny)

# ------- functions -----------

gh_url <- function(repo) {
    str_c("https://github.com/", repo)
}

gh_src_url <- function(repo, file, line = NA_character_) {
    line <- ifelse(is.na(line), "", str_c("#L", line))
    str_c(
        "https://github.com/",
        repo,
        "/blob/master/", 
        file, line
    )
}

# ---------  data ------------

packages <- read_csv(
    "data/_collect-params.csv",
    col_types = cols(
      target_pkg = col_character(),
      target_repo = col_character(),
      foreign_pkg = col_character(),
      foreign_repo = col_character()
    )
)

target_pkg <- packages$target_pkg[1]
target_repo <- packages$target_repo[1]

packages <- packages %>%
    transmute(
        foreign_pkg,
        foreign_repo,
        foreign_repo_url = paste0("https://github.com/", foreign_repo)
    )

target_objects <- read_csv(
    file.path("data", paste0(target_pkg, "__objects.csv")),
    col_types = cols(
        object_name = col_character(),
        index = col_double(),
        object_class = col_character(),
        src_file = col_character(),
        src_line_start = col_double(),
        src_line_end = col_double()
    )
) %>%
    mutate(src_link = gh_src_url(target_repo, src_file, src_line_start))

target_functions <- target_objects %>%
    filter(object_class == "function")

target_ggproto <- target_objects %>%
    filter(str_detect(object_class, "ggproto"))

function_refs <- list.files("data", "-function_refs\\.csv$", full.names = TRUE) %>%
    map_dfr(
        read_csv,
        col_types = cols(
            object_name = col_character(),
            index = col_double(),
            foreign_object = col_character(),
            foreign_src_file = col_character(),
            foreign_src_line_start = col_double(),
            foreign_src_line_end = col_double(),
            call_text = col_character()
        )
    ) %>%
    left_join(packages %>% select(foreign_pkg, foreign_repo), by = "foreign_pkg") %>%
    mutate(
        reference_link = gh_src_url(foreign_repo, foreign_src_file, foreign_src_line_start)
    )

ggproto_refs <- list.files("data", "-ggproto_refs\\.csv$", full.names = TRUE) %>%
    map_dfr(
        read_csv,
        col_types = cols(
            object_name = col_character(),
            index = col_double(),
            foreign_object = col_character(),
            foreign_object_class = col_character(),
            foreign_src_file = col_character(),
            foreign_src_line_start = col_double(),
            foreign_src_line_end = col_double()
        )
    )


function_ref_as_li <- function(ref) {
    tags$li(
        if(!is.na(ref$reference_link)) {
            span(
                "At",
                a(href = ref$reference_link, ref$foreign_src_file)
            )
        },
        "in",
        a(href = gh_url(ref$foreign_repo), ref$foreign_repo),
        br(),
        strong(code(paste0(ref$foreign_pkg, "::", ref$foreign_object, "()"))),
        pre(
            code(
                paste(
                    styler::style_text(ref$call_text),
                    collapse = "\n"
                )
            )
        )
    )
}

# ------------ UI ---------

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel(strong("ggplot2"), "usage"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("function_name",
                            "ggplot2 function:",
                            choices = target_functions$object_name),
            selectizeInput("foreign_pkg",
                           "In package:",
                           choices = c("(All)", packages$foreign_pkg))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("functionHeader"),
            tags$hr(),
            uiOutput("functionUsage")
        )
    )
)

# -------------- server -----------

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$functionHeader <- renderUI({
        target_obj <- target_functions %>% 
            filter(object_name == !!input$function_name)
        
        if(nrow(target_obj) > 0) {
            p(
                code(paste0(target_obj$object_name, "()")),
                "at",
                a(href = target_obj$src_link, target_obj$src_file),
                "in",
                a(href = gh_url(target_repo), target_repo)
            )
        } else {
            p("Searching...")
        }
    })
    
    output$functionUsage <- renderUI({
        references <- function_refs %>% 
            filter(object_name == !!input$function_name)
        
        if(input$foreign_pkg != "(All)") {
            references <- references %>% filter(foreign_pkg == !!input$foreign_pkg)
        }
        
        div(
            p(strong(nrow(references)), "references"),
            do.call(tags$ul, map(transpose(references), function_ref_as_li))
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
