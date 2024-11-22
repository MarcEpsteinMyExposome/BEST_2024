library(htmltools)

# Create a simple tabset with manually assigned IDs
tab1 <- tags$div(
  id = "custom-tab-1", # Manually assign an ID
  "Content of Tab 1"
)

tab2 <- tags$div(
  id = "custom-tab-2", # Manually assign an ID
  "Content of Tab 2"
)

# Create the tabset panel
tabset <- tagList(
  tags$div(
    class = "tabset",
    tags$ul(
      class = "nav nav-tabs",
      tags$li(class = "active", tags$a(href = "#custom-tab-1", "Tab 1")),
      tags$li(tags$a(href = "#custom-tab-2", "Tab 2"))
    ),
    tags$div(
      class = "tab-content",
      tab1,
      tab2
    )
  )
)

# Render the tabset as HTML
rendered_html <- renderTags(tabset)

# Print the rendered HTML to check if IDs are respected
cat(rendered_html$html)
