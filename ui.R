shinyUI(pageWithSidebar(
        headerPanel("Body Mass Index Calculator"),
        sidebarPanel(
                selectInput("units","Units:",
                            list("Metric (cm and kg) "="metric",
                                 "Imperial (ft and lbs)"="imperial")),
                numericInput("height", label = h3("Height"),
                             , value = 170),
                numericInput("weight", label = h3("Weight"),
                             ,value = 77),
                submitButton(text="calculate"),
                br(),
                br(),
                helpText(a("Documentation", href="http://fcdidone.github.io/BMI/App_Documentation.html",target="_blank"))
        ),
        mainPanel(
                h3('BMI:'),
                br(),
                h4(textOutput("ht")),
                br(),
                h4(textOutput("wt")),
                br(),
                h4(textOutput("BMI")),
                h4(textOutput("l")),
                plotOutput("pl")
                
                
                
                
                )
        
        
        ))