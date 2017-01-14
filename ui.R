

fluidPage(
  titlePanel("Compute your internal pagerank"),
  sidebarLayout(
    sidebarPanel(
      a("data-seo.com", href="https://data-seo.com"),
      tags$hr(),
      p('Step 1 : Export your outlinks data from ScreamingFrog'),      
      fileInput('file1', 'Choose file to upload (e.g. all_outlinks.csv)',
                accept = c(
                  'text/csv'
                )
                , multiple = FALSE
      ),
      # p('Step 2 : Choose your IP field, your date field, your url field by select one or two columns ( tested with apache logs )'
      #   #,a(href = 'mtcars.csv', 'More Info')
      # ),
      # 
      # selectInput("ipbot", "IP:",
      #             c("V1" = "v1"), multiple=TRUE),
      # 
      # selectInput("url", "URL:",
      #             c("V1" = "v1",
      #               "V2" = "v2",
      #               "V3" = "v3"), multiple=TRUE),
      # 
      # selectInput("date", "Date:",
      #             c("V1" = "v1",
      #               "V2" = "v2",
      #               "V3" = "v3"), multiple=TRUE), 
      # 
      # selectInput("useragent", "Useragent:",
      #             c("V1" = "v1",
      #               "V2" = "v2",
      #               "V3" = "v3"), multiple=TRUE), 
      # 
      # radioButtons("botmode", "Bot:",
      #              c("GoogleBot Desktop" = "googlebotdesktop",
      #                "GoogleBot Mobile" = "googlebotmobile"
      #                #,"Bingbot" = "bingbot"
      #                #,"Baidu" = "baidubot"
      #                #,"Yandex" = "yandexbot"
      #              )
      # ),
      # 
      #submitButton("Prepare Data"),
      tags$hr(),
      downloadButton('downloadData', 'Download CSV')
      
    ),
    mainPanel(
      h3(textOutput("caption")),
      tags$hr(),
      tableOutput('contents')
    )
  )
)

