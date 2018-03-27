

#the shiny plot!  

ui <- fluidPage(
  sidebarPanel(
    imageOutput("map", height=300),
    imageOutput("population", height=250),
    radioButtons("xax", "X Axis:",
                 c("Linear" = "lin",
                   "Log Sacle" = "ln")),
    choiceValues = list(
      "lin", "ln"),
    radioButtons("yax", "Y Axis:",
                 c("Linear" = "lin",
                   "Log Scale" = "ln")),
    choiceValues = list(
      "lin", "ln"),
    selectInput(inputId="yvar", label="Y Variable", choices= c("Child Mortality", "Life Expectancy", "Fertility")), 
    sliderInput(inputId="year", label="Year", min=1960, max=2014,2014),
    textOutput("source")),
  mainPanel(
    plotOutput("plot", hover=hoverOpts(id = "plot_hover", delay = 100), height= 800),
    textOutput("yearlab"),
    textOutput("yaxislab"),
    textOutput("xaxislab"),
    textOutput("poplab")))


server <- function(input, output, session) { 
  
  df <- reactive({subset(thedata2,thedata2$year==input$year)})
  
 # output$populations <- renderPlot({plot(x,y, xlim= c(0,3), ylim=c(.95,1.005), cex=sizefx(size), axes=FALSE, ylab="", xlab="", pch=16, col="gray", main="Population Size")
    #text(1,.994,"100,000")
    #text(1.5,.994,"10 million")
    #text(2,.994,"100 million")
    #text(2.5,.994,"1 billion")})

  
  xaxis <- reactive({
    switch(input$xax,
           "lin" = df()$GDP,
           "ln" = log(df()$GDP))})
  yaxis <- reactive({
    switch(input$yax,
           "lin" = yvar(),
           "ln" = log(yvar()) )})
  yvar <- reactive( {
    switch(input$yvar,
           "Child Mortality" = df()$mortality,
           "Life Expectancy" = df()$lifeexp,
           "Fertility" = df()$fertility)})
  ylab <- reactive( {
    switch(input$yvar,
           "Child Mortality" = "Child Mortality (Deaths per 10000 children)",
           "Life Expectancy" = "Life Expectency (years)",
           "Fertility" = "Fertitly (Average Child Births per Female)")})
  
  output$source <- renderText("Source: World Bank API")
  
  
  output$plot <- renderPlot({
    
    titlea <- reactive({
      switch(input$yvar,
             "Child Mortality" = "Child Mortality and GDP",
             "Life Expectancy" = "Life Expectency and GDP",
             "Fertility" = "Fertitly and GDP")})
    
    xaxtic <-  reactive({
      switch(input$xax,
             "lin" = axis(1,at=c(0,10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000,120000), labels= c(0,"10,000","20,000","30,000","40,000","50,000","60,000","70,000","80,000","90,000","100,000","110,000","120,000")),
             "ln" = axis(1,at=c(log(50),log(250),log(500),log(1000),log(2500),log(5000),log(10000),log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000),log(100000),log(110000),log(120000)), labels= c("50","250","500", "1,000","2,500", "5,000","10,000","20,000","30,000","40,000","50,000","60,000","70,000","80,000","90,000","100,000","110,000","120,000"))) })
    
    xlimit <-  reactive({
      switch(input$xax,
             "lin" = 120000,
             "ln" = log(120000))})
    
    xlimita <-  reactive({
      switch(input$xax,
             "lin" = 0,
             "ln" = log(35))})
    
    ylin <- reactive( {
      switch(input$yvar,
             "Child Mortality" = axis(2,at=c(0,25,50,75,100,125,150,175,200,225,250,275,300,325,350,375,400), labels= c("0","25","50","75","100","125","150","175","200","225","250","275","300","325","350","375","400")),
             "Life Expectancy" = axis(2,at=c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90), labels= c("15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90")) ,
             "Fertility" = axis(2,at=c(0,.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9), labels= c("0",".5","1","1.5","2","2.5","3","3.5","4","4.5","5","5.5","6","6.5","7","7.5","8","8.5","9")))})
    
    ylog <- reactive( {
      switch(input$yvar,
             "Child Mortality" = axis(2,at=c(0,log(5),log(15),log(25),log(50),log(75),log(100),log(125),log(150),log(175),log(200),log(225),log(250),log(275),log(300),log(325),log(350),log(375),log(400)), labels= c("0","5","15","25","50","75","100","125","150","175","200","225","250","275","300","325","350","375","400")),
             "Life Expectancy" = axis(2,at=c(log(15),log(20),log(25),log(30),log(35),log(40),log(45),log(50),log(55),log(60),log(65),log(70),log(75),log(80),log(85),log(90)), labels= c("15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90")),
             "Fertility" = axis(2,at=c(0,log(.5),log(1),log(1.5),log(2),log(2.5),log(3),log(3.5),log(4),log(4.5),log(5),log(5.5),log(6),log(6.5),log(7),log(7.5),log(8),log(8.5),log(9)), labels= c("0",".5","1","1.5","2","2.5","3","3.5","4","4.5","5","5.5","6","6.5","7","7.5","8","8.5","9")))})
    
    ylimitlin <- reactive( {
      switch(input$yvar,
             "Child Mortality" = 400,
             "Life Expectancy" = 90,
             "Fertility" = 9)}) 
    
    ylimitlina <- reactive( {
      switch(input$yvar,
             "Child Mortality" = 0,
             "Life Expectancy" = 15,
             "Fertility" = 0)}) 
    
    ylimitloga <- reactive( {
      switch(input$yvar,
             "Child Mortality" = 0,
             "Life Expectancy" = log(15),
             "Fertility" = 0)}) 
    
    ylimitlog <- reactive( {
      switch(input$yvar,
             "Child Mortality" = log(400),
             "Life Expectancy" = log(90),
             "Fertility" = log(9))}) 
    
    yaxtic <-  reactive({
      switch(input$yax,
             "lin" = ylin() ,
             "ln" = ylog() )})
    
    ylimit <-  reactive({
      switch(input$yax,
             "lin" = ylimitlin(),
             "ln" = ylimitlog())})
    
    ylimita <-  reactive({
      switch(input$yax,
             "lin" = ylimitlina(),
             "ln" = ylimitloga())})
    
    plot(xaxis(), yaxis(), main=titlea(), 
         cex.main=(session$clientData$output_plot_width/200),col="#5375BA60",pch=21, cex=sizefx(df()$population), 
         bg=colorf(df()$continent1), ylab=ylab(), xlab="Income per person (GDP/capita, PPP$ inflation-adjusted)"
         ,cex.lab=1.4, axes=FALSE, ylim=c(ylimita(),ylimit()), xlim=c(xlimita(),xlimit())) 
    yaxtic() 
    xaxtic()
    mtext(input$year, side=3, line=-30, cex=(session$clientData$output_plot_width/35), col="#b4b8b930")})
  output$map <- renderImage({
    width  <- session$clientData$output_map_width
    height <- session$clientData$output_map_height
    filename <-normalizePath(file.path("Data/map.jpg"))
    list(src = filename, width = width,
         height = 260)
  }, deleteFile = FALSE)
  
  output$population <- renderImage({
    width1  <- session$clientData$output_population_width
    height1 <- session$clientData$output_population_height
    filename1 <-normalizePath(file.path("Data/Population size.png"))
    list(src = filename1, width = width1,
         height = 200)
  }, deleteFile = FALSE)
  
  
  output$yearlab <- renderPrint({ 
    
    xaxisa <- reactive({
      switch(input$xax,
             "lin" = "GDP",
             "ln" ="lngdp" )})
    
    variabley <- reactive({
      ifelse(input$yax=="lin",
             switch(input$yvar,
                    "Child Mortality" = "mortality",
                    "Life Expectancy" = "lifeexp",
                    "Fertility" =       "fertility"),
             switch(input$yvar,
                    "Child Mortality" = "lnmort",
                    "Life Expectancy" = "lnlexp",
                    "Fertility" =       "lnfert"))})
    
    
    
    
    write.table("Country Name:     ", quote=FALSE, row.names=FALSE, col.names=FALSE);
    write.table(nearPoints(df(), xvar=xaxisa(), yvar=variabley(), input$plot_hover, maxpoints=1)[1], row.names=FALSE, col.names=FALSE, quote=FALSE)})
  
  output$yaxislab <- renderPrint({ 
    
    titles <- reactive({
      switch(input$yvar,
             "Child Mortality" = "Child Mortality: ",
             "Life Expectancy" = "Life Expectency: ",
             "Fertility" =       "Fertitly:        ")})
    
    xaxisa <- reactive({
      switch(input$xax,
             "lin" = "GDP",
             "ln" ="lngdp" )})
    
    variabley <- reactive({
      ifelse(input$yax=="lin",
             switch(input$yvar,
                    "Child Mortality" = "mortality",
                    "Life Expectancy" = "lifeexp",
                    "Fertility" =       "fertility"),
             switch(input$yvar,
                    "Child Mortality" = "lnmort",
                    "Life Expectancy" = "lnlexp",
                    "Fertility" =       "lnfert"))})
    
    numone <- reactive({
      switch(input$yvar,
             "Child Mortality" = 6,
             "Life Expectancy" = 8,
             "Fertility" = 7)})
    
    
    write.table(titles(), quote=FALSE, row.names=FALSE, col.names=FALSE);
    write.table(nearPoints(df(), xvar=xaxisa(), yvar=variabley(), input$plot_hover, maxpoints=1)[numone()], row.names=FALSE, col.names=FALSE, quote=FALSE)})
  
  output$xaxislab <- renderPrint({ 
    
    xaxisa <- reactive({
      switch(input$xax,
             "lin" = "GDP",
             "ln" ="lngdp" )})
    
    variabley <- reactive({
      ifelse(input$yax=="lin",
             switch(input$yvar,
                    "Child Mortality" = "mortality",
                    "Life Expectancy" = "lifeexp",
                    "Fertility" =       "fertility"),
             switch(input$yvar,
                    "Child Mortality" = "lnmort",
                    "Life Expectancy" = "lnlexp",
                    "Fertility" =       "lnfert"))})
    
    
    
    write.table("GDP:             ", quote=FALSE, row.names=FALSE, col.names=FALSE);
    write.table(nearPoints(df(), xvar=xaxisa(), yvar=variabley(), input$plot_hover, maxpoints=1)[5], row.names=FALSE, col.names=FALSE, quote=FALSE)})
  
  output$poplab <- renderPrint({ 
    
    
    xaxisa <- reactive({
      switch(input$xax,
             "lin" = "GDP",
             "ln" ="lngdp" )})
    
    variabley <- reactive({
      ifelse(input$yax=="lin",
             switch(input$yvar,
                    "Child Mortality" = "mortality",
                    "Life Expectancy" = "lifeexp",
                    "Fertility" =       "fertility"),
             switch(input$yvar,
                    "Child Mortality" = "lnmort",
                    "Life Expectancy" = "lnlexp",
                    "Fertility" =       "lnfert"))})
    
    
    
    write.table("Population:        ", quote=FALSE, row.names=FALSE, col.names=FALSE);
    write.table(nearPoints(df(), xvar=xaxisa(), yvar=variabley(), input$plot_hover, maxpoints=1)[4], row.names=FALSE, col.names=FALSE, quote=FALSE)})
  
}

shinyApp(ui=ui, server=server)



