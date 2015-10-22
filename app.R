library(shiny)
library(ggplot2)
require(rCharts)
library(googleVis)

ui <- fluidPage(
        tags$head(
                tags$script(type = "text/javascript", src = "d3.min.js"),
                tags$link(rel = 'stylesheet', type = 'text/css', href = 'tree.css')
        ),
        headerPanel("rCharts: Interactive Charts from R using polychart.js"),
        sidebarPanel(
                selectInput(inputId = "x",
                            label = "Choose X",
                            choices = c('SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth'),
                            selected = "SepalLength"),
                selectInput(inputId = "y",
                            label = "Choose Y",
                            choices = c('SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth'),
                            selected = "SepalWidth")
        ),
        mainPanel(
                #options(RCHART_LIB = 'polycharts')
                showOutput("myChart", lib = "polycharts"),
                showOutput("chartR", lib = "polycharts"),
                #
                rCharts::showOutput("chartN", lib = "nvd3"),
                rCharts::showOutput("chartN2", lib = "nvd3"),
                #
                rCharts::showOutput("chartH", "highcharts"),
                rCharts::showOutput("chartHspider", "highcharts"),
                rCharts::showOutput("chartHpl", "highcharts"),
                #
                #rCharts::showOutput("chartX", "xcharts"),
                #
                networkD3::radialNetworkOutput("treeRadial"),
                networkD3::dendroNetworkOutput("dendro"),
                #
                htmlOutput("gtree"),
                tags$div(id = "myTree", tags$script(type = "text/javascript", src = "cartesian.js"))
        )
)

server <- function(input, output) {
        
        ##########################################
        ##                 Poly                 ##
        ##########################################
        output$myChart <- renderChart({
                names(iris) = gsub("\\.", "", names(iris))
                p1 <- rPlot(input$x, input$y, data = iris, color = "Species", 
                            facet = "Species", type = 'point')
                p1$addParams(dom = 'myChart')
                return(p1)
        })
        
        hair_eye <- as.data.frame(HairEyeColor)
        output$chartR <- renderChart({
                #options(RCHART_WIDTH = 800)
                pol <- rPlot(Freq ~ Hair | Eye, color = "Eye", data = hair_eye, type = "bar")
                pol$addParams(dom = 'chartR')
                return(pol)
        })

        ##########################################
        ##                 NVD3                 ##
        ##########################################
        # http://nvd3.org/examples/index.html
        
        hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
        output$chartN <- rCharts::renderChart({
                #options(RCHART_WIDTH = 800)
                theplot <- nPlot( Freq ~ Hair, group = "Eye", data = hair_eye_male, type = 'multiBarChart') # multiBarChart, multiBarHorizontalChart, 'scatterChart', 'pieChart'
                theplot$xAxis(axisLabel = 'up in the Hair')
                #theplot$chart(color = c('brown', 'blue', '#594c26', 'green'))
                theplot$addParams(dom = 'chartN')
                return(theplot)
        })
        
        output$chartN2 <- rCharts::renderChart({
                #options(RCHART_WIDTH = 800)
                ecm <- reshape2::melt( economics[,c('date', 'uempmed', 'psavert')], id = 'date' )
                theplot <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineWithFocusChart')
                
                #dates from R to JSON will come over as number of days since 1970-01-01
                #so convert to milliseconds 86400000 in a day and then format with d3
                #on lineWithFocusChart type xAxis will also set x2Axis unless it is specified
                theplot$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
                theplot$x2Axis( tickFormat="#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#" )
                #
                theplot$addParams(dom = 'chartN2')
                return(theplot)
        })

        ##########################################
        ##              HighCharts              ##
        ##########################################
        # http://www.highcharts.com/demo
        
        output$chartH <- rCharts::renderChart({
                h1 <- Highcharts$new()
                h1$chart(type = "spline")
                h1$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
                h1$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 4), dashStyle = "shortdot")
                #
                h1$legend( symbolWidth = 80)
                h1$addParams(dom = 'chartH')
                return(h1)
        })

        output$chartHspider <- rCharts::renderChart({
                h1 <- Highcharts$new()
                h1$chart(type = "line", polar = TRUE)
                h1$series(data = c(43000, 19000, 60000, 35000, 17000, 10000), dashStyle = "longdash", pointPlacement = 'on', name = 'Allocated Budget')
                h1$series(data = c(50000, 39000, 42000, 31000, 26000, 14000), dashStyle = "shortdot", pointPlacement = 'on', name = 'Actual Spending')
                h1$xAxis( categories = c('Sales', 'Marketing', 'Development', 'Customer Support', 'Information Technology', 'Administration'), tickmarkPlacement = 'on', lineWidth = 0)
                h1$yAxis( gridLineInterpolation = 'polygon', lineWidth = 0, min = 0)
                h1$tooltip( shared = TRUE, pointFormat= '<span style="color:{series.color}">{series.name}: <b>${point.y:,.0f}</b><br/>' )
                #
                h1$legend( align = 'right', verticalAlign = 'top', y = 70, layout = 'vertical' )
                h1$addParams(dom = 'chartHspider')
                return(h1)
        })
        
        output$chartHpl <- rCharts::renderChart({
                #x <- data.frame(key = c("a", "b", "c"), value = c(1, 2, 3))
                #theplot <- hPlot(x = "key", y = "value", data = x, type = "pie")
                #
                a <- hPlot(Pulse ~ Height, data = MASS::survey, type = 'scatter', group = 'Sex', radius = 6, group.na = "Not Available")
                a$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
                a$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
                a$plotOptions(scatter = list(marker = list(symbol = 'circle')))
                a$tooltip(formatter = "#! function() { return this.x + ', ' + this.y; } !#")
                #
                a$addParams(dom = 'chartHpl')
                return(a)
        })
        
        ##########################################
        ##                xCharts               ##
        ##########################################
#         output$chartX <- rCharts::renderChart({
#                 theplot <- xPlot(Hair ~ Freq, group = "Eye", data = hair_eye_male)
#                 theplot$addParams(dom = 'chartX')
#                 return(theplot)
#         })
        
        ##########################################
        ##               Network3D              ##
        ##########################################

        output$treeRadial <- networkD3::renderRadialNetwork({
            require(networkD3)
                source("./flare.list")
                # hc <- hclust(dist(USArrests), "ave") # treeNetwork( as.treeNetwork(hc) )
            radialNetwork(List = CanadaPC, fontSize = 10, fontFamily = "serif", linkColour = "#ccc", nodeColour = "#fff",
                        nodeStroke = "steelblue", textColour = "#111", opacity = 0.9, margin = 0)
        })
        
         output$dendro <- networkD3::renderDendroNetwork({
             require(networkD3)
                 hc <- hclust( dist(USArrests), "ave")
                 dendroNetwork(hc, height = 600)
#                 dendroNetwork(hc, fontSize = 10, fontFamily = "serif", linkColour = "#ccc", nodeColour = "#fff",
#                             nodeStroke = "steelblue", textColour = "#111", opacity = 0.9, margin = 0,
#                             linkType = "diagonal", treeOrientation = "vertical", zoom = TRUE)
         })
        
        ##########################################
        ##               GoogleVis              ##
        ##########################################
        output$gtree <- renderGvis({
                gvisOrgChart(Regions, idvar = "Region", parentvar = "Parent", tipvar = "Val",
                             options=list(width=600, height=250, nodeClass = "nodeContainer",#  selectedNodeClass
                                                   size='large', allowHtml = TRUE, allowCollapse = TRUE, gvis.editor='edit Me!'))
        })

        ##########################################
        ##               Gg              ##
        ##########################################

        #output$gtree <- renderUI({})
}

shinyApp(ui = ui, server = server)