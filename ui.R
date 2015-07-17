shinyUI(pageWithSidebar(

    titlePanel(title="MLViS: machine learning-based virtual screening tool"),

    sidebarPanel(
        conditionalPanel(condition="input.tabs1=='Introduction'",
            HTML('<p align="center"><img src="intro.png" width=300 height=300></p>'),
            tags$head(includeScript("google-analytics.js"))
        ),

        conditionalPanel(condition="input.tabs1=='Data upload'",
            h4("Enter your data"),

            radioButtons("dataInput", "", list("Load example data"=1, "Upload a file"=2, "Paste your data"=3, "Single molecule"=4), selected=1),
            HTML('<br>'),
            checkboxInput("molName", "Data has PubChem CID numbers", FALSE),


        conditionalPanel(condition="input.dataInput=='1'",
            HTML('<br>'),
            radioButtons("sampleData", "", list("Example data (n=104, p=6)"=1, "Example data with CID numbers (n=133, p=6)"=2), selected=1)

            ),

        conditionalPanel(condition="input.dataInput=='2'",
            HTML('<br>'),
            h5("Upload a delimited text file: "),
            fileInput("upload", "", multiple = FALSE),
            radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3,"Space"=4),selected=2),
            HTML('<br>'),
            HTML('<p>You can upload your data as separated by comma, tab, semicolon or space.</p>'),
            HTML('<p>First row must be header.</p>'),
            HTML('<br>')
        ),

        conditionalPanel(condition="input.dataInput=='3'",
            HTML('<br>'),
            h5("Paste or enter your data below:"),
            tags$textarea(id="myData", rows=10, cols=5, ""),
            actionButton('clearText_button','Clear data'),
            HTML('<br>'),
            HTML('<br>'),

            radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3), selected=2)
            ),


        conditionalPanel(condition="input.dataInput=='4'",
            HTML('<br>'),

#numericInput(inputId = "acNum", label = "PubChem CID number", value = NULL),

            h5("Enter single molecule values below:"),

            textInput(inputId = "acNum", label = "PubChem CID number (if any)", value="Enter CID number"),
            textInput(inputId = "logP", label = "Partition coefficient (logP)", value="Enter logP"),
            textInput(inputId = "PSA", label = "Polar surface area", value="Enter PSA"),
            textInput(inputId = "donorCount", label = "Donor count", value="Enter donor count"),
            textInput(inputId = "aliphaticringCount", label = "Aliphatic ring count", value="Enter aliphatic ring count"),
            textInput(inputId = "aromaticringCount", label = "Aromatic ring count", value="Enter aromatic ring count"),
            textInput(inputId = "balabanindex", label = "Balaban index", value="Enter balaban index"),
            HTML('<br>'),
            checkboxInput("showEx", "Load example", FALSE),
            HTML('<br>'),
            helpText("Note: Use . as delimiter")
        ),

            HTML('<p>NOTE 1: If Data has PubChem CID numbers, click <b>"Data has PubChem CID numbers"</b> checkbox above.</p>'),
            HTML('<p>NOTE 2: CID numbers must be placed in first column of data matrix.</p>')

    ),


        conditionalPanel(condition="input.tabs1=='Analyze'",
            h4("Choose algorithm(s)"),
            HTML('<br>'),
            column(5, checkboxInput("selectAll", "Select All", FALSE)),
            column(5, checkboxInput("deselectAll", "Deselect All", FALSE)),
            HTML('<br>'),
            HTML('<br>'),

            h5("Discriminant Algorithm"),
            checkboxGroupInput('disMeth', "",
            c("Flexible Discriminant Analysis (FDA)" = "FDA"), selected = "FDA"
            ),

            h5("Tree Based Algorithms"),
            checkboxGroupInput('treeMeth', "",
            c("C5.0" = "C5", "J48" = "J48"), selected = "C5"
            ),

            h5("Kernel Based Algorithms"),
            checkboxGroupInput('kernelMeth', "",
            c("Least Squares Support Vector Machines with Radial Basis Function (lsSVMrbf)" = "lsSVMrbf", "Support Vector Machines with Radial Basis Function (SVMrbf)" = "SVMrbf", "Support Vector Machines with Linear Kernel Function (SVMlin)" = "SVMlin"), selected = "lsSVMrbf"
            ),

            h5("Ensemble Algorithms"),
            checkboxGroupInput('ensembleMeth', "",
            c("Random Forests (RF)" = "RF", "Bagged Support Vector Machines with Radial Basis Function (bagSVM)" = "bagSVM"),
            selected = "RF"
            ),

            h5("Other Algorithms"),
            checkboxGroupInput('otherMeth', "",
            c("k-Nearest Neighbors (kNN)" = "kNN", "Neural Networks (NN)" = "NN"),
            selected = "kNN"
            )
        ),

        conditionalPanel(condition="input.tabs1=='Manual'",
            HTML('<p align="center"><img src="manual.png" width=200 height=200></p>')
            ),

        conditionalPanel(condition="input.tabs1=='News'",
            HTML('<p align="center"><img src="news.png" width=200 height=200></p>')
        ),

        conditionalPanel(condition="input.tabs1=='Authors'",
            HTML('<p align="center"> <a href="https://www.hacettepe.edu.tr/english/" target="_blank"><img src="hu_logo.JPEG" width=150 height=150></a> </p>')
        ),

        conditionalPanel(condition="input.tabs1=='Citation'",
            HTML('<p align="center"><img src="cite.png" width=200 height=200></p>')
        ),

        conditionalPanel(condition="input.tabs1=='Plots'",

            radioButtons("graphInput", "", list("Heat map (*)"=1, "Dendrogram"=2), selected=NULL),


            conditionalPanel(condition="input.graphInput=='1'",

            HTML('<br>'),
            actionButton("aHeatmap", "Create Heat map"),
            HTML('<br>'),
            HTML('<br>'),
            helpText("(*) Data must have PubChem CID numbers"),
            HTML('<br>'),
            checkboxInput(inputId = "heatmapOpt", label = "Heat map options", value = FALSE),
            HTML('<br>'),

            conditionalPanel(condition = "input.heatmapOpt",
            fluidRow(column(5,sliderInput("myheightHeat", "Plot height (for download):", value=500, min=200, max=1200)),
            column(2),
            column(5,sliderInput("mywidthHeat", "Plot width (for download):", value=900, min=200, max=1200 ))),
            HTML('<br>'),


            fluidRow(
                column(5, selectizeInput("heatmapScale", "Scale and center", choices = c("none","row", "column"), multiple = FALSE, selected = "none")),
                column(1),
                column(5, selectizeInput("heatmapDend", "Dendrogram style", choices = c("both","row","column","none"), multiple = FALSE, selected = "both"))
            ),

            HTML('<br>'),

            fluidRow(
                column(5, selectInput("heatmapDens", "Color key", choices = c("none","histogram","density"), multiple = FALSE)),
                column(1),
                column(5, selectInput("heatmapCol", "Color type", choices = c("redblue","bluered","greenred","redgreen"), multiple = FALSE, selected = "greenred")
                )),

            HTML('<br>'),

            fluidRow(
                column(5, numericInput(inputId = "heatmapColdens", label = "Color density", value = 400, min = 2, max = 1000000, step = 1)

                ),
                column(1),
                column(5, selectizeInput("heatmapTrace", "Trace line", choices = c("column","row","both","none"), multiple = FALSE, selected = "none"))
            ),

            HTML('<br>'),


            fluidRow(
                column(5, numericInput(inputId = "heatmapCexRow", label = "Cex for row", value = 0.4, min = 0.1, max = 100, step = 0.1)

                ),
                column(1),
                column(5, numericInput(inputId = "heatmapCexCol", label = "Cex for column", value = 0.4, min = 0.1, max = 100, step = 0.1))
            ),


            HTML('<br>'),


            fluidRow(
                column(5, textInput("heatmapMain", "Plot title", "Heat map")),

                column(1),

                column(5, selectizeInput("heatmapColor", "Backround color", choices = c("white"="#ffffff", "grey"="#808080", "dim grey"="#696969", "dark grey"="#a9a9a9", "silver"="#C0C0C0", "light grey"="#D3D3D3", "gainsboro"="#DCDCDC", "white smoke"="#F5F5F5", "antique white"="#FAEBD7", "beige"="#F5F5DC", "bisque"="#FFE4C4", "blanched almond"="#FFEBCD", "wheat"="#F5DEB3", "corn silk"="#FFF8DC", "lemon chiffon"="#FFFACD", "light golden rod yellow"="#FAFAD2", "light yellow"="#FFFFE0", "moccasin"="#FFE4B5", "navajo white"="#FFDEAD", "ivory"="#FFFFF0", "azure"="#F0FFFF",      "snow"="#FFFAFA", "light cyan"="#E0FFFF"), selected = "#ffffff", multiple = FALSE))
            )

            )),


            HTML('<br>'),

            conditionalPanel(condition="input.graphInput=='2'",

                radioButtons("dendInput", "", list("From data (*)"=1, "From SDF file"=2), selected=1),

                HTML('<br>'),

            conditionalPanel(condition="input.dendInput=='1'",

                actionButton("aDendogram", "Create Dendrogram"),

                HTML('<br>'),
                HTML('<br>'),
                helpText("(*) Data must have PubChem CID numbers"),
                HTML('<br>')

            ),


            conditionalPanel(condition="input.dendInput=='2'",

                h5("Upload an SDF file: "),
                fileInput("uploadSDF", "", multiple = FALSE),
                actionButton("aDendogramSDF", "Create Dendrogram"),
                 HTML('<br>'),
                 HTML('<br>'),
                 HTML('<br>')

            ),


            checkboxInput(inputId = "dendogramOpt", label = "Dendrogram options", value = FALSE),
            HTML('<br>'),
            conditionalPanel(condition = "input.dendogramOpt",


            fluidRow(column(5,sliderInput("myheightDend", "Plot height (for download):", value=500, min=200, max=1200)),
                column(2),
                column(5,sliderInput("mywidthDend", "Plot width (for download):", value=900, min=200, max=1200 ))),
                HTML('<br>'),


            fluidRow(
                column(6, selectizeInput("dendogramMethod", "Dendrogram method", choices = c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"), multiple = FALSE)),

                column(1),

                column(5, selectizeInput("dendogramMetric", "Similarity metric", choices = c("tanimoto", "euclidean", "cosine", "dice", "hamming"), selected = "tanimoto", multiple = FALSE))
            ),


            HTML('<br>'),

            fluidRow(
                column(6, selectizeInput("dendogramType", "Dendrogram type", choices = c("phylogram", "fan", "cladogram", "unrooted", "radial"), selected = "fan", multiple = FALSE)),

                column(1),

                column(5, selectizeInput("dendogramFin", "Fingerprint type", choices = c("compact", "complete"), selected = "compact", multiple = FALSE))
            ),

            HTML('<br>'),

            fluidRow(
                column(6, numericInput(inputId = "dendogramlabelOffset", label = "Label offset", value = 0.01, min = -0.1, max = 0.1, step = 0.01)),

                column(1),

                column(5, numericInput(inputId = "dendogramCex", label = "Size of labels", value = 0.5, min = 0,  step = 0.1))
            ),

            HTML('<br>'),


            fluidRow(
                column(6, numericInput(inputId = "k", label = "Number of clusters", value = 2)),

                column(1),

                column(5, selectizeInput("dendogramDirect", "Tree direction", choices = c("rightwards", "leftwards", "upwards",  "downwards"), selected = "rightwards", multiple = FALSE))
            ),



            HTML('<br>'),

            fluidRow(
                column(6, textInput(inputId = "dendogramMain", label = "Plot Title", value = "Dendrogram")),

                column(1),

                column(5, selectizeInput("dendogramColor", "Backround color", choices = c("white"="#FFFFFF", "grey"="#808080", "dim grey"="#696969", "dark grey"="#A9A9A9", "silver"="#C0C0C0", "light grey"="#D3D3D3", "gainsboro"="#DCDCDC", "white smoke"="#F5F5F5", "antique white"="#FAEBD7", "beige"="#F5F5DC", "bisque"="#FFE4C4", "blanched almond"="#FFEBCD", "wheat"="#F5DEB3", "corn silk"="#FFF8DC", "lemon chiffon"="#FFFACD", "light golden rod yellow"="#FAFAD2", "light yellow"="#FFFFE0", "moccasin"="#FFE4B5", "navajo white"="#FFDEAD", "ivory"="#FFFFF0", "azure"="#F0FFFF", "snow"="#FFFAFA", "light cyan"="#E0FFFF"), selected = "#FFFFFF", multiple = FALSE))
            )



            ))),

        conditionalPanel(condition="input.tabs1=='PubChem'",

            selectizeInput("pubChemID2", "Select CID number of molecule(s) (*)", choices = NULL, multiple = TRUE),
            sidebarPanel(
            # Action button

            div(class = "span12", actionButton("aButton2", "Submit")),
            HTML('<br>'),
            HTML('<br>'),

            div(class = "span12", actionButton("clearPubchem2","Clear"))
        ),
            HTML('<br>'),
            HTML('<br>'),
            HTML('<br>'),
            HTML('<br>'),
            HTML('<br>'),
            helpText("(*) 16 molecules can be selected at a time for plotting molecules" ),
            HTML('<br>'),
            fluidRow(column(5,sliderInput("myheightPlot", "Plot height (for download):", value=500, min=200, max=1200)),
            column(2),
            column(5,sliderInput("mywidthPlot", "Plot width (for download):", value=900, min=200, max=1200 ))),

            HTML('<br>'),
            HTML('<br>'),
            downloadButton("downloadSDF", "Download SDF-file (**)"),

            HTML('<br>'),
            HTML('<br>'),

            helpText("(**) Any number of molecules can be selected to download SDF-fie" )

    )



),


mainPanel(
    tabsetPanel(
        tabPanel("Introduction",

            HTML('<p align = "justify"> Virtual screening is an important step in early-phase of drug discovery process. Since there are thousands of compounds, this step should be both fast and effective in order to distinguish drug-like and nondrug-like molecules. Statistical machine learning methods are widely used in drug discovery studies for classification purpose. Here, we developed a new tool, which can classify molecules as drug-like and nondrug-like based on various machine learning methods, including discriminant, tree-based, kernel-based, ensemble and other algorithms. To construct this tool, first, performances of twenty-three different machine learning algorithms are compared by ten different measures, then, ten best performing algorithms have been selected based on principal component and hierarchical cluster analysis results. Besides classification, this application has also ability to create heat map and dendrogram for visual inspection of the molecules through hierarchical cluster analysis. Moreover, users can connect the PubChem database to download molecular information and to create two-dimensional structures of compounds. More detailed information about this tool can be found in the <a href="http://www.plosone.org/article/fetchObject.action?uri=info:doi/10.1371/journal.pone.0124600&representation=PDF" target="_blank"> main paper</a>.</p>'),

            HTML('<br>'),

            HTML('<p><div align="center"><table cellpadding="0" cellspacing="0"><tr><td><img src="heatmap.png" width="1300" height="1300" border="0"></td><td><img src="plot.png" width="1300" height="1300" border="0"></td><td><img src="dendogram.png" width="700" height="700" border="0"></td></tr></table></div></p>'),

            HTML('<br>'),

            h6("If you use this tool for your research please cite: Korkmaz S, Zararsiz G, Goksuluk D (2015) MLViS: A Web Tool for Machine Learning-Based Virtual Screening in Early-Phase of Drug Discovery and Development. PLoS ONE 10(4): e0124600.")
        ),

            tabPanel("Data upload",
                navbarPage(
                title = '',
                tabPanel('Data',                dataTableOutput('RawData'))
            )
        ),

            tabPanel("Analyze",
                downloadButton("downloadMLViSResult", "Download results set as txt-file"),


                navbarPage(
                title = '',
                tabPanel('Statistical Machine Learning Predictions',                dataTableOutput('MLVS'))
            ),

                HTML('<br>'),
                #verbatimTextOutput("console"),
                textOutput("footnote")
        ),


            tabPanel("Plots",

                HTML('<div align="center">'),
                fluidRow(
                    column(5, downloadButton("downloadHeatmap", "Download Heat map")),
                    column(1),
                    column(5, downloadButton("downloadDendogram", "Download Dendrogram"))
                ),

                tagList(
                tags$head(
                tags$link(rel="stylesheet", type="text/css",href="style.css"),
                tags$script(type="text/javascript", src = "busy.js")
            )
            ),

                div(class = "busy",
                p("Calculation in progress... this may take a while...."),
                img(src="loading.gif")
                ),
                HTML('</div>'),
                HTML('<br>'),

                HTML('<div align="center">'),
                fluidRow(
                column(5, plotOutput("heatmap")),
                column(1),
                column(5, plotOutput("dendogram"))
            ),
                HTML('</div>')

            ),

            tabPanel("PubChem",
                #downloadButton("downloadSDF", "Download SDF-file", class = "busy"),
                downloadButton("downloadPlot", "Download plot as pdf-file", class = "busy"),


                tagList(
                tags$head(
                tags$link(rel="stylesheet", type="text/css",href="style.css"),
                tags$script(type="text/javascript", src = "busy.js")
                )
            ),

            div(class = "busy",
            p("Calculation in progress, please wait."),
            img(src="loading.gif")
            ),
            #div(class = "span70", plotOutput("molPlot"))
            plotOutput("molPlot")

        ),


            tabPanel("Manual",

                h5("Usage of the web-tool"),
                HTML('<p>In order to use this application,</p>'),
                HTML('<p>   (i) load your data set using <b>Data upload</b> tab. Here, users have three options: <b>"Data upload"</b>, <b>"Paste your data"</b> and <b>"Single molecule"</b></p>'),
                HTML('<p>   (ii) choose statistical machine learning algorithm(s) in the <b>Analyze</b> tab.</p>'),
                HTML('<p>   (iii) in the <b>Plots</b> tab, users can create dendrogram using <a href="http://www.bioconductor.org/packages/release/bioc/html/Rcpi.html" target="_blank"><b>Rcpi</b> </a>package and heat map using <a href="http://www.bioconductor.org/packages/release/bioc/html/ChemmineR.html" target="_blank"><b>ChemmineR</b> </a> and <a href="http://cran.r-project.org/web/packages/gplots/index.html" target="_blank"><b>gplots</b> </a>packages based on PubChem’s fingerprints . To create dendrogram and heat map from data, it must have PubChem CID numbers. Alternatively, to create a dendrogram, users can upload an SDF file, which contains molecular informations about compounds. Please note that creating dendrogram and heat map may take for a while due to the large number of compounds</p>'),
                HTML('<p>   (iv) create molecule plot(s) in the <b>PubChem</b> tab. Data must have PubChem CID numbers and 16 molecules can be selected at a time. If users want to download SDF file without plotting, then they can select any number of molecules.</p>'),

                HTML('<p> Users can download statistical machine-learning predictions as txt in the <b>Analyze</b> tab, heat map and dendrogram plots as pdf in the <b>Plots</b> tab, molecule plot and molecule SDF file in the <b>PubChem</b> tab.</p>'),
                HTML('<p> <b>Please note that data set must have following descriptors in precise order: logP, polar surface area (PSA), donor count (DC), aliphatic ring count (AlRC), aromatic ring count (ArRC) and Balaban index (BI).</b> </p>'),
                HTML('<p> <b> If Data has PubChem CID numbers, this must be placed in the first column of the data matrix.</b> </p>')
        ),

            tabPanel("Authors",
                h4("Authors"),

                HTML('<p><a href="http://yunus.hacettepe.edu.tr/~selcuk.korkmaz/" target="_blank"> <b>Selcuk Korkmaz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:selcuk.korkmaz@hacettepe.edu.tr" target="_blank">selcuk.korkmaz@hacettepe.edu.tr</a><p>'),

                HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Gokmen_Zararsiz_CV_Eng.pdf" target="_blank"> <b>Gokmen Zararsiz</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:gokmen.zararsiz@hacettepe.edu.tr" target="_blank">gokmen.zararsiz@hacettepe.edu.tr</a><p>'),

                HTML('<p><a href="http://www.biostatistics.hacettepe.edu.tr/cv/Dincer_Goksuluk_CV_Eng.pdf" target="_blank"> <b>Dincer Goksuluk</b></a><p>'),
                HTML('<p>Hacettepe University Faculty of Medicine <a href="http://www.biostatistics.hacettepe.edu.tr" target="_blank"> Department of Biostatistics</a><p>'),
                HTML('<p><a href="mailto:dincer.goksuluk@hacettepe.edu.tr" target="_blank">dincer.goksuluk@hacettepe.edu.tr</a><p>'),


                HTML('<br>'),
                h6("Please feel free to send us bugs and feature requests.")
        ),

            tabPanel("News",

                h5("April 30, 2015"),
                HTML('<p>(1) <a href="http://www.plosone.org/article/fetchObject.action?uri=info:doi/10.1371/journal.pone.0124600&representation=PDF" target="_blank">MLViS paper </a> published at PLoS ONE. The complete reference information is at the <b>Citation</b> tab <p>'),
                HTML('<br>'),



                h5("Version 1.1 (October 30, 2014)"),
                HTML('<p>(i) 4 new statistical machine-learning methods have been added <p>'),
                HTML('<p>(ii) Plots and PubChem tabs have been added <p>'),
                HTML('<br>'),
                h5("Version 1.0 (October 8, 2014)"),
                HTML('<p>MLViS web-tool has been released. <p>'),

                HTML('<br>'),

                h5("Other Tools"),

                HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/easyROC/" target="_blank"> <b>easyROC: a web-tool for ROC curve analysis</b></a><p>'),
                HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/MVN/" target="_blank"> <b>MVN: a web-tool for assessing multivariate normality </b></a><p>'),
                HTML('<p><a href="http://www.biosoft.hacettepe.edu.tr/DDNAA/" target="_blank"> <b>DDNAA: Decision support system for differential diagnosis of nontraumatic acute abdomen </b></a><p>')


                ),


                tabPanel("Citation",

                    h5("If you use this tool for your research please cite:"),
                    HTML('<p>Korkmaz S, Zararsiz G, Goksuluk D (2015) MLViS: A Web Tool for Machine Learning-Based Virtual Screening in Early-Phase of Drug Discovery and Development. PLoS ONE 10(4): e0124600. doi:<a href="http://www.plosone.org/article/fetchObject.action?uri=info:doi/10.1371/journal.pone.0124600&representation=PDF" target="_blank"> 10.1371/journal.pone.0124600 </a><p>')
                ),

            id="tabs1"
        ),

            tags$head(tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
            tags$style(type="text/css", "select { max-width: 200px; }"),
            tags$style(type="text/css", "textarea { max-width: 185px; }"),
            tags$style(type="text/css", ".jslider { max-width: 200px; }"),
            tags$style(type='text/css', ".well { max-width: 330px; }"),
            tags$style(type='text/css', ".span4 { max-width: 330px; }")),

            tags$head(tags$link(rel = "shortcut icon", href = "favicon2.ico"))







        )
))




