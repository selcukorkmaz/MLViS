shinyServer(function(input, output, session){
	library(caret)
    library(Rcpi)
    library(ape)
    library(plyr)
    library(RWeka)
    library(rJava)
    library(ChemmineR)
    library(gplots)
	load("model.rda")
    load("bagSVM.rda")
    load("j48.rda")
	load("Parameters.rda")  ## stores PreProcessing (z-score) parameters
	source("prediction.R")  ## Function for prediction of new sample(s)
    source("bagPrediction.R")
	
	# Return the requested logP count
	logP <- reactive({
		as.numeric(input$logP)
	})
	
	# Return the requested PSA level
	PSA <- reactive({
		as.numeric(input$PSA)
	})
    
    # Return the requested donorCount level
	donorCount <- reactive({
		as.numeric(input$donorCount)
	})
    
    # Return the requested aliphaticringCount level
	aliphaticringCount <- reactive({
		as.numeric(input$aliphaticringCount)
	})
    
    # Return the requested aromaticringCount level
	aromaticringCount <- reactive({
		as.numeric(input$aromaticringCount)
	})
    
    # Return the requested balabanindex level
	balabanindex <- reactive({
		as.numeric(input$balabanindex)
	})

    selectedMethods <- reactive({
        c("bagSVM", "C5", "RF", "SVMrbf", "SVMlin", "lsSVMrbf", "kNN", "NN", "FDA", "J48")
    })
    
    disBasedMethods <- reactive({
        c("FDA")
    })
    
    treeBasedMethods <- reactive({
        c("C5", "J48")
    })
    
    kernelBasedMethods <- reactive({
        c("lsSVMrbf", "SVMrbf", "SVMlin")
    })
    
    ensembleMethods <- reactive({
        c("RF", "bagSVM")
    })
    
    otherMethods <- reactive({
        c("kNN", "NN", "FDA")
    })
    
    observe({
		if (input$clearText_button == 0) {return()}
		isolate({ updateTextInput(session, "myData", label = ",", value = "") })
	})
    
	################
    ## Data input ##
    ###############
    
	dataM <- reactive({
        if(input$dataInput==1){  ## Load example data.
            if(input$sampleData==1){
                data <- read.table("unbalancedData.txt", header=TRUE)
            }
            
           else if(input$sampleData==2){
                data <- read.table("testset2.txt", header=TRUE)
            }
        }
        
		else if(input$dataInput==2){  ## Upload data.
            inFile <- input$upload
            mySep <- switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="")
            
			if (is.null(input$upload))  {return(NULL)}
            
            if (file.info(inFile$datapath)$size <= 10485800){
                data <- read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE)
            }
            
            else print("File is bigger than 10MB and will not be uploaded.")
		}
		
		else if (input$dataInput==3){  ## Paste data.
			if(is.null(input$myData)) {return(NULL)}
			
			tmp <- matrix(strsplit(input$myData, "\n")[[1]])
			mySep <- switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
			myColnames <- strsplit(tmp[1], mySep)[[1]]
			data <- matrix(0, length(tmp)-1, length(myColnames))
			colnames(data) <- myColnames
            
			for(i in 2:length(tmp)){
				myRow <- as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
				data[i-1,] <- myRow[-length(myRow)]
			}
			
            data <- data.frame(data)
		}
        
        else {  ## single molecule için dataTable çıktısı vermeye gerek yoksa bu kısmı çıkartabiliriz.
            data = data.frame(data_InputSingle())
        }
        
        return(data)
	})
    
	observe({
		if (input$showEx){  ## update text input via show example check
            updateTextInput(session, inputId = "acNum",                 value = "1978")
            updateTextInput(session, inputId = "logP",                  value = "1.68")
            updateTextInput(session, inputId = "PSA",                   value = "63.6")
            updateTextInput(session, inputId = "donorCount",            value = "3")
            updateTextInput(session, inputId = "aliphaticringCount",    value = "0")
            updateTextInput(session, inputId = "aromaticringCount",     value = "1")
            updateTextInput(session, inputId = "balabanindex",          value = "2.42")
        }
    })
    
    
    observe({
    
        if(input$dataInput!=4){
            if(input$molName){
            updateSelectInput(session, "pubChemID", choices = as.numeric(dataM()[,1]), selected = as.numeric(dataM()[,1]))
            }
        }
            
    })
    
    observe({
        
        if(input$dataInput!=4){
            if(input$molName){
                updateSelectInput(session, "pubChemID2", choices = as.numeric(dataM()[,1]), selected = as.numeric(dataM()[,1])[1])
            }
        }
        
    })
    
    observe({
        
        if(input$dataInput==4){
            if(!input$molName){
                updateNumericInput(session, inputId = "acNum", value = NULL)
                updateSelectInput(session, "pubChemID", choices = input$acNum, selected = input$acNum)
            }
        }
        
    })
    
    observe({
        if (input$aHeatmap != 0)
        {return()}
        isolate({ updateRadioButtons(session, "input.graphInput", label = "", choices = NULL)})
    })
    
    
    observe({
		if (input$clearPubchem2 == 0) {return()}
        
		isolate({ updateTextInput(session, "pubChemID2", label = "Select CID number of molecule(s) (*)", value = "") })
	})
    
    observe({
        if (input$aButton2 != 0)
        {return()}
        isolate({ updateSelectInput(session, "pubChemID2", label = "Select CID number of molecule(s) (*)")})
    })

	
    c("bagSVM", "C5", "RF", "SVMrbf", "SVMlin", "lsSVMrbf", "kNN", "NN", "FDA", "J48")[c("bagSVM", "C5", "RF", "SVMrbf", "SVMlin", "lsSVMrbf", "kNN", "NN", "FDA", "J48") %in% c("bagSVM")]
    
    observe({
        if(input$selectAll){
            updateCheckboxGroupInput(session, inputId = "disMeth",      selected = disBasedMethods())
            updateCheckboxGroupInput(session, inputId = "treeMeth",     selected = treeBasedMethods())
            updateCheckboxGroupInput(session, inputId = "kernelMeth",   selected = kernelBasedMethods())
            updateCheckboxGroupInput(session, inputId = "ensembleMeth", selected = ensembleMethods())
            updateCheckboxGroupInput(session, inputId = "otherMeth",    selected = otherMethods())
            updateCheckboxInput(session, "deselectAll", label = "Deselect All", FALSE)
            updateCheckboxInput(session, "selectAll",   label = "Select All",   TRUE)
        }
    })
    
    observe({
        if(input$deselectAll){
            updateCheckboxGroupInput(session, inputId = "disMeth",      selected = "")
            updateCheckboxGroupInput(session, inputId = "treeMeth",     selected = "")
            updateCheckboxGroupInput(session, inputId = "kernelMeth",   selected = "")
            updateCheckboxGroupInput(session, inputId = "ensembleMeth", selected = "")
            updateCheckboxGroupInput(session, inputId = "otherMeth",    selected = "")
            updateCheckboxInput(session, "selectAll",   label = "Select All",   FALSE)
            updateCheckboxInput(session, "deselectAll", label = "Deselect All", TRUE)
        }
    })
    
    observe({
        if (sum(length(input$disMeth), length(input$treeMeth), length(input$kernelMeth), length(input$ensembleMeth), length(input$otherMeth)) != length(selectedMethods())){
            updateCheckboxInput(session, "selectAll", label = "Select All", FALSE)
        }
    })
    
    observe({
        if (sum(length(input$disMeth), length(input$treeMeth), length(input$kernelMeth), length(input$ensembleMeth), length(input$otherMeth)) != 0){
            updateCheckboxInput(session, "deselectAll", label = "Deselect All", FALSE)
        }
    })

	observe({
		if (any(input$acNum != "1978", input$logP != "1.68", input$PSA != "63.6", input$donorCount != "3",
                input$aliphaticringCount != "0", input$aromaticringCount != "1",
                input$balabanindex != "2.42")){
                    
			updateCheckboxInput(session, "showEx", value = FALSE)
            
		}
	})
	
    heightsizePlot <- reactive(input$myheightPlot)
	widthsizePlot <- reactive(input$mywidthPlot)
    
    heightsizeHeat <- reactive(input$myheightHeat)
	widthsizeHeat <- reactive(input$mywidthHeat)
    
    heightsizeDend <- reactive(input$myheightDend)
	widthsizeDend <- reactive(input$mywidthDend)
    
    # New data is saved as a row vector (1xp). dataM() içerisinde kullanılıyor. Yüklenen verinin data upload kısmında tablo olarak gösterilmesi için.
    data_InputSingle <- reactive({
        raw = matrix(c(logP(), PSA(), donorCount(), aliphaticringCount(), aromaticringCount(), balabanindex()), ncol=6)
        raw = data.frame(raw)
        colnames(raw) = c("logP", "PSA", "donorCount", "aliphaticringCount", "aromaticringCount", "balabanindex")
        return(raw)
    })
    
	# New data (z-score transformed) is saved as a row vector (1xp)
	data_Input <- reactive({
        raw = matrix(c(logP(), PSA(), donorCount(), aliphaticringCount(), aromaticringCount(), balabanindex()), ncol=6)
        zscore = matrix((raw - param$Mean) / param$Std.Dev., nrow=1)
        zscore = data.frame(zscore)
        return(zscore)
    })
    
    predTable <- reactive({  ## Gözlemlerin sınıflandırılması işlemi. Sonuç tablosu.
        if(input$dataInput == 4){  ## single molecule giriliyor ise;
            if (all(logP() != "Enter logP", PSA() != "Enter PSA", donorCount() != "Enter donorCount", aliphaticringCount() != "Enter aliphaticringCount", aromaticringCount() != "Enter aromaticringCount", balabanindex() != "Enter balabanindex")){
                
                test = data_Input()
                colnames(test) = c("logP", "PSA", "donorCount", "aliphaticringCount", "aromaticringCount", "balabanindex")
            
                pred = lapply(model, FUN=function(x){
                                         prediction(x, newdat=test)})

                pred2 = data.frame(matrix(t(ldply(pred, rbind)[,-1]), nrow=1))
                colnames(pred2) = names(pred)
                
                .jcache(J48$classifier)
                j48 = predict(J48, newdat = as.data.frame(test), type = "class")
                j48pred = c("Nondrug-like", "Drug-like")[as.numeric(j48)]
                #j48pred = data.frame(j48pred)
                
            
                #bagsKNN = bag.prediction(bagKNN.models, test)
                #bagsKNN.pred = c("Nondrug-like","Drug")[as.numeric(apply(bagsKNN, 1,function(x)names(which.max(table(x)))))]
                
                bagsSVM = bag.prediction(bagSVM.models, test)
                bagsSVM.pred = c("Nondrug-like","Drug-like")[as.numeric(apply(bagsSVM, 1,function(x)names(which.max(table(x)))))]
                
                id = 1
                
                pred2 = data.frame(ID = id, bagSVM = bagsSVM.pred, pred2, j48pred)
                colnames(pred2) = c("ID", selectedMethods())
            }
        }
        
        if (input$dataInput != 4){ # upload / load ve paste için
            
            if(!input$molName){ ## Veri seti molekül isimlerini içermiyor ise;
                testRAW = dataM()
                id = 1:nrow(dataM())
            }
            
            if(input$molName){ ## Veri seti molekül isimlerini içeriyor ise;
                testRAW = dataM()[,-1]
                id = dataM()[,1]
            }
            
            testZ = scale(testRAW, center=param$Mean, scale=param$Std.Dev)
            test = testZ
            
            colnames(test) = c("logP", "PSA", "donorCount", "aliphaticringCount", "aromaticringCount", "balabanindex")
            
            pred = lapply(model, FUN=function(x){
                                     prediction(x, newdat=test)})
            
            pred2 = data.frame(t(ldply(pred, rbind)[,-1]))
            colnames(pred2) = names(pred)
            
            .jcache(J48$classifier)
            j48 = predict(J48, newdat = as.data.frame(test), type = "class")
            j48pred = c("Nondrug-like", "Drug-like")[as.numeric(j48)]
            #j48pred = data.frame(j48pred)
        
            
            #bagsKNN = bag.prediction(bagKNN.models, test)
            #bagsKNN.pred = c("Nondrug-like","Drug")[as.numeric(apply(bagsKNN, 1,function(x)names(which.max(table(x)))))]
            
            bagsSVM = bag.prediction(bagSVM.models, test)
            bagsSVM.pred = c("Nondrug-like","Drug-like")[as.numeric(apply(bagsSVM, 1,function(x)names(which.max(table(x)))))]
            
            pred2 = data.frame(ID = id, bagSVM = bagsSVM.pred, pred2, j48pred)
            colnames(pred2) = c("ID", selectedMethods())
        }

    return(pred2)
    
    })
    
    predRes <- reactive({
        predTable()[,c("ID", input$disMeth, input$treeMeth, input$kernelMeth, input$ensembleMeth, input$otherMeth)]
    })

    output$RawData <- renderDataTable(dataM(), options = list(iDisplayLength = 10))  ## returns input data
    output$MLVS <- renderDataTable(predRes(), options = list(iDisplayLength = 50))

    output$footnote <- renderText({
		"Statistical machine-learning algorithms classify compunds as drug-like and nondrug-like based on their molecular descriptors."
    })
	
    output$downloadMLViSResult <- downloadHandler(
        filename = function() { paste("Results.txt") },
    
        content = function(file) {
            write.table(predRes(), file, row.names=FALSE, quote=FALSE, sep="\t")
            
    })
        
        ############################################
        ## Molecule plot function for PubChem tab ##
        ############################################
        molPlotInput = function() {
            
            if(length(input$pubChemID2) > 16) {stop('Only 16 molecules can be selected at a time. Please, check the number of molecules')}
            if(input$sampleData == 1 & input$molName & input$dataInput != 2 & input$dataInput != 3) {stop('No PubChem CID number, please uncheck "Data has PubChem CID numbers"')}
            if(input$sampleData == 2 & !input$molName) {stop('please check "Data has PubChem CID numbers"')}
            if(input$sampleData == 1) {stop('No PubChem CID number')}
            
            #if (input$clearText_button == 0) return()
            
            g = getIds(c(as.numeric(input$pubChemID2)))
            g@ID = as.character(input$pubChemID2)
            p = plot(g)
         
        }
        
        ####################################
        ## Heatmap function for Plots tab ##
        ####################################
    
        heatmapInput = function() {
    
            if(input$dataInput == 4) {stop('No results for single molecule')}
            if(input$sampleData == 1 & input$molName & input$dataInput != 2 & input$dataInput != 3) {stop('No PubChem CID number, please uncheck "Data has PubChem CID numbers"')}
            if(input$sampleData == 2 & !input$molName) {stop('please check "Data has PubChem CID numbers"')}
            if(input$sampleData == 1) {stop('No PubChem CID number')}
    
            if (input$dataInput != 4){ # upload / load ve paste için
        
            if(!input$molName){ ## Veri seti molekül isimlerini içermiyor ise;
            testRAW = dataM()
            }
        
            if(input$molName){ ## Veri seti molekül isimlerini içeriyor ise;
            testRAW = dataM()[,-1]
            }}
            
            #op = par(bg=input$heatmapColor)
            #cc <- rainbow(ncol(as.matrix(testRAW)), start = input$heatmapStartC, end = input$heatmapEndC)
            #rc <- rainbow(nrow(as.matrix(testRAW)), start = input$heatmapStartR, end = input$heatmapEndR)
            #heatmap.2(as.matrix(testRAW), Rowv=TRUE, Colv=TRUE,
            #dendrogram=input$heatmapDend, col=eval(parse(text = paste(input$heatmapCol,"(",input$heatmapColdens,")", sep=""))),
            #key=T, keysize=1.5, density.info=input$heatmapDens, labRow=NA, scale=input$heatmapScale , ColSideColors = cc,  RowSideColors = rc, cexCol = input$heatmapCexCol, trace = input$heatmapTrace, main = input$heatmapMain)
            
            g = getIds(c(as.numeric(dataM()[,1])))
            tmp = tempdir()
            file = paste(tmp, "/", "distmat.rda", sep="")
            apset <- sdf2ap(g)
            fpset <- desc2fp(apset)
            dummy <- cmp.cluster(db=apset, cutoff=0, save.distances=file, quiet=TRUE)
            load(file)
            
            simMA <- sapply(cid(fpset), function(x) fpSim(fpset[x], fpset, sorted=FALSE))
            hc <- hclust(as.dist(1-simMA), method="single")
            
            op = par(bg=input$heatmapColor)
            heatmap.2(1-distmat, Rowv=as.dendrogram(hc), Colv=as.dendrogram(hc),
            col=eval(parse(text = paste(input$heatmapCol,"(",input$heatmapColdens,")", sep=""))),
            density.info=input$heatmapDens, scale=input$heatmapScale, cexCol = input$heatmapCexCol, cexRow = input$heatmapCexRow, dendrogram=input$heatmapDend, trace = input$heatmapTrace, main = input$heatmapMain)
    
        }
    
    
        #################################################
        ## Dendogram function for Plots tab (from data)##
        #################################################
    
        dendrogramInput1 = function() {
        
            if(input$dataInput == 4) {stop('No results for single molecule')}
            if(input$sampleData == 1 & input$molName & input$dataInput != 2 & input$dataInput != 3) {stop('No PubChem CID number, please uncheck "Data has PubChem CID numbers"')}
            if(input$sampleData == 2 & !input$molName) {stop('please check "Data has PubChem CID numbers"')}
            if (input$dataInput != 4){ # upload / load ve paste için
            if(!input$molName) {stop('No PubChem CID number')}
            if(input$molName){ ## Veri seti molekül isimlerini içeriyor ise;
                testRAW = dataM()[,-1]
            }
            }
            
            Sys.sleep(5)
            g = getIds(c(as.numeric(dataM()[,1])))
            tmp = tempdir()
            file = paste(tmp, "/", "SDF_file.sdf", sep="")
            write.SDF(g, file, sig=FALSE)
            mols=readMolFromSDF(file)
        
            simmat = diag(length(mols))
        
            for (i in 1:length(mols)) {for (j in i:length(mols)) {
                fp1 = extractDrugEstate(mols[[i]])
                fp2 = extractDrugEstate(mols[[j]])
                tmp = calcDrugFPSim(fp1, fp2, fptype = input$dendogramFin, metric = input$dendogramMetric)
                simmat[i, j] = tmp
                simmat[j, i] = tmp
            }}
        
            mol.hc = hclust(as.dist(1 - simmat), method = input$dendogramMethod)
        
            require(ape) # for tree-like visualization
            clus5 = cutree(mol.hc, k=input$k)  # cut dendrogram into k clusters
            require(RColorBrewer)
            pal5 = brewer.pal(input$k, 'Set1')
            hc = as.phylo(mol.hc)
            hc$tip.label = c(as.numeric(dataM()[,1]))
            op = par(bg = input$dendogramColor)
            plot(as.phylo(hc), type = input$dendogramType, tip.color = pal5[clus5],
            label.offset = input$dendogramlabelOffset, cex = input$dendogramCex, direction=input$dendogramDirect, main = input$dendogramMain)
            
        }


        #####################################################
        ## Dendogram function for Plots tab (from SDF file)##
        #####################################################

        dendrogramInput2 = function() {
            inFileSDF <- input$uploadSDF
            if (is.null(input$uploadSDF))  {return(NULL)}
            #dataSDF <- read.table(inFileSDF$datapath, sep=mySep, header=TRUE, fill=TRUE)
            if (file.info(inFileSDF$datapath)$size <= 70108864){
                mols=readMolFromSDF(inFileSDF$datapath)
            }
    
            else print("File is bigger than 10MB and will not be uploaded.")
    
            simmat = diag(length(mols))
        
            for (i in 1:length(mols)) {for (j in i:length(mols)) {
                fp1 = extractDrugEstate(mols[[i]])
                fp2 = extractDrugEstate(mols[[j]])
                tmp = calcDrugFPSim(fp1, fp2, fptype = input$dendogramFin, metric = input$dendogramMetric)
                simmat[i, j] = tmp
                simmat[j, i] = tmp
            }}
    
            mol.hc = hclust(as.dist(1 - simmat), method = input$dendogramMethod)
    
    
            require(ape) # for tree-like visualization
            clus5 = cutree(mol.hc, k=input$k)  # cut dendrogram into k clusters
            require(RColorBrewer)
            pal5 = brewer.pal(input$k, 'Set1')
            hc = as.phylo(mol.hc)
            op = par(bg = input$dendogramColor)
            plot(as.phylo(hc), type = input$dendogramType, tip.color = pal5[clus5],
            label.offset = input$dendogramlabelOffset, cex = input$dendogramCex, direction=input$dendogramDirect, main = input$dendogramMain)
        }


        ##########################
        ## Molecule plot output ##
        #########################

        output$molPlot <- renderPlot({
            
            if(input$aButton2==0) {return(NULL)}
            
            else
    
            {molPlotInput()}
    
        })

        ####################
        ## Heatmap output ##
        ###################
            
        output$heatmap <- renderPlot({
            
            if(input$aHeatmap == 0) {return()}
            
            else
    
            {heatmapInput()}
    
        })

        ######################
        ## Dendogram output ##
        #####################
        
        output$dendogram <- renderPlot({
        
            if(input$dendInput==1){
                
                if(input$aDendogram==0) {return(NULL)}
                
                else
    
                {dendrogramInput1()}
         
            }else
    
            if(input$dendInput==2){  ## Upload data.
                
                if(input$aDendogramSDF==0) {return(NULL)}
                
                else
        
                {dendrogramInput2()}
        
            }

        })
        
        
        #######################
        ## Download SDF file ##
        ######################

        output$downloadSDF <- downloadHandler(

            filename = function() { "SDF_file.sdf" },
            content = function(file) {
            #if(input$aButton2==0) return()
                g = getIds(c(as.numeric(input$pubChemID2)))
                write.SDF(g, file, sig=FALSE)
            }
        )
        
        
        ############################
        ## Download molecule plot ##
        ###########################
    
        output$downloadPlot <- downloadHandler(
    
            filename <- function() { paste('plot.pdf') },
            content <- function(file) {
                pdf(file, width = input$mywidthPlot/72, height = input$myheightPlot/72)
                if(input$aButton2==0) {stop('First, select molecules and then click submit.')}
                else
                {molPlotInput()}
                dev.off()
            },
            contentType = 'application/pdf'
        )
    
        ######################
        ## Download heatmap ##
        #####################
    
        output$downloadHeatmap <- downloadHandler(
    
            filename <- function() { paste('heatmap.pdf') },
            content <- function(file) {
                pdf(file, width = input$mywidthHeat/72, height = input$myheightHeat/72)
                if(input$aHeatmap==0) {stop('Create heatmap first.')}
                if (input$dataInput != 4){ # upload / load ve paste için
            
                if(!input$molName){ ## Veri seti molekül isimlerini içermiyor ise;
                testRAW = dataM()
                }
            
                if(input$molName){ ## Veri seti molekül isimlerini içeriyor ise;
                testRAW = dataM()[,-1]
                }
            }
        
            heatmapInput()

            dev.off()
            },
            contentType = 'application/pdf'
        )
    
    
        ########################
        ## Download dendogram ##
        #######################
    
        output$downloadDendogram <- downloadHandler(
    
            filename <- function() { paste('dendogram.pdf') },
            content <- function(file) {
                pdf(file, width = input$mywidthHeat/72, height = input$myheightHeat/72)
                
        
            if(input$dendInput==1){
                if(input$aDendogram==0) {stop('Create dendogram first.')}
                
                else
           
                {dendrogramInput1()}
           
                dev.off()
           
            }else
       
            if(input$dendInput==2){  ## Upload data.
                
                if(input$aDendogramSDF==0) {stop('Create dendogram first.')}
                
                else
           
                {dendrogramInput2()}
          
                dev.off()
           
            }
            
            },contentType = 'application/pdf'
        )
        
        ###################
        ## System clock ##
        ##################

        output$clock <- renderText({
            invalidateLater(1000, session)
            paste("The current time is", Sys.time())
        })
    })

