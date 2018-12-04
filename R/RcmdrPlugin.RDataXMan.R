.onAttach <- function(libname, pkgname){
    putRcmdr("appconfig",FALSE)
	putRcmdr("appconfig2",FALSE)
	
	if (!interactive()) return()
    putRcmdr("slider.env", new.env())    
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if (!pkgname %in% plugins) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        if("package:Rcmdr" %in% search()) {
            if(!getRcmdr("autoRestart")) {
                closeCommander(ask=FALSE, ask.save=TRUE)
                Commander()
            }
        }
        else {
            Commander()
        }
    }
}

#' Test x is identical to empty string
#' @param x object to test
#' @return Returns boolean
is.charv <- function(x) identical(x, "")

#' Test x is identical to NA
#' @param x object to test
#' @return Returns boolean
is.charv2 <- function(x) identical(x, "NA")

#' Test x is identical to char(0)
#' @param x object to test
#' @return Returns boolean
is.charv3 <- function(x) identical(x, character(0))

#' Test x is identical to NULL
#' @param x object to test
#' @return Returns boolean
is.charv4 <- function(x) identical(x, NULL)

#' Test x is not equal to TRUE, FALSE, Union or Intersection
#' @param x object to test
#' @return Returns boolean
is.charv5 <- function(x) !(identical(x, "TRUE") | identical(x, "FALSE") | identical(x, "Union") | identical(x, "Intersection")) 

#' Test x is identical to int0
#' @param x object to test
#' @return Returns boolean
is.charv6 <- function(x) identical(x, integer(0))

#' Test x is logical
#' @param x object to test
#' @return Returns boolean
is.log1 <- function(x) is.logical(x)

#' Build string with quote marks
#' @param strings A string to add quotation marks to.
#' @return Returns input string collapsed with quotemarks
QuotationMarkAdd <- function(strings){
  tmp <- strsplit(strings,",",fixed=T)
  tmp <- unlist(tmp)
  new.strings<-do.call("c",lapply(seq_along(tmp), function(id){
    paste("\'",tmp[id],"\'",sep="")
  }))
  new.strings <- stringr::str_c(new.strings,collapse =",")
  return(new.strings)
}	

#' Writes a file intended to save the execution string to disk
#' @param filename name of file to write
#' @param textString string of R commands to write
#' @return None
saveJob <-function(filename,textString)
{
	fileout<-file(filename)
	writeLines(c(textString), fileout)
	close(fileout)
}

#' Save the current database profile in the research folder
#' @return None	
saveDB <- function() {

	if(getRcmdr("appconfig")==FALSE)
	{
		tkmessageBox(title = "RDataXMan", message = "Please select a datasource first", icon = "warning", type = "ok")
		return()
	}

	db <- c(getRcmdr("useDB"),getRcmdr("useORE"),getRcmdr("conusername"),getRcmdr("conpassword"),getRcmdr("conconn"),getRcmdr("condatabase"),getRcmdr("contable"),getRcmdr("dtype"),getRcmdr("rschfld"),getRcmdr("appconfig"),getRcmdr("condatabaseflat"),getRcmdr("appconfig2"))
	a<-paste0(getwd(),"/research/",getRcmdr("rschfld"),"/","profile")
	save(db,file=a)
	tkmessageBox(title = "RDataXMan", message = "Profile saved in research folder", icon = "info", type = "ok")
}

#' Automatically load a passed saved database profile in the research folder
#' @return None	
loadDB <- function() {
	
	tkmessageBox(title = "RDataXMan", message = "Automatically loaded research settings from profile file", icon = "info", type = "ok")
	
	a<-paste0(getwd(),"/research/",getRcmdr("rschfld"),"/","profile")
	load(a, envir = e <- new.env())
	print("loadDB")
	putRcmdr("useDB",e$db[1])
	putRcmdr("useORE",e$db[2])
	putRcmdr("conusername",e$db[3])
	putRcmdr("conpassword",e$db[4])
	putRcmdr("conconn",e$db[5])
	putRcmdr("condatabase",e$db[6])
	putRcmdr("contable",e$db[7])
	putRcmdr("dtype",e$db[8])
	putRcmdr("rschfld",e$db[9])
	putRcmdr("appconfig",e$db[10])
	putRcmdr("condatabaseflat",e$db[11])
	putRcmdr("appconfig",e$db[12])
}

#' Create the UI to establish the current working directory
#' @return None	
initwk <- function(){

    initializeDialog(title=gettextRcmdr("Set Working Directoy"))
    
	setwd(tk_choose.dir(default = getwd(),caption = "Select Working Directory"))
	
	defaults <- list(a=getwd())
    dialog.values <- getDialog("initwk", defaults)
    a <- tclVar(dialog.values$a)
    aEntry <- tkentry(top, width="60", textvariable=a)
    
    onOK <- function(){
        closeDialog()
        val1 <- (tclvalue(a))
		
		workingDIR <- val1
		command <- paste0("setwd(\"",val1,"\")")
		doItAndPrint(command)
        command <- paste0("initWkdir(\"",val1,"\")")
        doItAndPrint(command)
		tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="initWkdir", reset="initwk")
    tkgrid(tklabel(top, text="Working DIR: "), aEntry, sticky="e")
 
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(aEntry, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=aEntry)
	onOK()
    }

#' Display the UI to set the research base directory
#' @return None		
setrschfld <- function(){

    initializeDialog(title=gettextRcmdr("Set Research Directoy"))
    defaults <- list(a=getwd(),b="")
		
    dialog.values <- getDialog("setrschfld", defaults)
    a <- tclVar(dialog.values$a)
	b <- tclVar(dialog.values$b)
    aEntry <- tkentry(top, width="60", textvariable=a)
    bEntry <- tkentry(top, width="60", textvariable=b)
    
    onOK <- function(){
        
        val1 <- (tclvalue(a))
		val2 <- (tclvalue(b))
        
		if(is.charv(val1) | is.charv(val2))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
		
		closeDialog()
		command <- paste0("initResearchFolder(\"",val1,"\",\"",val2,"\")")
        doItAndPrint(command)
		command <- paste0("putRcmdr(\"rschfld\",\"",val2,"\")")
        doItAndPrint(command)
		
		a<-paste0(getwd(),"/research/",getRcmdr("rschfld"),"/","profile")
		if(file.exists(a))
		{
			print("function")
			
			res2 <- tkmessageBox(message = "Saved profile data found for this research folder, do you wish to load?", title = "RDataXMan",
								icon = "question", type = "yesno", default = "yes")
			
			tclvalue(res2)     
			print(as.character(res2))
			if(strcmp("yes",as.character(res2)) == TRUE)
			{
				loadDB()
				return()
			}
		}
		
		if(strcmp("0",getRcmdr("useDB")) == TRUE)
		{
			putRcmdr("appconfig",TRUE)
			pickTable()
			return()
		}
		
		if(strcmp("0",getRcmdr("useORE")) == TRUE)
		{
			initDB()
		}
		else
		{
			initORE()
		}
		
		
    }
	
    OKCancelHelp(helpSubject="initResearchFolder", reset="initwk")
    tkgrid(tklabel(top, text="Working DIR: "), aEntry, sticky="e")
	tkgrid(tklabel(top, text="Research Name: "), bEntry, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(aEntry, sticky="w")
	tkgrid.configure(bEntry, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=bEntry)
    }

#' Display UI to select a new database table
#' @return None		
pickTable <- function(){
	
	if(getRcmdr("appconfig")==FALSE)
	{
		tkmessageBox(title = "RDataXMan", message = "Please select a datasource first", icon = "warning", type = "ok")
		return()
	}
	
	tables <- c("")
	
	if(strcmp("0",getRcmdr("useDB")) == TRUE)
	{
		tables <- RDataXMan::ListTN(getwd(),database = getRcmdr("condatabase"),data.type = getRcmdr("dtype"),research.folder = getRcmdr("rschfld"))
	}
	else
	{
		if(strcmp("ore",getRcmdr("dtype")) == TRUE)
		{
			tables <- RDataXMan::ListTN(getwd(),database = getRcmdr("condatabase"),data.type = getRcmdr("dtype"),conn_string=getRcmdr("conconn"),username = getRcmdr("conusername"),password = getRcmdr("conpassword"),research.folder = getRcmdr("rschfld"))
		}
		
		if(strcmp("sql",getRcmdr("dtype")) == TRUE)
		{
			tables <- RDataXMan::ListTN(getwd(),database = getRcmdr("condatabase"),data.type = getRcmdr("dtype"),username = getRcmdr("conusername"),password = getRcmdr("conpassword"),research.folder = getRcmdr("rschfld"))
		
		}
		
		if(strcmp("oracle",getRcmdr("dtype")) == TRUE)
		{
			tables <- RDataXMan::ListTN(getwd(),database = getRcmdr("condatabase"),data.type = getRcmdr("dtype"),username = getRcmdr("conusername"),password = getRcmdr("conpassword"),research.folder = getRcmdr("rschfld"))
		}
	}
		
    initializeDialog(title=gettextRcmdr("Select Database Table"))
    defaults <- list(a=getwd(), b="", c="", d="", e="", f="",g="",h="",i=getRcmdr("contable"),j="",k="",l="")
	
	inclusionBox <- variableListBox(top, tables,
	title=gettextRcmdr("Database Table (pick one)"),
	selectmode="single", listHeight=10)
   
    dialog.values <- getDialog("pickTable", defaults)
	
    onOK <- function(){
			
		val1 <- getSelection(inclusionBox)	
			
		if(is.charv3(val1))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
		
		putRcmdr("contable",toString(val1[1]))
		putRcmdr("appconfig2",TRUE)
		tkmessageBox(title = "RDataXMan", message = "Application Configured, please generate Inclusion Criteria", icon = "info", type = "ok")
		closeDialog()
		}
    OKCancelHelp()

	tkgrid(tklabel(top, text=""),getFrame(inclusionBox), sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    dialogSuffix()
    }

#' Display the generate inclusion UI Window
#' @return None		
genIncUI <- function(){
	
	if(getRcmdr("appconfig2")==FALSE)
	{
		tkmessageBox(title = "RDataXMan", message = "Please select a table first", icon = "warning", type = "ok")
		return()
	}
	
    initializeDialog(title=gettextRcmdr("Generate Inclusion Critera"))
    defaults <- list(a=getwd(), i=getRcmdr("contable"))
	variablesList<-c("")
   
	if(strcmp("0",getRcmdr("useDB")) == TRUE)
	{
		variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"))
	}
	else
	{
		if(strcmp("ore",getRcmdr("dtype")) == TRUE)
		{
			variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"), username = getRcmdr("conusername"), password = getRcmdr("conpassword"), conn_string = getRcmdr("conconn"))
		}
		
		if(strcmp("sql",getRcmdr("dtype")) == TRUE)
		{
			variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"), username = getRcmdr("conusername"), password = getRcmdr("conpassword"))
		
		}
		
		if(strcmp("oracle",getRcmdr("dtype")) == TRUE)
		{
			variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"), username = getRcmdr("conusername"), password = getRcmdr("conpassword"))
		}
	}
   
    dialog.values <- getDialog("genIncUI", defaults)
	
	keyV <- variableListBox(top, variablesList,
	title=gettextRcmdr("(pick one)"),
	selectmode="single", listHeight=10)
	
	keyD <- variableListBox(top, variablesList,
	title=gettextRcmdr("(pick zero or more)"),
	selectmode="multiple", listHeight=10)
	
	identV <- variableListBox(top, variablesList,
	title=gettextRcmdr("(pick one or more)"),
	selectmode="multiple", listHeight=10)
	
	i <- tclVar(dialog.values$i)
    iEntry <- tkentry(top, width="60", textvariable=i)
	
    onOK <- function(){
        
		val1 <- getRcmdr("rschfld")
		tmpvar <- getSelection(keyV)
		val3 <- toString(tmpvar[1])
		val4 <- getSelection(keyD)
		val5 <- getSelection(identV)
		val6 <- TRUE
		val8 <- getSelection(overwriteComboBox1)
		val9 <- (tclvalue(i))
		saveEx <- getSelection(saveEXComboBox2)
		
		#| is.charv3(val4)
		
		if(is.charv(val1) | is.charv2(val3) | is.charv3(val5) | is.charv(val6) | is.charv5(val8) | is.charv5(saveEx) | is.charv(val9))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
				
        command <- paste0("genInclusion(wkdir = \"",getwd(),"\",research.folder = \"",val1,"\", table_name = \"",val9,"\", key.var = \"",val3,
		"\", key.desc = c(",QuotationMarkAdd(paste(val4,collapse=",")),"), identifier.var = c(",QuotationMarkAdd(paste(val5,collapse=",")),"), count = \"",val6,"\", data.type = \"",getRcmdr("dtype"),"\", overwrite = ",
		val8,", username = \"",getRcmdr("conusername"),"\", password = \"",getRcmdr("conpassword"),"\", conn_string = \"",getRcmdr("conconn"),"\", database = \"",getRcmdr("condatabase"),"\")")
        
		if(saveEx==TRUE)
		{
			saveJob(paste0(getwd(),"/research/",getRcmdr("rschfld"),"/genInc-",gsub(pattern = ":",replacement = "-",x = Sys.time()),".txt"),command)
		}
		
		doItAndPrint(command)
		msg <- paste0("Operation Complete, Please proceed to complete the template in folder public or private template and move it to folder ",getwd(),"/research/",getRcmdr("rschfld"),"/request_input")
		tkmessageBox(title = "RDataXMan", message = msg, icon = "info", type = "ok")
		tkfocus(CommanderWindow())
		closeDialog()
        }

	OKCancelHelp(helpSubject="genIncUI", reset="genIncUI", apply="genIncUI")
 	tkgrid(tklabel(top, text="Key Variable: "),getFrame(keyV), sticky="e")
	tkgrid(tklabel(top, text="Key Descriptions: "),getFrame(keyD), sticky="e")
	tkgrid(tklabel(top, text="Table:"), iEntry, sticky="e")
	tkgrid(tklabel(top, text="Identifier Variables: "),getFrame(identV), sticky="e")
	
	VariablesCombo1 <- c("TRUE","FALSE")
	overwriteComboBox1 <- variableComboBox(top, variableList=VariablesCombo1,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("TRUE"),
    title="")
	
	tkgrid(tklabel(top, text="Overwrite:"),getFrame(overwriteComboBox1), sticky="e")
	
	VariablesCombo2 <- c("TRUE","FALSE")
	saveEXComboBox2 <- variableComboBox(top, variableList=VariablesCombo2,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("TRUE"),
    title="")
	
	tkgrid(tklabel(top, text="Save Execution:"),getFrame(saveEXComboBox2), sticky="e")
	
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    dialogSuffix(rows=4, columns=2)
    }

#' Display the generate variable UI Window
#' @return None		
genVarUI <- function(){

	if(getRcmdr("appconfig2")==FALSE)
	{
		tkmessageBox(title = "RDataXMan", message = "Please select a table first", icon = "warning", type = "ok")
		return()
	}
	
	variablesList<-c("")
   
	if(strcmp("0",getRcmdr("useDB")) == TRUE)
	{
		variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"))
	}
	else
	{
		if(strcmp("ore",getRcmdr("dtype")) == TRUE)
		{
			variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"), username = getRcmdr("conusername"), password = getRcmdr("conpassword"), conn_string = getRcmdr("conconn"))
		}
		
		if(strcmp("sql",getRcmdr("dtype")) == TRUE)
		{
			variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"), username = getRcmdr("conusername"), password = getRcmdr("conpassword"))
		
		}
		
		if(strcmp("oracle",getRcmdr("dtype")) == TRUE)
		{
			variablesList <- RDataXMan::ListVN(getwd(),research.folder = getRcmdr("rschfld"),data.type = getRcmdr("dtype"),database = getRcmdr("condatabase"),table_name = getRcmdr("contable"), username = getRcmdr("conusername"), password = getRcmdr("conpassword"))
		}
	}
		
    initializeDialog(title=gettextRcmdr("Generate Variable List"))
    defaults <- list(a=getwd(), i=getRcmdr("contable"))
    dialog.values <- getDialog("genVarUI", defaults)
   
	i <- tclVar(dialog.values$i)
    iEntry <- tkentry(top, width="60", textvariable=i)
		
	identV <- variableListBox(top, variablesList,
	title=gettextRcmdr("(pick one or more)"),
	selectmode="multiple", listHeight=10)
	
	omitV <- variableListBox(top, variablesList,
	title=gettextRcmdr("(pick zero or more)"),
	selectmode="multiple", listHeight=10)

    onOK <- function(){
		val1 <- getRcmdr("rschfld")
		val3 <- getSelection(identV)
		val4 <- getSelection(omitV)
		val6 <- getSelection(overwriteComboBox1)
		val8 <- (tclvalue(i))
		saveEx <- getSelection(saveEXComboBox2)
		
		# | is.charv3(val4)
		
		if(is.charv(val1) | is.charv3(val3) | is.charv5(val6) | is.charv(val8) | is.charv5(saveEx))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
		
		command <- paste0("genVariable(wkdir = \"",getwd(),"\",research.folder = \"",val1,"\", table_name  = \"",val8,"\", 
		identifier.var = c(",QuotationMarkAdd(paste(val3,collapse=",")),"), omit.var = c(",QuotationMarkAdd(paste(val4,collapse=",")),"), data.type = \"",getRcmdr("dtype"),"\", overwrite = ",val6,", 
		username = \"",getRcmdr("conusername"),"\", password = \"",getRcmdr("conpassword"),"\", conn_string = \"",getRcmdr("conconn"),
		"\", database = \"",getRcmdr("condatabase"),"\")")
        
		if(saveEx==TRUE)
		{
			saveJob(paste0(getwd(),"/research/",getRcmdr("rschfld"),"/genVar-",gsub(pattern = ":",replacement = "-",x = Sys.time()),".txt"),command)
		}
		
		doItAndPrint(command)
		msg <- paste0("Operation Complete, Please proceed to complete the template in folder public or private template and move it to folder ",getwd(),"/template and move it to folder ",getwd(),"/research/",getRcmdr("rschfld"),"/request_input")
		tkmessageBox(title = "RDataXMan", message = msg, icon = "info", type = "ok")       
		tkfocus(CommanderWindow())
		closeDialog()
        }

	OKCancelHelp(helpSubject="genVarUI", reset="genVarUI", apply="genVarUI")
   
	tkgrid(tklabel(top, text="Identifier Variables: "),getFrame(identV), sticky="e")
	tkgrid(tklabel(top, text="Table:"), iEntry, sticky="e")
	tkgrid(tklabel(top, text="Omit Variables:"),getFrame(omitV), sticky="e")

	VariablesCombo1 <- c("TRUE","FALSE")
	overwriteComboBox1 <- variableComboBox(top, variableList=VariablesCombo1,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("TRUE"),
    title="")	

	tkgrid(tklabel(top, text="Overwrite:"),getFrame(overwriteComboBox1), sticky="e")

	VariablesCombo2 <- c("TRUE","FALSE")
	saveEXComboBox2 <- variableComboBox(top, variableList=VariablesCombo2,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("TRUE"),
    title="")
	
	tkgrid(tklabel(top, text="Save Execution:"),getFrame(saveEXComboBox2), sticky="e")

    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    dialogSuffix(rows=4, columns=2)
    }
	
#' Display the Extraction UI Window
#' @return None		
extractUI <- function(){
	if(getRcmdr("appconfig2")==FALSE)
	{
		tkmessageBox(title = "RDataXMan", message = "Please select a table first", icon = "warning", type = "ok")
		return()
	}
		
    initializeDialog(title=gettextRcmdr("Extract Data"))
	rsch <- ""
	if (!is.null(getRcmdr("rschfld",fail = FALSE))) {
	rsch <- getRcmdr("rschfld",fail = FALSE)
	}

    defaults <- list(a=getwd(), b=rsch)
	inclusionFiles<-RDataXMan::ListFN(getwd(),rsch,"inclusion")
	variableFiles<-RDataXMan::ListFN(getwd(),rsch,"variable")
	
	dialog.values <- getDialog("extractUI", defaults)
	availableModes <- c("Generate Identifier Variable List","Generate Summary Statistics","Extract Data","Merge Data")

	inclusionBox <- variableListBox(top, inclusionFiles,
	title=gettextRcmdr("(pick one or more)"),
	selectmode="multiple", listHeight=10)
	
	variablesBox <- variableListBox(top, variableFiles,
	title=gettextRcmdr("(pick one or more)"),
	selectmode="multiple", listHeight=10)
	
	modeBox <- variableListBox(top, availableModes,
	title=gettextRcmdr("(pick one or more)"),
	selectmode="multiple", listHeight=10)
	
	VariablesCombo1 <- c("TRUE","FALSE")
	overwriteComboBox1 <- variableComboBox(top, variableList=VariablesCombo1,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("TRUE"),
    title="")
	
	VariablesCombo2 <- c("TRUE","FALSE")
	saveEXComboBox2 <- variableComboBox(top, variableList=VariablesCombo2,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("TRUE"),
    title="")
	
	VariablesCombo <- c("Union","Intersection")
	datalogicComboBox <- variableComboBox(top, variableList=VariablesCombo,
    export="FALSE", state="readonly",
    initialSelection=gettextRcmdr("Union"),
    title="")
	
    onOK <- function(){
        mmode <- getSelection(modeBox)	
		outputmodes <- which(availableModes%in%mmode)
		  
		val3 <- getSelection(inclusionBox)
		val4 <- getSelection(variablesBox)
		val5 <- getSelection(datalogicComboBox) 
		val6 <- outputmodes
		val7 <- getSelection(overwriteComboBox1) 
		saveEx <- getSelection(saveEXComboBox2) 
		
		if(is.charv3(val3) | is.charv3(val4) | is.charv5(val5)| is.charv6(val6) | is.charv3(val7) | is.charv5(saveEx))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
			
		command <- paste0("rdataxman_result <- extract_data(wkdir = \"",getwd(),"\", research.folder = \"",getRcmdr("rschfld"),"\", 
		inclusion.xls.file = c(",QuotationMarkAdd(paste(val3,collapse=",")),"), variable.xls.file = c(",QuotationMarkAdd(paste(val4,collapse=",")),"), 
		dataLogic = \"",val5,"\", select.output = c(",QuotationMarkAdd(paste(val6,collapse=",")),"), overwrite = ",val7,",username = \"",getRcmdr("conusername"),
		"\", password = \"",getRcmdr("conpassword"),"\", conn_string = \"",getRcmdr("conconn"),"\", database = \"",getRcmdr("condatabase"),"\")")
		print(command)
		
		if(saveEx==TRUE)
		{
			saveJob(paste0(getwd(),"/research/",getRcmdr("rschfld"),"/exData-",gsub(pattern = ":",replacement = "-",x = Sys.time()),".txt"),command)
		}
		
        doItAndPrint(command)
		tkmessageBox(title = "RDataXMan", message = "Operation Complete, to save extraction log select File -> Save script as...", icon = "info", type = "ok")
		closeDialog(top)
		tkfocus(CommanderWindow())
    }
    
	OKCancelHelp(helpSubject="extractUI", reset="extractUI", apply="extractUI")   
	tkgrid(tklabel(top, text="Inclusion Files: "),getFrame(inclusionBox), sticky="e")
	tkgrid(tklabel(top, text="Inclusion Data Logic:"),getFrame(datalogicComboBox), sticky="e")
	tkgrid(tklabel(top, text="Variable Files: "),getFrame(variablesBox), sticky="e")
	tkgrid(tklabel(top, text="Overwrite:"),getFrame(overwriteComboBox1), sticky="e")
	tkgrid(tklabel(top, text="Modes: "),getFrame(modeBox), sticky="e")
	tkgrid(tklabel(top, text="Save Execution:"),getFrame(saveEXComboBox2), sticky="e")
	tkgrid(tklabel(top, text="Note: Clicking OK or Apply may\ncause the overwrite\n of previous data extractions"), sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix()
    }
	
#' Launch masking tool from GitHub
#' @return None	
maskingtool <- function() {
	source("https://raw.githubusercontent.com/dasasmk/EzyDeident/master/setup.R")
	runGitHub("EzyDeident", "dasasmk",launch.browser=TRUE,port=9999) 
}

#' Display the wizard to identify the users data file/database connection
#' @return None	
setupwizard <- function()
{

  putRcmdr("useDB","")
  putRcmdr("useORE","")
  putRcmdr("conusername","")
  putRcmdr("conpassword","")
  putRcmdr("conconn","")
  putRcmdr("condatabase","")
  putRcmdr("condatabaseflat","")
  putRcmdr("contable","")
  putRcmdr("dtype","")
  putRcmdr("rschfld","")
  putRcmdr("appconfig",FALSE)
  putRcmdr("appconfig2",FALSE)
  
  res <- tkmessageBox(message = "Are you using a database?", title = "RDataXMan",
                      icon = "question", type = "yesno", default = "yes")
  
  tclvalue(res)     
  print(as.character(res))

  if(strcmp("yes",as.character(res)) == TRUE)
  {
    putRcmdr("useDB","1")
	
	res2 <- tkmessageBox(message = "Are you using Oracle R Enterprise?", title = "RDataXMan",
                        icon = "question", type = "yesno", default = "yes")
    
    tclvalue(res2)     
    print(as.character(res2))
    if(strcmp("yes",as.character(res2)) == TRUE)
    {
      putRcmdr("useORE","1")
	  print("Oracle ORE Selected")
	  putRcmdr("dtype","ore")
	  initwk()
	  setrschfld()
    }
    else
    {
      res3 <- tkmessageBox(message = "Are you using Oracle in SQL Mode? If No, MySQL mode will be used", title = "RDataXMan",
      icon = "question", type = "yesno", default = "yes")
    
		tclvalue(res3)     
		print(as.character(res3))
		if(strcmp("yes",as.character(res3)) == TRUE)
		{
			putRcmdr("useORE","0")
			putRcmdr("dtype","oracle")
			print("Oracle SQL Selected")
		}
		else
		{
			putRcmdr("useORE","0")
			putRcmdr("dtype","sql")
			print("MySQL Selected")
		}

	  initwk()
	  setrschfld()
	}
    tkfocus(CommanderWindow())
  }
  else
  {
    resPoP <- tkmessageBox(message = "Are you using your private data?", title = "RDataXMan",
                      icon = "question", type = "yesno", default = "yes")
					  
					  
	tclvalue(resPoP)     
    print(as.character(resPoP))
    if(strcmp("yes",as.character(resPoP)) == TRUE)
    {
		putRcmdr("condatabase","private")
	}
	else
	{
		putRcmdr("condatabase","public")
	}
	
	putRcmdr("useDB","0")
	putRcmdr("useORE","0")
	print("Flat File Selected")
	putRcmdr("dtype","flat")
	initwk()
	setrschfld()
  }
  
  print(getRcmdr("useDB"))
  print(getRcmdr("useORE"))
  print(getRcmdr("dtype"))
  print(getRcmdr("condatabase"))  
}

#' Show the database credential dialog
#' @return None	
initDB <- function(){

    initializeDialog(title=gettextRcmdr("Data Setup"))
	
    defaults <- list(a="", b="", c="", d="",e="")
    dialog.values <- getDialog("initDB", defaults)
    
	a <- tclVar(dialog.values$a)
    aEntry <- tkentry(top, width="60", textvariable=a)
    b <- tclVar(dialog.values$b)
    bEntry <- tkentry(top, width="60",show='*', textvariable=b)
   	d <- tclVar(dialog.values$c)
    dEntry <- tkentry(top, width="60", textvariable=d)
	
    onOK <- function(){
        
        val1 <- (tclvalue(a))
		putRcmdr("conusername",val1)
		val2 <- (tclvalue(b))
		putRcmdr("conpassword",val2)
		putRcmdr("conconn","")
		val4 <- (tclvalue(d))
		putRcmdr("condatabase",val4)
		putRcmdr("appconfig",TRUE)
		
		if(is.charv(val1) | is.charv(val2) | is.charv(val4))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
		closeDialog()
        tkfocus(CommanderWindow())
		putRcmdr("appconfig",TRUE)
		pickTable()
        }
    
	OKCancelHelp(helpSubject="initDB", reset="extractUI")

	tkgrid(tklabel(top, text="Username:"), aEntry, sticky="e")
	tkgrid(tklabel(top, text="Password:"), bEntry, sticky="e")	
	tkgrid(tklabel(top, text="Database:"), dEntry, sticky="e")

    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(aEntry, sticky="w")
    tkgrid.configure(bEntry, sticky="w")
	tkgrid.configure(dEntry, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=aEntry)
    }

#' Create the UI to establish an ORE connection
#' @return None	
initORE <- function(){

    initializeDialog(title=gettextRcmdr("Data Setup"))
	
    defaults <- list(a="", b="", c="", d="",e="")
    dialog.values <- getDialog("initDB", defaults)
    
	a <- tclVar(dialog.values$a)
    aEntry <- tkentry(top, width="60", textvariable=a)
    b <- tclVar(dialog.values$b)
    bEntry <- tkentry(top, width="60",show='*', textvariable=b)
    c <- tclVar(dialog.values$c)
	cEntry <- tkentry(top, width="60", textvariable=c)
	d <- tclVar(dialog.values$c)
    dEntry <- tkentry(top, width="60", textvariable=d)
	e <- tclVar(dialog.values$c)
    eEntry <- tkentry(top, width="60", textvariable=e)
	
    onOK <- function(){
        
        val1 <- (tclvalue(a))
		putRcmdr("conusername",val1)
		val2 <- (tclvalue(b))
		putRcmdr("conpassword",val2)
		val3 <- (tclvalue(c))
		putRcmdr("conconn",val3)
		val4 <- (tclvalue(d))
		putRcmdr("condatabase",val4)
		putRcmdr("appconfig",TRUE)

		if(is.charv(val1) | is.charv(val2) | is.charv(val3) | is.charv(val4))
		{
			tkmessageBox(title = "RDataXMan", message = "Required Items are missing", icon = "warning", type = "ok")
			return()
		}
		closeDialog()
        tkfocus(CommanderWindow())
		putRcmdr("appconfig",TRUE)
		pickTable()
    }
    
	OKCancelHelp(helpSubject="extract_data", reset="extractUI")

	tkgrid(tklabel(top, text="Username:"), aEntry, sticky="e")
	tkgrid(tklabel(top, text="Password:"), bEntry, sticky="e")
	tkgrid(tklabel(top, text="Conn String:"), cEntry, sticky="e")
	tkgrid(tklabel(top, text="Database:"), dEntry, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(aEntry, sticky="w")
    tkgrid.configure(bEntry, sticky="w")
    tkgrid.configure(cEntry, sticky="w")
	tkgrid.configure(dEntry, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=aEntry)
    }
	