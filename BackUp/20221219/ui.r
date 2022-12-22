options = list(display.mode='showcase')
navbarPage(actionLink('ResetSetting',   label=paste0("INPUT:___", labelInput), style = "font-size:12px;"),
			 
  id="nav", 
 ############################################################################################          
           tabPanel("CIC",
                     fluidPage (
					 theme = shinytheme(Theme),
                       
                    #   column(width = 4,actionLink('Rookery_polygon',   label= paste0("Rookery_polygon:  ", Rookery_polygon), style = "font-size:12px;")),
                    #   column(width = 4,actionLink('Haulout_polygon',   label= paste0("Haulout_polygon:  ", Haulout_polygon), style = "font-size:12px;")),
                    #   column(width = 4,actionLink('Exlude_polygon',   label= paste0("Exlude_polygon:  ", Exlude_polygon), style = "font-size:12px;")),
                    #   column(width = 10,actionLink('AnmlsMearPth',   label= paste0("AnmlsMearPth:  ", AnmlsMearPth), style = "font-size:12px;")),
                 
                 fluidRow(column(width = 11, selectInput('Bath_Process', 'Bath Process', width="1000px",multiple = T,
                                                        c("00_DataStructure"="00_DataStructure",
                                                           "01_Unzip"="01_Unzip",
														   "02.0_KMLprepare"="02.0_KMLprepare",
														   "03_Image_prepare"="03_Image_prepare",
                                                           "04_Unet"="04.0_Unet",
														   "05_BlobAnalys"="05_BlobAnalys",
														   "06_Geo_ref"="06_Geo_ref",
														   "07_KML"="07.0_KML"

														
                                                          ),
														  selected=listTMP$BatchProcessVector
                                                        )),
                          column(width = 6, selectInput('OPPListPred', 'OPP List', width="1000px",multiple = T,
                                                        c("All",listOPP),
							                            selected=listTMP$OPPListPred1),
														
														checkboxInput("SubFold", "SubFold", value=FALSE)
														
														
														)																								
														), 
														
												
	
			
														#############
      
				 
		
				 
				 
				 fluidRow(column(width = 4, actionButton('Start_Batch_process', 'Start', width="200px")),
				          column(width = 4, actionButton('Up', 'Up', width="200px"))
                           ),
                
                )),
#######################################################################################################
tabPanel("Settings",	 
		hr(), 
         fluidRow(column(width = 4, actionLink('System_data',label= paste0("System_data:  ", System_data), style = "font-size:12px;"))),
        hr(),
        
            fluidRow(column(width = 12,actionLink('NFS_Pup_weight_pth',label= paste0("NFS_Pup_weight_pth:  ", NFS_Pup_weight_pth), style = "font-size:12px;"))),
			hr(), 
             fluidRow(column(width = 12,actionLink('SQLite_path',label= paste0("SQLite_path:  ", SQLite_path), style = "font-size:12px;"))),
		 hr(),   
		    fluidRow(column(width = 12,actionLink('KK_Effort',label= paste0("KK_Effort:  ", KK_Effort), style = "font-size:12px;"))),
	   	 hr(),
		 checkboxInput("DarkTheme", "DarkTheme", value=DarkTheme),
		  hr(),
	  	   fluidRow(dataTableOutput("text")) 
),
#####################################################################################################
####################################################################################################  


conditionalPanel("false", icon("crosshair"))
 ) 
