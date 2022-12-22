options = list(display.mode='showcase')
navbarPage(actionLink('ResetSetting',   label=paste0("INPUT:___", pthOPP), style = "font-size:12px;"),
			 
  id="nav", 
 ############################################################################################          
           tabPanel("CIC",
                     fluidPage (
					 theme = shinytheme(Theme),    
                 fluidRow(column(width = 11, selectInput('Bath_Process', 'Bath Process', width="1000px",multiple = T,
                                                        c("00_DataStructure"="00_DataStructure",
                                                           "01_Unzip"="01_Unzip",
														   "02.0_KMLprepare"="02.0_KMLprepare",
														   "03_Image_prepare"="03_Image_prepare",
                                                           "04_Unet"="04.0_Unet",
														   "05_BlobAnalys"="05_BlobAnalys",
														   "06_Geo_ref"="06_Geo_ref",
														   "07_KML"="07.0_KML",
														   "test"="test",
														   
														   
														  "08.01_Points_table_for_mask"="08.01_Points_table_for_mask",
														  "08.02_Animals_Count_On_Image"="08.02_Animals_Count_On_Image",
														  "08.03_Mask_Image_Create"="08.03_Mask_Image_Create"	
														   

														
                                                          ),
														  selected=BatchProcessVector
                                                        )),
                          column(width = 11, selectInput('OPPListPred', 'OPP List', width="1000px",multiple = T,
                                                        c("All",listOPP),
							                            selected=listTMP$OPPListPred1)
														#checkboxInput("SubFold", "SubFold", value=TRUE)														
														)																								
														), 
														#############
				 fluidRow(column(width = 4, actionButton('Start_Batch_process', 'Start', width="200px"))
				        #  column(width = 4, actionButton('Up', 'Up', width="200px"))
                           ),
						   hr(),
				 fluidRow(column(width = 4,textInput("X", "X:", value="0.00000006442986"))),
				  fluidRow(column(width = 4,textInput("Y", "Y:", value="0.00000003503696"))),
                
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
