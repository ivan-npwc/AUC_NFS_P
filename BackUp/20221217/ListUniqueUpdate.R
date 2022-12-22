
listUniq <- list(
                  listOPP=listOPP,   
                  pthOPP=pthOPP,
                  labelInput = labelInput,
                  NFS_Pup_weight_pth=NFS_Pup_weight_pth,
                  SQLite_path=SQLite_path,
                  KK_Effort=KK_Effort, 
				  System_data=System_data,
				  site=site,
				  DarkTheme=DarkTheme,
				  type=type


				  
               )
			    saveRDS(listUniq, "listUniq")
				
				
		