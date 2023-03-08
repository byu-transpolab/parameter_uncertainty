
  folder<-"C:/projects/rvtpo_master/Base"
  new.folder <- "C:/projects/sensitivity_out"
  files <- list.files(folder, full.names = TRUE, recursive = TRUE, pattern = "LOADED_2012A.NET")
  files_dn <- list.files(folder, full.names = FALSE, recursive = TRUE, pattern = "LOADED_2012A.NET")
  foldernamesinitial <- toupper(substr(files_dn, 1, 5))
  foldernames <- replace(foldernamesinitial, 101, "BASE")
  foldernames <- replace(foldernames, 3, "LHS100")
  foldernames <- replace(foldernames, 1, "LHS1")
  foldernames <- replace(foldernames, 13, "LHS2")
  foldernames <- replace(foldernames, 24, "LHS3")
  foldernames <- replace(foldernames, 35, "LHS4")
  foldernames <- replace(foldernames, 46, "LHS5")
  foldernames <- replace(foldernames, 57, "LHS6")
  foldernames <- replace(foldernames, 68, "LHS7")
  foldernames <- replace(foldernames, 79, "LHS8")
  foldernames <- replace(foldernames, 90, "LHS9")
  
  
  
  file_names_new <- paste0(foldernames, "_LOADED.NET")
  
  files_new <- paste0(new.folder, "/", file_names_new) 
  file.copy(files, files_new, overwrite = TRUE)

  
  