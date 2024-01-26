qmd_file_create<- function(filename = "filename", 
                           title = "Some title", 
                           text = "Materiale in preparazione") {
  filename<- paste0(filename, ".qmd")
  
  cat("---", "\n",
      "title: ",
      title,
      "\n---\n\n",
      text, "\n",
      file = filename)
}

files_list<- c("concetti_strumenti",
               "analisi_fenomeni_demo",
               "tavole_mortalita",
               "mortalita_approfondimenti",
               "form_scioglim_coppie",
               "fecondita",
               "fecondita_controllata",
               "mobilita_migrazioni",
               "modelli_popolazione",
               "rif_bibliografici")

for(x in files_list) qmd_file_create(x, title = x)
