##### TITRE   :   MeF Nom Fichier       #####
##### AUTEUR  :   IR                    #####
##### DATE DE CREATION      :   2016    #####
##### DATE DE MODIFICATION  :   2021    #####




##### DOSSIERS #####
      setwd("C:/OneDrive/Images")
      dir()
      ls()

      
      

##### SUPPRESSION/MODIFICATION #####
      list_file       <-  dir()[which(dir() %in% c(".jpeg", ".gif", dir()[grep("Fleurs", dir())]))]
      #list_invertfile <-  dir()[which(!dir() %in% c(".jpeg", ".gif", dir()[grep("Fleurs", dir())]))]

      
      list_drop      <-  c(")")
      list_replace   <-  c(" (")
      
      
      #i <- 2
      #k <- 7
      
      for (i in 1:length(list_file)) {
        name_toreplace  <-  list_file[i]
      
        for (k in 1:nchar(name_toreplace)) {
          if (substring(name_toreplace,k,k+(nchar(list_drop)-1)) == list_drop) {
            name_modif1  <-  paste(substring(name_toreplace,1,k-1),substring(name_toreplace,k+nchar(list_drop),nchar(name_toreplace)),sep="")
            file.rename(name_toreplace,name_modif1)
            rm(name_modif1)
            break
          }
        };  rm(k)
      
        rm(name_toreplace)
      };  rm(i)





##### INSERTION #####
      list_file    <-  dput(dir())
      list_insert  <-  c("2006-2012_iris2_Cl06_", "cancer_prostate_homme")
      
      i          <- 2
      nbinsert   <- 2
      
      for (i in 1:length(list_file)) {
        name_toreplace  <-  list_file[i]
        name_split1  <-  unlist(strsplit(name_toreplace, split="_"))
        
        for (nbinsert in 1:length(list_insert)) {
          type_insert_split  <-  strsplit(list_insert[nbinsert], split="_")[[1]]
          
          if (FALSE %in% c(type_insert_split %in% name_split1)) {
            if (nbinsert == 1) {
              ind_deb     <-  which(name_split1 %in% "DivBy-1")
              ind_fin     <-  which(name_split1 %in% "Matrice2014")
              name_split2  <- c(name_split1[1:ind_deb], type_insert_split, name_split1[ind_fin:length(name_split1)])
              name_modif1  <- paste(name_split2, collapse="_")
              rm(ind_deb, ind_fin)
            }#IF1.A
            if (nbinsert == 2) {
              if ("name_modif1" %in% ls()) {
                ind_deb      <-  which(name_split2 %in% c("ProbNull", paste("Scen", c(1:9), sep="")))
                ind_fin      <-  which(name_split2 %in% "DivBy-1")
                name_split3   <-  c(name_split2[1:ind_deb], type_insert_split, name_split2[ind_fin:length(name_split2)])
                name_modif2  <- paste(name_split3, collapse="_")
                file.rename(name_toreplace,name_modif1)
                file.rename(name_modif1,name_modif2);  rm(name_split2, name_modif1, name_split3, name_modif2)
                rm(ind_deb, ind_fin)
              }#IF1.B.1
            }#IF1.B
          }#IF1
          
          rm(type_insert_split)
        };  rm(nbinsert)
        
        rm(name_split1, name_toreplace)
      };  rm(i)




##### MODIFICATION A L-INTERIEUR D-UN FICHIER #####
      list_file      <-  dir()[which(!dir() %in% c("Old", "Temp", dir()[grep("0-499", dir())]))]
      dir_changing   <-  "C:/Application/R/SIMULATION/Output/Moran/Temp"
      list_ToDrop    <-  c("; poisson ; SIR ;")
      list_ToChange  <-  c("; Normalite ;", "; Permuation ;", "; Permuation aleatoire ;", ";   ; Bootstrap parametrique ;", "; poisson ;", ";   ; EBI ;")
      list_changing   <-  c("; Normality ;", "; Randomisation ;", "; Bootstrap Non-parametrique ;",  "; Observed ; Bootstrap Parametrique ;", "; Poisson ;", "; RR ; EBI ;")

      
      nbfile   <-  1
      nblgn    <-  15
      nbletter <-  47
      nbmod    <-  1
      
      for (nbfile in 1:length(list_file)) {
        name_file  <-  list_file[nbfile]
        file       <-  read.table(name_file, header=FALSE, sep="\t", fill=TRUE, stringsAsFactors=FALSE)
        
        for (nblgn in 1:nrow(file)) {
          lgn.ToChange  <-  file[nblgn,1]
          
          for (nbletter in 1:nchar(lgn.ToChange)) {
            
            for (nbmod in 1:length(list_ToChange)) {
              if (substring(lgn.ToChange,nbletter,nbletter+(nchar(list_ToChange[nbmod])-1)) == list_ToChange[nbmod]) {
                lgn_changing   <-  paste(substring(lgn_ToChange,1,nbletter-1), list_changing[nbmod], substring(lgn_ToChange,nbletter+nchar(list_ToChange[nbmod]),nchar(lgn_ToChange)),sep="")
                file[nblgn,1]  <-  lgn_changing
                rm(lgn_changing)
              }
            };  rm(nbmod)

          };  rm(nbletter)
          
          rm(lgn_ToChange)
        };  rm(nblgn)
        
        path_file_ToSave  <-  paste(dir_changing, name_file, sep="/")
        write.table(file, path_file_ToSave, row.names = FALSE, col.names=FALSE, eol="\n", quote=FALSE)
        
        rm(file, name_file)
      };  rm(i)



      
      
##### SUPPRESSION/MODIFICATION #####
      
      setwd("C:/Pellicule")
      
      list_dirs       <-  dir()[which(!dir() %in% c(".jpeg", ".gif", ".mp4", ".MPG", ".mov"))]
      
      list_file       <-  dir()[which(dir() %in% c(".jpeg", ".gif", dir()[grep("Violet", dir())]))]
      
      dir()[1]
      file.mtime(dir()[1])
