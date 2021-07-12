##### TITRE   :   MeF Nom Fichier       #####
##### AUTEUR  :   Ingrid ROCHE          #####
##### DATE DE CREATION      :   2016    #####
##### DATE DE MODIFICATION  :   2021    #####




##### DOSSIERS #####
      setwd("C:/IngridROCHE")
      dir()
      ls()

      
      

##### SUPPRESSION/MODIFICATION #####
      list.file       <-  dir()[which(dir() %in% c(".jpeg", ".gif", dir()[grep("Violet", dir())]))]
      #list.invertfile <-  dir()[which(!dir() %in% c(".jpeg", ".gif", dir()[grep("Violet", dir())]))]

      
      list.drop      <-  c(")")
      list.replace   <-  c(" (")
      
      
      #i <- 2
      #k <- 7
      
      for (i in 1:length(list.file)) {
        name.toreplace  <-  list.file[i]
      
        for (k in 1:nchar(name.toreplace)) {
          if (substring(name.toreplace,k,k+(nchar(list.drop)-1)) == list.drop) {
            name.modif1  <-  paste(substring(name.toreplace,1,k-1),substring(name.toreplace,k+nchar(list.drop),nchar(name.toreplace)),sep="")
            file.rename(name.toreplace,name.modif1)
            rm(name.modif1)
            break
          }
        };  rm(k)
      
        rm(name.toreplace)
      };  rm(i)





##### INSERTION #####
      list.file    <-  dput(dir())
      list.insert  <-  c("2006-2012_iris2_Cl06_", "cancer_prostate_homme")
      
      i          <- 2
      nbinsert   <- 2
      
      for (i in 1:length(list.file)) {
        name.toreplace  <-  list.file[i]
        name.split1  <-  unlist(strsplit(name.toreplace, split="_"))
        
        for (nbinsert in 1:length(list.insert)) {
          type.insert.split  <-  strsplit(list.insert[nbinsert], split="_")[[1]]
          
          if (FALSE %in% c(type.insert.split %in% name.split1)) {
            if (nbinsert == 1) {
              ind.deb     <-  which(name.split1 %in% "DivBy-1")
              ind.fin     <-  which(name.split1 %in% "Matrice2014")
              name.split2  <- c(name.split1[1:ind.deb], type.insert.split, name.split1[ind.fin:length(name.split1)])
              name.modif1  <- paste(name.split2, collapse="_")
              rm(ind.deb, ind.fin)
            }#IF1.A
            if (nbinsert == 2) {
              if ("name.modif1" %in% ls()) {
                ind.deb      <-  which(name.split2 %in% c("ProbNull", paste("Scen", c(1:9), sep="")))
                ind.fin      <-  which(name.split2 %in% "DivBy-1")
                name.split3   <-  c(name.split2[1:ind.deb], type.insert.split, name.split2[ind.fin:length(name.split2)])
                name.modif2  <- paste(name.split3, collapse="_")
                file.rename(name.toreplace,name.modif1)
                file.rename(name.modif1,name.modif2);  rm(name.split2, name.modif1, name.split3, name.modif2)
                rm(ind.deb, ind.fin)
              }#IF1.B.1
            }#IF1.B
          }#IF1
          
          rm(type.insert.split)
        };  rm(nbinsert)
        
        rm(name.split1, name.toreplace)
      };  rm(i)




##### MODIFICATION A L-INTERIEUR D-UN FICHIER #####
      list.file      <-  dir()[which(!dir() %in% c("Old", "Temp", dir()[grep("0-499", dir())]))]
      dir.changing   <-  "C:/IngridROCHE/Application/R/SIMULATION/Output/Moran/Temp"
      list.ToDrop    <-  c("; poisson ; SIR ;")
      list.ToChange  <-  c("; Normalite ;", "; Permuation ;", "; Permuation aleatoire ;", ";   ; Bootstrap parametrique ;", "; poisson ;", ";   ; EBI ;")
      list.changing   <-  c("; Normality ;", "; Randomisation ;", "; Bootstrap Non-parametrique ;",  "; Observed ; Bootstrap Parametrique ;", "; Poisson ;", "; RR ; EBI ;")

      
      nbfile   <-  1
      nblgn    <-  15
      nbletter <-  47
      nbmod    <-  1
      
      for (nbfile in 1:length(list.file)) {
        name.file  <-  list.file[nbfile]
        file       <-  read.table(name.file, header=FALSE, sep="\t", fill=TRUE, stringsAsFactors=FALSE)
        
        for (nblgn in 1:nrow(file)) {
          lgn.ToChange  <-  file[nblgn,1]
          
          for (nbletter in 1:nchar(lgn.ToChange)) {
            
            for (nbmod in 1:length(list.ToChange)) {
              if (substring(lgn.ToChange,nbletter,nbletter+(nchar(list.ToChange[nbmod])-1)) == list.ToChange[nbmod]) {
                lgn.changing   <-  paste(substring(lgn.ToChange,1,nbletter-1), list.changing[nbmod], substring(lgn.ToChange,nbletter+nchar(list.ToChange[nbmod]),nchar(lgn.ToChange)),sep="")
                file[nblgn,1]  <-  lgn.changing
                rm(lgn.changing)
              }
            };  rm(nbmod)

          };  rm(nbletter)
          
          rm(lgn.ToChange)
        };  rm(nblgn)
        
        path.file.ToSave  <-  paste(dir.changing, name.file, sep="/")
        write.table(file, path.file.ToSave, row.names = FALSE, col.names=FALSE, eol="\n", quote=FALSE)
        
        rm(file, name.file)
      };  rm(i)



      
      
##### SUPPRESSION/MODIFICATION #####
      
      setwd("C:/IngridROCHE/Pellicule")
      
      list.dirs       <-  dir()[which(!dir() %in% c(".jpeg", ".gif", ".mp4", ".MPG", ".mov"))]
      
      list.file       <-  dir()[which(dir() %in% c(".jpeg", ".gif", dir()[grep("Violet", dir())]))]
      
      dir()[1]
      file.mtime(dir()[1])
