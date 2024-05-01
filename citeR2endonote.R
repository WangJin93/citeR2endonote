#' @title Export the references of R package to bibtex and endnote format files

#' @description Export the references of R package to bibtex and endnote format files

#' @param symbol

#' @return NULL

#' @examples citeR2endonote(c("shiny","drawProteins"),"ref_list",type = "endnote")

#' @export citeR2endonote
#' 
citeR2endonote <- function(pkg_list, filename, type = "endnote") {
  
  #ht to https://stackoverflow.com/questions/2470248/write-lines-of-text-to-a-file-in-r for sink()
  for (i in 1:length(pkg_list)) {
    if (type == "bibtex"){
      sink(file = paste(filename, ".bib", sep = ""), append = T)
      writeLines(toBibtex(citation(package = pkg_list[i])))
    }else{
      sink(file = paste(filename, ".enw", sep = ""), append = T)
      c <- citation(package = pkg_list[i])
      # cit <- toBibtex(c)
      for (j in 1:length(c)) {
        c1 <- c[[j]]
        
        for (k in 1:(length(c1$author))) {
          c1$author$given[k][[1]] <- paste(c1$author$given[k][[1]],collapse = " ")
        }
        auts <- paste0(c1$author$family,", ",c1$author$given)
        for (m in 1:(length(auts))) {
          if (substr(auts[m],1,2) == ", "){
            auts[m] <- substr(auts[m],3,nchar(auts[m]))
              
          }

        }
        
        authors <-paste0("%A ",  paste0(auts,collapse = "\n%A "))
        writeLines(paste0("%0 Journal Article\n",
                          authors,
                          "\n%+ ", c1$organization,
                          "\n%T ", c1$title,
                          "\n%J ", c1$journal,
                          "\n%D ", c1$year,
                          "\n%V ", c1$volumn,
                          "\n%N ", c1$number,
                          "\n%P ", c1$pages,
                          "\n%R ", c1$doi,
                          "\n%U ", c1$url,
                          "\n%Z ", pkg_list[i],
                          "\n"))
      }

    }
    sink()
  }
}
# citeR2endonote(c("shiny","RMySQL"),filename = "aa")
#devtools::install_github("WangJin93/citeR2endonote")

library(citeR2endonote)
citeR2endonote(c("shiny","bs4Dash","shinyWidgets","ggplot2","ggpubr","survminer","survival","dplyr","RMySQL","psych","aplot","ggtree"),"shiny")
