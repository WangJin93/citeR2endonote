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
      sink(file = paste(filename, ".txt", sep = ""), append = T)
      c <- citation(package = pkg_list[i])
      # cit <- toBibtex(c)
      auts <- paste0(c$author$family,", ",c$author$given)
      authors <-paste0("%A ",  paste0(auts,collapse = "\n%A "))

      writeLines(paste0("%0 Journal Article\n",
                        authors,
                        "\n%+ ", c$organization,
                        "\n%T ", c$title,
                        "\n%V ", c$journal,
                        "\n%D ", c$year,
                        "\n%V ", c$volumn,
                        "\n%N ", c$number,
                        "\n%P ", c$pages,
                        "\n%R ", c$doi,
                        "\n%U ", c$url))
    }
    sink()
  }
}
# citeR2endonote(c("shiny","RMySQL"),filename = "aa")
