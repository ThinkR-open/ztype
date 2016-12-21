#' @title gen_liste
#' @description extracts a list of all function names from a given list of packages
#' @param packages package(s) to parse and extract function names from
#' @export
#' @importFrom stringr str_length
#' @examples
#' \dontrun{
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_liste()
#' }

gen_liste<-function(packages){
  sapply(packages,function(.){require(.,character.only=TRUE)})
  liste <- unlist(sapply(packages,function(.){ls(paste0("package:",.))}))
  liste[order(str_length(liste))]
}

#' @title niveau
#' @description generates a collection of words to build a ZType game level
#' @param liste a vector of the collection of words to use
#' @param quantite an integer the number of words to pick in 
#' @param difficulte an integer reflecting the difficulty level
#' @export
#' @importFrom stats dpois
#' @examples
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_liste() %>% niveau(10,50)


niveau <- function(liste,quantite,difficulte){
  paste(sample(liste,size=quantite,prob =  dpois(seq_along(liste),difficulte),replace=TRUE),collapse=" ")
}

#' @title gen_paragraphe
#' @description generate a paragraph
#' @param liste the words to use
#' @param nb the number of level
#' @export
#' @importFrom stats dpois
#' @examples
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_liste() %>% 
#' gen_paragraphe(10) %>% cat()



gen_paragraphe <- function(liste,nb=25){
  p <- mapply(FUN = niveau,
              quantite=3:(nb+2),
              difficulte=seq(from=1,to=length(liste),length.out =nb),
              liste=list(liste))
  paste(p,collapse="\n\n\n")
}


#' @title gen_game
#' @description generate a ZType game 
#' @param text text to use
#' @param open booleen open browser
#' @export
#' @import rvest
#' @importFrom magrittr %>%
#' @importFrom utils browseURL
#' @examples
#' \dontrun{
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_liste() %>% 
#' gen_paragraphe(10) %>% 
#' gen_game() %>% browseURL()
#' }
#'
gen_game <-function(text,open=FALSE){
  
  pgsession <-html_session("http://zty.pe/?load")
  pgform    <-html_form(pgsession)[[1]]
  fake_submit_button <- list(name = NULL,
                             type = "submit",
                             value = NULL,
                             checked = NULL,
                             disabled = NULL,
                             readonly = NULL,
                             required = FALSE)
  attr(fake_submit_button, "class") <- "input"
  pgform[["fields"]][["submit"]] <- fake_submit_button
  
  filled_form <- set_values(pgform,`text` = text)
  submit_form(pgsession,filled_form)-> j
  j$response %>% unclass() %>%  .$url ->out
  
  
  if(open){
    message("opening : ",out)
    browseURL(out)}
  invisible(out)
  
}





#' @title ztype
#' @description launch a ZType game using function names of R packages
#' @param packages a vector containing installed packages from which extract function names
#' @param nb number of levels to design in the ZType game
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' require(ztype)
#' require(magrittr)
#' ztype()# dplyr, ggplot2 and lubridate
#' c("lubridate") %>% ztype()
#' }
#'
ztype <-function(packages=c("dplyr","ggplot2","lubridate"),nb=25){
  
  packages %>%
    gen_liste() %>% gen_paragraphe(nb) %>% gen_game(open = TRUE)
  
}
