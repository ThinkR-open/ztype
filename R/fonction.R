#' @title gen_set_of_words
#' @description extracts all function names from a given list of packages
#' @param packages character vector. package(s) to parse and extract function names from
#' @export
#' @importFrom stringr str_length
#' @return a character vector
#' @examples
#' \dontrun{
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_set_of_words()
#' }

gen_set_of_words<-function(packages){
  #sapply(packages,function(.){require(.,character.only=TRUE)})
  set_of_words <- unlist(sapply(packages,function(.){ls(paste0("package:",.))}))
  set_of_words[order(str_length(set_of_words))]
}

#' @title level
#' @description generates a collection of words to build a ZType game level
#' @param set_of_words a sorted vector of the collection of words to use
#' @param quantity an integer the number of words to pick in 
#' @param difficulte an integer reflecting the level's difficulty 
#' @export
#' @importFrom stats dpois
#' @examples
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_set_of_words() %>% level(10,50)


level <- function(set_of_words,quantity,difficulte){
  paste(sample(set_of_words,size=quantity,prob =  dpois(seq_along(set_of_words),difficulte),replace=TRUE),collapse=" ")
}

#' @title gen_set_of_levels
#' @description generate a set of levels with increasing difficulty
#' @param set_of_words a sorted vector of the collection of words to use
#' @param nb the number of levels to generate
#' @export
#' @importFrom stats dpois
#' @examples
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_set_of_words() %>% 
#' gen_set_of_levels(10) %>% cat()



gen_set_of_levels <- function(set_of_words,nb=25){
  p <- mapply(FUN = level,
              quantity=3:(nb+2),
              difficulte=seq(from=1,to=length(set_of_words),length.out =nb),
              set_of_words=list(set_of_words))
  paste(p,collapse="\n\n\n")
}


#' @title gen_game
#' @description generate a ZType game by puhsing a set of levels on the website.
#' @param set_of_levels the set of levels to use
#' @param open booleen open browser
#' @export
#' @importFrom  rvest html_session html_form set_values
#' @importFrom magrittr %>%
#' @importFrom utils browseURL
#' @examples
#' \dontrun{
#' require(ztype)
#' require(magrittr)
#' c("dplyr","ggplot2","lubridate") %>% gen_set_of_words() %>% 
#' gen_set_of_levels(10) %>% 
#' gen_game() %>% browseURL()
#' }
#'
gen_game <-function(set_of_levels,open=FALSE){
  
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
  
  filled_form <- set_values(pgform,`text` = set_of_levels)
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
    gen_set_of_words() %>% gen_set_of_levels(nb) %>% gen_game(open = TRUE)
  
}
