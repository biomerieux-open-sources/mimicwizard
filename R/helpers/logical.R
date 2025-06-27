andcondition <- function(tibblea,tibbleb=NULL){
  if(is.null(tibbleb)) return(tibblea)
  if(!("stay_id" %in% names(tibblea)) | is.null(tibblea$stay_id[1])){
    tibblea %>% select(!c(stay_id)) %>%
      inner_join(tibbleb, by = c("subject_id","hadm_id"))
  } else if(!("stay_id" %in% names(tibbleb)) | is.null(tibbleb$stay_id[1])){
    tibbleb %>% select(!c(stay_id)) %>%
      inner_join(tibblea, by = c("subject_id","hadm_id"))
  } else{
    inner_join(tibblea, tibbleb, by = c("subject_id","hadm_id","stay_id"))
  }
}

orcondition <- function(tibblea,tibbleb=NULL){
  if(is.null(tibbleb)) return(tibblea)

  if(!("stay_id" %in% names(tibblea)) | is.null(tibblea$stay_id[1])){
    tibblea %>% select(!c(stay_id)) %>%
      full_join(tibbleb, by = c("subject_id","hadm_id"))
    print()
  } else if(!("stay_id" %in% names(tibbleb)) | is.null(tibbleb$stay_id[1])){
    tibbleb %>% select(!c(stay_id)) %>%
      full_join(tibblea, by = c("subject_id","hadm_id"))
  } else{
    tibblea %>% full_join(tibbleb, by = c("subject_id","hadm_id","stay_id"))
  }
}

parsecondition <- function(string){
  cursor <- 1
  while (cursor < nchar(string)) {
    if(substr(string,cursor,cursor) == " "){
      cursor <- cursor + 1
    }
    else if(substr(string,cursor,cursor) == "("){
      linked_closing_bck_idx <-
        regexpr(
          "\\([^)(]*+(?:(?R)[^)(]*)*+\\)",
          substr(string, cursor, nchar(string)) ,
          perl = T
        )
      closing_parenthesis <- linked_closing_bck_idx[1] + attributes(linked_closing_bck_idx)$match.length
      left_string <- parsecondition(substr(string, cursor+1, closing_parenthesis-1))
      print(paste0("current full string :",string))
      print(paste0("left_string (not parsed) :",substr(string, cursor+1, closing_parenthesis-1)))
      right_string <- substr(string, closing_parenthesis+1, nchar(string))
      print(paste0("right_string :",right_string))
      cursor <- nchar(left_string)
      string <- paste0(left_string,right_string)
    }
    else if(substr(string,cursor,cursor) == "&"){
      return(paste0("andcondition[",parsecondition(substr(string, 1, cursor-1)),",",parsecondition(substr(string, cursor+1, nchar(string))),"]"))
    }
    else if(substr(string,cursor,cursor) == "|"){
      return(paste0("orcondition[",parsecondition(substr(string, 1, cursor-1)),",",parsecondition(substr(string, cursor+1, nchar(string))),"]"))
    }
    else{
      #is a variable name
      cursor <- cursor + 1
    }
  }
  return(string)
}

truncate_string <- function(input_string,n=200) {
  if (nchar(input_string) > n) {
    return(paste0(substr(input_string, 1, n), "..."))
  } else {
    return(input_string)
  }
}


