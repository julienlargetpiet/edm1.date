#' format_date
#'
#' Allow to convert xx-month-xxxx date type to xx-xx-xxxx
#' @param f_dialect are the months from the language of which the month come
#' @param sentc is the date to convert
#' @param sep_in is the separator of the dat input (default is "-")
#' @param sep_out is the separator of the converted date (default is "-")
#' @examples
#'
#' print(format_date(f_dialect=c("janvier", "février", "mars", "avril", "mai", "juin",
#' "juillet", "aout", "septembre", "octobre", "novembre", "décembre"), sentc="11-septembre-2023"))
#'
#' #[1] "11-09-2023"
#'
#' @export

format_date <- function(f_dialect, sentc, sep_in="-", sep_out="-"){
  
  traduct <- f_dialect
  
  x <- unlist(str_split(sentc, sep_in))
  
  x2 <- match(x[2], f_dialect)
  
  x2 <- as.character(x2)

  if (nchar(x2) == 1){

    x2 <- paste0("0", x2)
  
  }

  x <- paste0(x[1], sep_out, x2, sep_out, x[3]) 
  
  return(x)
  
}

#' leap_year 
#'
#' Get if the year is leap
#'
#' @param year is the input year
#' 
#' @examples
#'
#' print(leap_yr(year=2024))
#' 
#' #[1] TRUE
#'
#' @export

leap_yr <- function(year){

  if (year == 0){ return(FALSE) }

  if (year %% 4 == 0){
    
    if (year %% 100 == 0){
      
      if (year %% 400 == 0){
        
        bsx <- TRUE
        
      }else{
        
        bsx <- FALSE
        
      }
      
    }else{
      
      bsx <- TRUE
      
    }
    
  }else{
    
    bsx <- FALSE
    
  }

  return(bsx)

}

#' converter_date
#' 
#' Allow to convert any date like second/minute/hour/day/month/year to either second, minute...year. The input date should not necessarily have all its time units (second, minute...) but all the time units according to a format. Example: "snhdmy" is for second, hour, minute, day, month, year. And "mdy" is for month, day, year.
#'
#' @param inpt_date is the input date
#' @param convert_to is the time unit the input date will be converted ("s", "n", "h", "d", "m", "y")
#' @param frmt is the format of the input date
#' @param sep_ is the separator of the input date. For example this input date "12-07-2012" has "-" as a separator
#' @examples 
#'
#' print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="m"))
#' 
#' #[1] 24299.15
#' 
#' print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="y"))
#' 
#' #[1] 2024.929
#'
#' print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="s"))
#' 
#' #[1] 63900626400
#'
#' print(converter_date(inpt_date="63900626400", sep_="-", frmt="s", convert_to="y"))
#'
#' #[1] 2024.929
#'
#' print(converter_date(inpt_date="2024", sep_="-", frmt="y", convert_to="s"))
#'
#' #[1] 63873964800
#' 
#' @export

converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

  is_divisible <- function(inpt_v=c(), divisible_v=c()){

        cnt = 1

        while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                cnt = cnt + 1

        }

        return(inpt_v)

  }

  leap_yr <- function(year){

          if (year == 0){ return(FALSE) }

          if (year %% 4 == 0){
            
            if (year %% 100 == 0){
              
              if (year %% 400 == 0){
                
                bsx <- TRUE
                
              }else{
                
                bsx <- FALSE
                
              }
              
            }else{
              
              bsx <- TRUE
              
            }
            
          }else{
            
            bsx <- FALSE
            
          }

          return(bsx)

  }

  inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

  stay_date_v <- c("s", "n", "h", "d", "m", "y")

  stay_date_val <- c(0, 0, 0, 0, 0, 0)

  frmt <- unlist(strsplit(x=frmt, split=""))

  for (el in 1:length(frmt)){

          stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

  }

  l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
            30, 31, 30, 31)

  l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
            30, 31, 30, 31)

  if (!(leap_yr(year=stay_date_val[6]))){

        l_dm <- l_dm1

  }else if (stay_date_val[6] == 0){

        l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

  }else{

        l_dm <- l_dm2

  }

  may_bsx_v <- c(1:stay_date_val[6])

  may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

  may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

  val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

  day_val = 0

  for (dt in length(stay_date_val):1){

        day_val = day_val + stay_date_val[dt] * val_mult[dt] 

  }

  day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

  if (str_detect(string=stay_date_val[5], pattern="\\.")){

          all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

          int_part <- all_part[1]

          if (int_part != 0){

                day_val = day_val + sum(l_dm[1:int_part])

          }else{ int_part <- 1 }

          day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

  }else if (stay_date_val[5] != 0){

        day_val = day_val + sum(l_dm1[1:stay_date_val[5]]) 

  }

  val_mult2 <- c(60, 60, 24, 1)

  idx_convert <- grep(pattern=convert_to, x=stay_date_v)

  if (idx_convert < 5){

        for (i in 4:idx_convert){

            day_val = day_val * val_mult2[i]

        }

        return(day_val)

  }else{

    year = 0

    l_dm <- l_dm1

    month = 0

    bsx_cnt = 0

    while ((day_val / sum(l_dm)) >= 1 ){

        l_dmb <- l_dm

        day_val2 = day_val

        day_val = day_val - sum(l_dm)

        month = month + 12

        year = year + 1

        if (!(leap_yr(year=year))){

                l_dm <- l_dm1

        }else{

                bsx_cnt = bsx_cnt + 1

                l_dm <- l_dm2

        } 

    }

    if (leap_yr(year=year)){

        day_val = day_val - 1

    }

    cnt = 1

    while ((day_val / l_dm[cnt]) >= 1){

        day_val = day_val - l_dm[cnt]

        month = month + 1

        cnt = cnt + 1 

    }

    month = month + (day_val / l_dm[cnt])

    if (convert_to == "m"){

            return(month)

    }else{

            year = year + ((month - 12 * year) / 12)

            return(year)

    }

  }

}

#' date_converter_reverse
#'
#' Allow to convert single date value like 2025.36 year to a date like second/minutehour/day/month/year (snhdmy)
#' 
#' @param inpt_date is the input date 
#' @param convert_to is the date format the input date will be converted
#' @param frmt is the time unit of the input date
#' @param sep_ is the separator of the outputed date
#' @examples
#'
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="hmy", frmt="y", sep_="-"))
#' 
#' #[1] "110-11-2024"
#' 
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="dmy", frmt="y", sep_="-"))
#'
#' #[1] "4-11-2024"
#' 
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="hdmy", frmt="y", sep_="-"))
#'
#' #[1] "14-4-11-2024"
#' 
#' print(date_converter_reverse(inpt_date="2024.929", convert_to="dhym", frmt="y", sep_="-"))
#' 
#' #[1] "4-14-2024-11"
#'
#' @export

date_converter_reverse <- function(inpt_date, convert_to="dmy", frmt="y", sep_="-"){
 
        converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

          is_divisible <- function(inpt_v=c(), divisible_v=c()){

                cnt = 1

                while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                        inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                        cnt = cnt + 1

                }

                return(inpt_v)

          }

          leap_yr <- function(year){

                  if (year == 0){ return(FALSE) }

                  if (year %% 4 == 0){
                    
                    if (year %% 100 == 0){
                      
                      if (year %% 400 == 0){
                        
                        bsx <- TRUE
                        
                      }else{
                        
                        bsx <- FALSE
                        
                      }
                      
                    }else{
                      
                      bsx <- TRUE
                      
                    }
                    
                  }else{
                    
                    bsx <- FALSE
                    
                  }

                  return(bsx)

          }

          inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

          stay_date_v <- c("s", "n", "h", "d", "m", "y")

          stay_date_val <- c(0, 0, 0, 0, 0, 0)

          frmt <- unlist(strsplit(x=frmt, split=""))

          for (el in 1:length(frmt)){

                  stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

          }

          l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          if (!(leap_yr(year=stay_date_val[6]))){

                l_dm <- l_dm1

          }else if (stay_date_val[6] == 0){

                l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

          }else{

                l_dm <- l_dm2

          }

          may_bsx_v <- c(1:stay_date_val[6])

          may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

          may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

          val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

          day_val = 0

          for (dt in length(stay_date_val):1){

                day_val = day_val + stay_date_val[dt] * val_mult[dt] 

          }

          day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

          if (str_detect(string=stay_date_val[5], pattern="\\.")){

                  all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

                  int_part <- all_part[1]

                  if (int_part != 0){

                        day_val = day_val + sum(l_dm[1:int_part])

                  }else{ int_part <- 1 }

                  day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

          }else if (stay_date_val[5] != 0){

                day_val = day_val + sum(l_dm1[1:stay_date_val[5]]) 

          }

          val_mult2 <- c(60, 60, 24, 1)

          idx_convert <- grep(pattern=convert_to, x=stay_date_v)

          if (idx_convert < 5){

                for (i in 4:idx_convert){

                    day_val = day_val * val_mult2[i]

                }

                return(day_val)

          }else{

            year = 0

            l_dm <- l_dm1

            month = 0

            bsx_cnt = 0

            while ((day_val / sum(l_dm)) >= 1 ){

                l_dmb <- l_dm

                day_val2 = day_val

                day_val = day_val - sum(l_dm)

                month = month + 12

                year = year + 1

                if (!(leap_yr(year=year))){

                        l_dm <- l_dm1

                }else{

                        bsx_cnt = bsx_cnt + 1

                        l_dm <- l_dm2

                } 

            }

            if (leap_yr(year=year)){

                day_val = day_val - 1

            }

            cnt = 1

            while ((day_val / l_dm[cnt]) >= 1){

                day_val = day_val - l_dm[cnt]

                month = month + 1

                cnt = cnt + 1 

            }

            month = month + (day_val / l_dm[cnt])

            if (convert_to == "m"){

                    return(month)

            }else{

                    year = year + ((month - 12 * year) / 12)

                    return(year)

            }

          }

        }

        date_symb <- c("s", "n", "h", "d", "m", "y")

        date_val <- c(0, 0, 0, 0, 0, 0)

        convert_to_v <- unlist(strsplit(x=convert_to, split=""))

        pre_v <- c()

        for (el in convert_to_v){

            pre_v <- c(pre_v, grep(pattern=el, x=date_symb))

        }

        pre_v2 <- sort(x=pre_v, decreasing=TRUE)

        cvrt_v <- date_symb[pre_v2]

        calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[1], frmt=frmt)) 

        if (str_detect(pattern="e", string=calcd)){ calcd <- "0.0001" }

        pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

        inpt_date <- paste("0.", pre_str[2], sep="")

        date_val[pre_v2[1]] <- pre_str[1]

        if (length(convert_to_v) > 1){

            for (el in 1:(length(cvrt_v) - 1)){

                calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[el+1], frmt=cvrt_v[el]))
               
                pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

                inpt_date <- paste("0.", pre_str[2], sep="")

                date_val[pre_v2[el+1]] <- pre_str[1]

            }

        }

    return(paste(date_val[pre_v], collapse=sep_))

}

#' converter_format
#'
#' Allow to convert a format to another
#'
#' @param inpt_val is the input value that is linked to the format
#' @param inpt_frmt is the format of the input value
#' @param sep_ is the separator of the value in inpt_val
#' @param frmt is the format you want to convert to
#' @param default_val is the default value given to the units that are not present in the input format
#' @examples
#'
#' print(converter_format(inpt_val="23-12-05-1567", sep_="-", 
#'                        inpt_frmt="shmy", frmt="snhdmy", default_val="00"))
#'
#' #[1] "23-00-12-00-05-1567"
#'
#' print(converter_format(inpt_val="23-12-05-1567", sep_="-", 
#'                        inpt_frmt="shmy", frmt="Pnhdmy", default_val="00"))
#' 
#' #[1] "00-00-12-00-05-1567"
#'
#' @export

converter_format <- function(inpt_val, sep_="-", inpt_frmt, 
                             frmt, default_val="00"){

        frmt <- unlist(strsplit(x=frmt, split=""))

        inpt_frmt <- unlist(strsplit(x=inpt_frmt, split=""))
        
        inpt_val <- unlist(strsplit(x=inpt_val, split=sep_))

        val_v <- c()

        for (i in 1:length(frmt)){

                val_v <- c(val_v, default_val)

        }

        for (el in 1:length(inpt_val)){

                pre_grep <- grep(x=frmt, pattern=inpt_frmt[el])

                if (!identical(pre_grep, integer(0))){

                        val_v[pre_grep] <- inpt_val[el]

                }

        }

        return(paste(val_v, collapse=sep_))

}

#' date_addr
#' 
#' Allow to add or substract two dates that have the same time unit or not
#'
#' @param date1 is the date from which the second date will be added or substracted
#' @param date2 is the date that will be added or will substract date1
#' @param add equals to FALSE if you want date1 - date2 and TRUE if you want date1 + date2
#' @param frmt1 is the format of date1 (snhdmy) (second, minute, hour, day, monthn year)
#' @param frmt2 is the format of date2 (snhdmy)
#' @param sep_ is the separator of date1 and date2
#' @param convert_to is the format of the outputed date
#' @examples
#'
#' print(date_addr(date1="25-02", date2="58-12-08", frmt1="dm", frmt2="shd", sep_="-", 
#'                 convert_to="dmy"))
#'
#' #[1] "18-2-0"
#'
#' print(date_addr(date1="25-02", date2="58-12-08", frmt1="dm", frmt2="shd", sep_="-", 
#'                 convert_to="dmy", add=TRUE))
#'
#' #[1] "3-3-0"
#'
#' print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-", 
#'                 convert_to="dmy", add=TRUE))
#'
#' #[1] "27-3-2024"
#'
#' print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-", 
#'                 convert_to="dmy", add=FALSE))
#' 
#' #[1] "23-1-2024" 
#'
#' print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-", 
#'                  convert_to="n", add=FALSE))
#'
#' #[1] "1064596320"
#' 
#' print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-", 
#'                  convert_to="s", add=FALSE))
#'
#' #[1] "63875779200"
#'
#' @export


date_addr <- function(date1, date2, add=FALSE, frmt1, frmt2=frmt1, sep_="-", convert_to="dmy"){
 
        converter_format <- function(inpt_val, sep_="-", inpt_frmt, 
                             frmt, default_val="00"){

                frmt <- unlist(strsplit(x=frmt, split=""))

                inpt_frmt <- unlist(strsplit(x=inpt_frmt, split=""))
                
                inpt_val <- unlist(strsplit(x=inpt_val, split=sep_))

                val_v <- c()

                for (i in 1:length(frmt)){

                        val_v <- c(val_v, default_val)

                }

                for (el in 1:length(inpt_val)){

                        pre_grep <- grep(x=frmt, pattern=inpt_frmt[el])

                        if (!identical(pre_grep, integer(0))){

                                val_v[pre_grep] <- inpt_val[el]

                        }

                }

                return(paste(val_v, collapse=sep_))

        }

        converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

          is_divisible <- function(inpt_v=c(), divisible_v=c()){

                cnt = 1

                while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                        inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                        cnt = cnt + 1

                }

                return(inpt_v)

          }

          leap_yr <- function(year){

                  if (year == 0){ return(FALSE) }

                  if (year %% 4 == 0){
                    
                    if (year %% 100 == 0){
                      
                      if (year %% 400 == 0){
                        
                        bsx <- TRUE
                        
                      }else{
                        
                        bsx <- FALSE
                        
                      }
                      
                    }else{
                      
                      bsx <- TRUE
                      
                    }
                    
                  }else{
                    
                    bsx <- FALSE
                    
                  }

                  return(bsx)

          }

          inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

          stay_date_v <- c("s", "n", "h", "d", "m", "y")

          stay_date_val <- c(0, 0, 0, 0, 0, 0)

          frmt <- unlist(strsplit(x=frmt, split=""))

          for (el in 1:length(frmt)){

                  stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

          }

          l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
                    30, 31, 30, 31)

          if (!(leap_yr(year=stay_date_val[6]))){

                l_dm <- l_dm1

          }else if (stay_date_val[6] == 0){

                l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

          }else{

                l_dm <- l_dm2

          }

          may_bsx_v <- c(1:stay_date_val[6])

          may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

          may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

          val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

          day_val = 0

          for (dt in length(stay_date_val):1){

                day_val = day_val + stay_date_val[dt] * val_mult[dt] 

          }

          day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

          if (str_detect(string=stay_date_val[5], pattern="\\.")){

                  all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

                  int_part <- all_part[1]

                  if (int_part != 0){

                        day_val = day_val + sum(l_dm[1:int_part])

                  }else{ int_part <- 1 }

                  day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

          }else if (stay_date_val[5] != 0){

                day_val = day_val + sum(l_dm1[1:stay_date_val[5]]) 

          }

          val_mult2 <- c(60, 60, 24, 1)

          idx_convert <- grep(pattern=convert_to, x=stay_date_v)

          if (idx_convert < 5){

                for (i in 4:idx_convert){

                    day_val = day_val * val_mult2[i]

                }

                return(day_val)

          }else{

            year = 0

            l_dm <- l_dm1

            month = 0

            bsx_cnt = 0

            while ((day_val / sum(l_dm)) >= 1 ){

                l_dmb <- l_dm

                day_val2 = day_val

                day_val = day_val - sum(l_dm)

                month = month + 12

                year = year + 1

                if (!(leap_yr(year=year))){

                        l_dm <- l_dm1

                }else{

                        bsx_cnt = bsx_cnt + 1

                        l_dm <- l_dm2

                } 

            }

            if (leap_yr(year=year)){

                day_val = day_val - 1

            }

            cnt = 1

            while ((day_val / l_dm[cnt]) >= 1){

                day_val = day_val - l_dm[cnt]

                month = month + 1

                cnt = cnt + 1 

            }

            month = month + (day_val / l_dm[cnt])

            if (convert_to == "m"){

                    return(month)

            }else{

                    year = year + ((month - 12 * year) / 12)

                    return(year)

            }

          }

        }

        date_converter_reverse <- function(inpt_date, convert_to="dmy", frmt="y", sep_="-"){
 
                converter_date <- function(inpt_date, convert_to, frmt="snhdmy", sep_="-"){

                  is_divisible <- function(inpt_v=c(), divisible_v=c()){

                        cnt = 1

                        while (length(inpt_v) > 0 & cnt < (length(divisible_v) + 1)){

                                inpt_v <- inpt_v[(inpt_v %% divisible_v[cnt]) == 0]

                                cnt = cnt + 1

                        }

                        return(inpt_v)

                  }

                  leap_yr <- function(year){

                          if (year == 0){ return(FALSE) }

                          if (year %% 4 == 0){
                            
                            if (year %% 100 == 0){
                              
                              if (year %% 400 == 0){
                                
                                bsx <- TRUE
                                
                              }else{
                                
                                bsx <- FALSE
                                
                              }
                              
                            }else{
                              
                              bsx <- TRUE
                              
                            }
                            
                          }else{
                            
                            bsx <- FALSE
                            
                          }

                          return(bsx)

                  }

                  inpt_date <- unlist(strsplit(x=inpt_date, split=sep_)) 

                  stay_date_v <- c("s", "n", "h", "d", "m", "y")

                  stay_date_val <- c(0, 0, 0, 0, 0, 0)

                  frmt <- unlist(strsplit(x=frmt, split=""))

                  for (el in 1:length(frmt)){

                          stay_date_val[grep(pattern=frmt[el], x=stay_date_v)] <- as.numeric(inpt_date[el]) 

                  }

                  l_dm1 <- c(31, 28, 31, 30, 31, 30, 31, 31,
                            30, 31, 30, 31)

                  l_dm2 <- c(31, 29, 31, 30, 31, 30, 31, 31,
                            30, 31, 30, 31)

                  if (!(leap_yr(year=stay_date_val[6]))){

                        l_dm <- l_dm1

                  }else if (stay_date_val[6] == 0){

                        l_dm <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

                  }else{

                        l_dm <- l_dm2

                  }

                  may_bsx_v <- c(1:stay_date_val[6])

                  may_bsx_v <- may_bsx_v[may_bsx_v > 0] 

                  may_bsx_v <- may_bsx_v[(may_bsx_v %% 4) == 0]

                  val_mult <- c((1 / 86400), (1 / 1440), (1/24), 1, 0, 365)

                  day_val = 0

                  for (dt in length(stay_date_val):1){

                        day_val = day_val + stay_date_val[dt] * val_mult[dt] 

                  }

                  day_val = day_val + length(may_bsx_v[(may_bsx_v %% 100) != 0]) + length(is_divisible(inpt_v=may_bsx_v, divisible_v=c(100, 400))) 

                  if (str_detect(string=stay_date_val[5], pattern="\\.")){

                          all_part <- as.numeric(unlist(strsplit(x=as.character(stay_date_val[5]), split="\\.")))

                          int_part <- all_part[1]

                          if (int_part != 0){

                                day_val = day_val + sum(l_dm[1:int_part])

                          }else{ int_part <- 1 }

                          day_val = day_val + l_dm1[int_part] * as.numeric(paste("0.", all_part[2], sep=""))

                  }else if (stay_date_val[5] != 0){

                        day_val = day_val + sum(l_dm1[1:stay_date_val[5]]) 

                  }

                  val_mult2 <- c(60, 60, 24, 1)

                  idx_convert <- grep(pattern=convert_to, x=stay_date_v)

                  if (idx_convert < 5){

                        for (i in 4:idx_convert){

                            day_val = day_val * val_mult2[i]

                        }

                        return(day_val)

                  }else{

                    year = 0

                    l_dm <- l_dm1

                    month = 0

                    bsx_cnt = 0

                    while ((day_val / sum(l_dm)) >= 1 ){

                        l_dmb <- l_dm

                        day_val2 = day_val

                        day_val = day_val - sum(l_dm)

                        month = month + 12

                        year = year + 1

                        if (!(leap_yr(year=year))){

                                l_dm <- l_dm1

                        }else{

                                bsx_cnt = bsx_cnt + 1

                                l_dm <- l_dm2

                        } 

                    }

                    if (leap_yr(year=year)){

                        day_val = day_val - 1

                    }

                    cnt = 1

                    while ((day_val / l_dm[cnt]) >= 1){

                        day_val = day_val - l_dm[cnt]

                        month = month + 1

                        cnt = cnt + 1 

                    }

                    month = month + (day_val / l_dm[cnt])

                    if (convert_to == "m"){

                            return(month)

                    }else{

                            year = year + ((month - 12 * year) / 12)

                            return(year)

                    }

                  }

                }

                date_symb <- c("s", "n", "h", "d", "m", "y")

                date_val <- c(0, 0, 0, 0, 0, 0)

                convert_to_v <- unlist(strsplit(x=convert_to, split=""))

                pre_v <- c()

                for (el in convert_to_v){

                    pre_v <- c(pre_v, grep(pattern=el, x=date_symb))

                }

                pre_v2 <- sort(x=pre_v, decreasing=TRUE)

                cvrt_v <- date_symb[pre_v2]

                calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[1], frmt=frmt)) 

                if (str_detect(pattern="e", string=calcd)){ calcd <- "0.0001" }

                pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

                inpt_date <- paste("0.", pre_str[2], sep="")

                date_val[pre_v2[1]] <- pre_str[1]

                if (length(convert_to_v) > 1){

                    for (el in 1:(length(cvrt_v) - 1)){

                        calcd <- as.character(converter_date(inpt_date=as.character(inpt_date), convert_to=cvrt_v[el+1], frmt=cvrt_v[el]))
                       
                        pre_str <- as.numeric(unlist(strsplit(x=calcd, split="\\.")))

                        inpt_date <- paste("0.", pre_str[2], sep="")

                        date_val[pre_v2[el+1]] <- pre_str[1]

                    }

                }

            return(paste(date_val[pre_v], collapse=sep_))

        }

        ptrn_twkr <- function(inpt_l, depth="max", sep="-", 
                              default_val="0", add_sep=TRUE, end_=TRUE){
          
          ln <- length(inpt_l)
          
          if (depth == "min"){
            
            pre_val <- str_count(inpt_l[1], sep)
            
            for (i in 2:ln){
              
              if (str_count(inpt_l[i], sep) < pre_val){
                
                pre_val <- str_count(inpt_l[i], sep)
                
              }
              
            }
            
            depth <- pre_val
            
          }

          if (depth == "max"){
            
            pre_val <- str_count(inpt_l[1], sep)
            
            for (i in 2:ln){
              
              if (str_count(inpt_l[i], sep) > pre_val){
                
                pre_val <- str_count(inpt_l[i], sep)
                
              }
              
            }
            
            depth <- pre_val
            
          }

          if (end_){

                  for (I in 1:ln){
                   
                    hmn <- str_count(inpt_l[I], "-")
                    
                    if (hmn < depth){
                     
                      inpt_l[I] <- paste0(inpt_l[I], sep, default_val)

                      diff <- depth - hmn - 1

                      if (diff > 0){
                      
                                if (add_sep){
                                  
                                  for (i in 1:diff){
                                  
                                    inpt_l[I] <- paste0(inpt_l[I], sep, default_val)
                                  
                                  }
                                
                                }else{
                                  
                                  for (i in 1:diff){
                                    
                                    inpt_l[I] <- paste0(inpt_l[I], default_val)
                                    
                                  }
                                  
                                }

                     }
                    
                    }else if(depth < hmn){

                        if (add_sep){

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                        }else{

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
                       
                        }

                    }

                  }
          
          }else{

                for (I in 1:ln){
                   
                    hmn <- str_count(inpt_l[I], "-")
                    
                    if (hmn < depth){
                     
                      inpt_l[I] <- paste0(default_val, sep, inpt_l[I])

                      diff <- depth - hmn - 1

                      if (diff > 0){
                      
                                if (add_sep){
                                  
                                  for (i in 1:diff){
                                  
                                    inpt_l[I] <- paste0(default_val, sep, inpt_l[I])
                                  
                                  }
                                
                                }else{
                                  
                                  for (i in 1:diff){
                                    
                                    inpt_l[I] <- paste0(default_val, inpt_l[I])
                                    
                                  }
                                  
                                }

                     }
                    
                    }else if(depth < hmn){

                        if (add_sep){

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                        }else{

                                inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
                       
                        }

                    }

                  }

          }

          return(inpt_l)
          
        }

        date_symb <- c("s", "n", "h", "d", "m", "y")

        pre_v <- c()

        for (el in unlist(strsplit(x=frmt1, split=""))){

                pre_v <- c(pre_v, grep(x=date_symb, pattern=el))

        }

        min1 <- min(pre_v)

        pre_v <- c()

        for (el in unlist(strsplit(x=frmt2, split=""))){

                pre_v <- c(pre_v, grep(x=date_symb, pattern=el))

        }

        min2 <- min(pre_v)

        if (min1 >= min2){

                min_ <- min2 

        }

        if (min1 < min2){

                min_ <- min1

        }

        date1 <- converter_format(inpt_val=date1, inpt_frmt=frmt1, frmt="snhdmy", sep_=sep_) 

        date2 <- converter_format(inpt_val=date2, inpt_frmt=frmt2, frmt="snhdmy", sep_=sep_) 

        date1 <- converter_date(inpt_date=date1, frmt="snhdmy", convert_to=date_symb[min_], sep_=sep_)

        date2 <- converter_date(inpt_date=date2, frmt="snhdmy", convert_to=date_symb[min_], sep_=sep_)

        if (add){

                datef <- date1 + date2

        }else{

                datef <- date1 - date2

        }

        return(date_converter_reverse(inpt_date=datef, frmt=date_symb[min_], 
                                      convert_to=convert_to, sep_=sep_))

}

#' sort_date
#'
#' Allow to sort any vector containing a date, from any kind of format (my, hdmy, ymd ...), see examples.
#'
#' @param inpt_v is the input vector containing all the dates
#' @param frmt is the format  of the dates, (any combinaison of letters "s" for second, "n", for minute, "h" for hour, "d" for day, "m" for month and "y" for year)
#' @param sep_ is the separator used for the dates
#' @param ascending is the used to sort the dates
#' @param give takes only two values "index" or "value", if give == "index", the function will output the index of sorted dates from inpt_v, if give == "value", the function will output the value, it means directly the sorted dates in inpt_v, see examples
#' @examples
#'
#' print(sort_date(inpt_v = c("01-11-2025", "08-08-1922", "12-04-1966")
#'                 , frmt = "dmy", sep_ = "-", ascending = TRUE, give = "value"))
#' 
#' [1] "08-08-1922" "12-04-1966" "01-11-2025"
#'
#' print(sort_date(inpt_v = c("01-11-2025", "08-08-1922", "12-04-1966")
#'                 , frmt = "dmy", sep_ = "-", ascending = FALSE, give = "value"))
#' 
#' [1] "01-11-2025" "12-04-1966" "08-08-1922"
#'
#' print(sort_date(inpt_v = c("01-11-2025", "08-08-1922", "12-04-1966")
#'                 , frmt = "dmy", sep_ = "-", ascending = TRUE, give = "index"))
#'
#' [1] 2 3 1
#'
#' print(sort_date(inpt_v = c("22-01-11-2025", "11-12-04-1966", "12-12-04-1966")
#'                 , frmt = "hdmy", sep_ = "-", ascending = FALSE, give = "value"))
#'
#' [1] "22-01-11-2025" "12-12-04-1966" "11-12-04-1966"
#'
#' print(sort_date(inpt_v = c("03-22-01-11-2025", "56-11-12-04-1966", "23-12-12-04-1966")
#'                 , frmt = "nhdmy", sep_ = "-", ascending = FALSE, give = "value"))
#'
#' [1] "03-22-01-11-2025" "23-12-12-04-1966" "56-11-12-04-1966"
#'
#' @export

sort_date <- function(inpt_v, frmt, sep_ = "-", ascending = FALSE, give = "value"){
  test_order <- function(inpt_v_from, inpt_v_test){
    lst_idx <- match(x = inpt_v_test[1], table = inpt_v_from)
    if (length(inpt_v_test) > 1){
      for (i in inpt_v_test){
        tst_idx <- match(x = i, table = inpt_v_from)
        if (tst_idx < lst_idx){
          return(FALSE)
        }
        lst_idx <- tst_idx
      }
    }
    return(TRUE)
  }
  converter_format <- function(inpt_val, sep_="-", inpt_frmt, 
                         frmt, default_val="00"){
    frmt <- unlist(strsplit(x=frmt, split=""))
    inpt_frmt <- unlist(strsplit(x=inpt_frmt, split=""))
    inpt_val <- unlist(strsplit(x=inpt_val, split=sep_))
    val_v <- c()
    for (i in 1:length(frmt)){
            val_v <- c(val_v, default_val)
    }
    for (el in 1:length(inpt_val)){
            pre_grep <- grep(x=frmt, pattern=inpt_frmt[el])
            if (!identical(pre_grep, integer(0))){
                    val_v[pre_grep] <- inpt_val[el]
            }
    }
    return(paste(val_v, collapse=sep_))
  }
  func <- function(){
    return(gsub(x = converter_format(inpt_val = inpt_val, sep_ = sep_, 
                inpt_frmt = inpt_frmt, frmt = frmt), 
                pattern = sep_, replacement = ""))
  }
  frmt_frm <- c("y", "m", "d", "h", "n", "s")
  if (!(test_order(inpt_v_from = frmt_frm, 
                   inpt_v_test = unlist(strsplit(x = frmt, split = ""))))){
    frmt_to <- frmt_frm[sort(match(x = unlist(strsplit(x = frmt, split = "")), table = frmt_frm))]
    new_v <- as.numeric(mapply(function(x) return(gsub(x = converter_format(inpt_val = x, 
                                                                sep_ = sep_, 
                                                                inpt_frmt = frmt, 
                                                                frmt = frmt_to), 
                                          replacement = "",
                                          pattern = sep_)), 
                   inpt_v))
  }else if (sep_ != ""){
    new_v <- as.numeric(mapply(function(x) return(gsub(x = x, pattern = sep_, replacement = "")), inpt_v))
  }else{
    new_v <- as.numeric(inpt_v)
  }
  if (give == "index"){
    return(match(x = sort(x = new_v, decreasing = !(ascending)), table = new_v))
  }else if (give == "value"){
    return(inpt_v[match(x = sort(x = new_v, decreasing = !(ascending)), table = new_v)])
  }else{
    return(NULL)
  }
}

