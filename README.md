![](logo.png)

# Install

-> git clone https://github.com/julienlargetpiet/edm1.date

-> cd edm1

edm1 > R

R > library("devtools")

R > build()

R > install()

# `converter_date`

converter_date


## Description

Allow to convert any date like second/minute/hour/day/month/year to either second, minute...year. The input date should not necessarily have all its time units (second, minute...) but all the time units according to a format. Example: "snhdmy" is for second, hour, minute, day, month, year. And "mdy" is for month, day, year.


## Usage

```r
converter_date(inpt_date, convert_to, frmt = "snhdmy", sep_ = "-")
```


## Arguments

Argument      |Description
------------- |----------------
`inpt_date`     |     is the input date
`convert_to`     |     is the time unit the input date will be converted ("s", "n", "h", "d", "m", "y")
`frmt`     |     is the format of the input date
`sep_`     |     is the separator of the input date. For example this input date "12-07-2012" has "-" as a separator


## Examples

```r
print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="m"))

#[1] 24299.15

print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="y"))

#[1] 2024.929

print(converter_date(inpt_date="14-04-11-2024", sep_="-", frmt="hdmy", convert_to="s"))

#[1] 63900626400

print(converter_date(inpt_date="63900626400", sep_="-", frmt="s", convert_to="y"))

#[1] 2024.929

print(converter_date(inpt_date="2024", sep_="-", frmt="y", convert_to="s"))

#[1] 63873964800
```


# `converter_format`

converter_format


## Description

Allow to convert a format to another


## Usage

```r
converter_format(inpt_val, sep_ = "-", inpt_frmt, frmt, default_val = "00")
```


## Arguments

Argument      |Description
------------- |----------------
`inpt_val`     |     is the input value that is linked to the format
`sep_`     |     is the separator of the value in inpt_val
`inpt_frmt`     |     is the format of the input value
`frmt`     |     is the format you want to convert to
`default_val`     |     is the default value given to the units that are not present in the input format


## Examples

```r
print(converter_format(inpt_val="23-12-05-1567", sep_="-",
inpt_frmt="shmy", frmt="snhdmy", default_val="00"))

#[1] "23-00-12-00-05-1567"

print(converter_format(inpt_val="23-12-05-1567", sep_="-",
inpt_frmt="shmy", frmt="Pnhdmy", default_val="00"))

#[1] "00-00-12-00-05-1567"
```


# `date_addr`

date_addr


## Description

Allow to add or substract two dates that have the same time unit or not


## Usage

```r
date_addr(
  date1,
  date2,
  add = FALSE,
  frmt1,
  frmt2 = frmt1,
  sep_ = "-",
  convert_to = "dmy"
)
```


## Arguments

Argument      |Description
------------- |----------------
`date1`     |     is the date from which the second date will be added or substracted
`date2`     |     is the date that will be added or will substract date1
`add`     |     equals to FALSE if you want date1 - date2 and TRUE if you want date1 + date2
`frmt1`     |     is the format of date1 (snhdmy) (second, minute, hour, day, monthn year)
`frmt2`     |     is the format of date2 (snhdmy)
`sep_`     |     is the separator of date1 and date2
`convert_to`     |     is the format of the outputed date


## Examples

```r
print(date_addr(date1="25-02", date2="58-12-08", frmt1="dm", frmt2="shd", sep_="-",
convert_to="dmy"))

#[1] "18-2-0"

print(date_addr(date1="25-02", date2="58-12-08", frmt1="dm", frmt2="shd", sep_="-",
convert_to="dmy", add=TRUE))

#[1] "3-3-0"

print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-",
convert_to="dmy", add=TRUE))

#[1] "27-3-2024"

print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-",
convert_to="dmy", add=FALSE))

#[1] "23-1-2024"

print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-",
convert_to="n", add=FALSE))

#[1] "1064596320"

print(date_addr(date1="25-02-2024", date2="1-01", frmt1="dmy", frmt2="dm", sep_="-",
convert_to="s", add=FALSE))

#[1] "63875779200"
```


# `date_converter_reverse`

date_converter_reverse


## Description

Allow to convert single date value like 2025.36 year to a date like second/minutehour/day/month/year (snhdmy)


## Usage

```r
date_converter_reverse(inpt_date, convert_to = "dmy", frmt = "y", sep_ = "-")
```


## Arguments

Argument      |Description
------------- |----------------
`inpt_date`     |     is the input date
`convert_to`     |     is the date format the input date will be converted
`frmt`     |     is the time unit of the input date
`sep_`     |     is the separator of the outputed date


## Examples

```r
print(date_converter_reverse(inpt_date="2024.929", convert_to="hmy", frmt="y", sep_="-"))

#[1] "110-11-2024"

print(date_converter_reverse(inpt_date="2024.929", convert_to="dmy", frmt="y", sep_="-"))

#[1] "4-11-2024"

print(date_converter_reverse(inpt_date="2024.929", convert_to="hdmy", frmt="y", sep_="-"))

#[1] "14-4-11-2024"

print(date_converter_reverse(inpt_date="2024.929", convert_to="dhym", frmt="y", sep_="-"))

#[1] "4-14-2024-11"
```


# `format_date`

format_date


## Description

Allow to convert xx-month-xxxx date type to xx-xx-xxxx


## Usage

```r
format_date(f_dialect, sentc, sep_in = "-", sep_out = "-")
```


## Arguments

Argument      |Description
------------- |----------------
`f_dialect`     |     are the months from the language of which the month come
`sentc`     |     is the date to convert
`sep_in`     |     is the separator of the dat input (default is "-")
`sep_out`     |     is the separator of the converted date (default is "-")


## Examples

```r
print(format_date(f_dialect=c("janvier", "février", "mars", "avril", "mai", "juin",
"juillet", "aout", "septembre", "octobre", "novembre", "décembre"), sentc="11-septembre-2023"))

#[1] "11-09-2023"
```


# `leap_yr`

bsx_year


## Description

Get if the year is leap


## Usage

```r
leap_yr(year)
```


## Arguments

Argument      |Description
------------- |----------------
`year`     |     is the input year


## Examples

```r
print(leap_yr(year=2024))

#[1] TRUE
```


# `sort_date`

sort_date


## Description

Allow to sort any vector containing a date, from any kind of format (my, hdmy, ymd ...), see examples.


## Usage

```r
sort_date(inpt_v, frmt, sep_ = "-", ascending = FALSE, give = "value")
```


## Arguments

Argument      |Description
------------- |----------------
`inpt_v`     |     is the input vector containing all the dates
`frmt`     |     is the format  of the dates, (any combinaison of letters "s" for second, "n", for minute, "h" for hour, "d" for day, "m" for month and "y" for year)
`sep_`     |     is the separator used for the dates
`ascending`     |     is the used to sort the dates
`give`     |     takes only two values "index" or "value", if give == "index", the function will output the index of sorted dates from inpt_v, if give == "value", the function will output the value, it means directly the sorted dates in inpt_v, see examples


## Examples

```r
print(sort_date(inpt_v = c("01-11-2025", "08-08-1922", "12-04-1966")
, frmt = "dmy", sep_ = "-", ascending = TRUE, give = "value"))

[1] "08-08-1922" "12-04-1966" "01-11-2025"

print(sort_date(inpt_v = c("01-11-2025", "08-08-1922", "12-04-1966")
, frmt = "dmy", sep_ = "-", ascending = FALSE, give = "value"))

[1] "01-11-2025" "12-04-1966" "08-08-1922"

print(sort_date(inpt_v = c("01-11-2025", "08-08-1922", "12-04-1966")
, frmt = "dmy", sep_ = "-", ascending = TRUE, give = "index"))

[1] 2 3 1

print(sort_date(inpt_v = c("22-01-11-2025", "11-12-04-1966", "12-12-04-1966")
, frmt = "hdmy", sep_ = "-", ascending = FALSE, give = "value"))

[1] "22-01-11-2025" "12-12-04-1966" "11-12-04-1966"

print(sort_date(inpt_v = c("03-22-01-11-2025", "56-11-12-04-1966", "23-12-12-04-1966")
, frmt = "nhdmy", sep_ = "-", ascending = FALSE, give = "value"))

[1] "03-22-01-11-2025" "23-12-12-04-1966" "56-11-12-04-1966"
```


