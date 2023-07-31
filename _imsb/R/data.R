#' Did the toast fall on the butter side?
#'
#' A dataset containing the results of 100 toast throws.
#'
#' @usage data(tartine1)
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{trial}{the trial number}
#'   \item{side}{the side of the toast, 1 is butter}
#'   }
#' @source Home-made dataset.
"tartine1"

#' Did the toast fall on the butter side?
#'
#' A dataset containing the results of 500 toast throws.
#'
#' @usage data(tartine2)
#' @format A data frame with 500 rows and 2 variables:
#' \describe{
#'   \item{trial}{the trial number}
#'   \item{side}{the side of the toast, 1 is butter}
#'   }
#' @source Home-made dataset.
"tartine2"

#' Estimating the probability of presence in a Psychology experiment
#'
#' A dataset containing the presence percentage according to weekdays and whether
#' a reminder email was sent.
#'
#' @usage data(absence)
#' @format A data frame with 13 rows and 6 variables:
#' \describe{
#'   \item{day}{day of the week}
#'   \item{inscription}{inscription mode}
#'   \item{reminder}{whether a reminder email was sent}
#'   \item{absence}{the number of absent participants}
#'   \item{presence}{the number of present participants}
#'   \item{total}{the total number of participants}
#'   }
#' @source Home-made dataset.
"absence"

#' Estimating the probability of presence in a Psychology experiment
#'
#' A dataset containing the presence percentage according to weekdays and the
#' researcher that collected the data.
#'
#' @usage data(absence_multilevel)
#' @format A data frame with 20 rows and 5 variables:
#' \describe{
#'   \item{reminder}{whether a reminder email was sent}
#'   \item{researcher}{id of the researcher who collected the data}
#'   \item{presence}{the number of present participants}
#'   \item{absence}{the number of absent participants}
#'   \item{total}{the total number of participants}
#'   }
#' @source Home-made dataset.
"absence_multilevel"

#' How long does it take to get a coffee?
#'
#' A dataset containing the time it takes to get a coffee at various coffee
#' places.
#'
#' @usage data(robot)
#' @format A data frame with 200 rows and 3 variables:
#' \describe{
#'   \item{cafe}{id for the place}
#'   \item{afternoon}{morning or afternoon}
#'   \item{wait}{waiting time in minutes}
#'   }
#' @source Adapted from the `rethinking package`.
"robot"

#' How long does it take to get a coffee?
#'
#' A dataset containing the time it takes to get a coffee at various coffee
#' places, with unequal variance.
#'
#' @usage data(robot_unequal)
#' @format A data frame with 450 rows and 3 variables:
#' \describe{
#'   \item{cafe}{id for the place}
#'   \item{afternoon}{morning or afternoon}
#'   \item{wait}{waiting time in minutes}
#'   }
#' @source Adapted from the `rethinking package`.
"robot_unequal"

#' Apples growth
#'
#' A dataset containing the diameter of apples recorded over multiple days and
#' multiple trees.
#'
#' @usage data(apples)
#' @format A data frame with 480 rows and 5 variables:
#' \describe{
#'   \item{tree}{id of the tree}
#'   \item{apple}{id of the apple (per apple)}
#'   \item{id}{id for each tree-apple combination}
#'   \item{time}{time in days}
#'   \item{diam}{diameter (in cm)}
#'   }
#' @source Home-made dataset.
"apples"

#' How moral was it?
#'
#' A dataset containing morality judgements in multiple conditions.
#'
#' @usage data(morale)
#' @format A data frame with 9930 rows and 7 variables:
#' \describe{
#'   \item{response}{how moral was it? response from 1 to 5}
#'   \item{id}{id of the subject}
#'   \item{age}{age of the subject}
#'   \item{male}{gender indicator}
#'   \item{action}{action condition (binary)}
#'   \item{intention}{intention condition (binary)}
#'   \item{contact}{contact conditio (binary)}
#'   }
#' @source Home-made dataset.
"morale"

#' Inheritance of human height
#'
#' A dataset containing height data for individuals and their parents.
#'
#' @usage data(parents)
#' @format A data frame with 40 rows and 4 variables:
#' \describe{
#'   \item{gender}{gender indicator variable (M vs. F)}
#'   \item{height}{height in inches}
#'   \item{mother}{height of mother in inches.}
#'   \item{father}{height of father in inches}
#'   }
#' @source Adapted from the `rethinking` package.
"parents"

#' Survival of Titanic's passengers
#'
#' Information on the survival of the passengers of the Titanic according to
#' economic status, sex, and age.
#'
#' @usage data(titanic)
#' @format A data frame with 539 rows and 5 variables:
#' \describe{
#'   \item{survival}{binary numerical variable (0 or 1)}
#'   \item{pclass}{lower or upper class}
#'   \item{gender}{binary sex indicator variable (female or male)}
#'   \item{age}{age in years}
#'   \item{parch}{number of relatives on board}
#'   }
#' @source Adapted from `datasets::Titanic`.
"titanic"

#' Howell !Kung demography data
#'
#' Demographic data from Kalahari !Kung San people collected by Nancy Howell
#'
#' @usage data(howell)
#' @format A data frame with 544 rows and 4 variables:
#' \describe{
#'   \item{height}{height in cm}
#'   \item{weight}{weight in kg}
#'   \item{age}{age in years}
#'   \item{male}{gender indicator}
#'   }
#' @source Adapted from the `rethinking` package.
"howell"

#' Waffle House and marriage statistics
#'
#' Data for the individual States of the United States, describing number of
#' Waffle House diners and various marriage and demographic facts.
#'
#' @usage data(waffle)
#' @format A data frame with 50 rows and 13 variables:
#' \describe{
#'   \item{Location}{State name}
#'   \item{Loc}{State abbreviation}
#'   \item{Population}{2010 population in millions}
#'   \item{MedianAgeMarriage}{2005-2010 median age at marriage}
#'   \item{Marriage}{2009 marriage rate per 1000 adults}
#'   \item{Marriage.SE}{Standard error of rate}
#'   \item{Divorce}{2009 divorce rate per 1000 adults}
#'   \item{Divorce.SE}{Standard error of rate}
#'   \item{WaffleHouses}{Number of diners}
#'   \item{South}{1 indicates Southern State}
#'   \item{Slaves1860}{Number of slaves in 1860 census}
#'   \item{Population1860}{Population from 1860 census}
#'   \item{PropSlaves1860}{Proportion of total population that were slaves in 1860}
#'   }
#' @source 1860 census data from http://mapserver.lib.virginia.edu.
#' Marriage and divorce rates from 2009 American Community Survey (ACS).
#' Waffle House density data from wafflehouse.com (retrieved January 2012).
#' Adapted from the `rethinking` package.
"waffle"

#' Primate milk data
#'
#' Small dataset from Hinde and Milligan (2011) on primate milk composition.
#' This data is discussed in detail in McElreath (2020).
#'
#' @usage data(milk)
#' @format A data frame with 29 rows and 8 variables:
#' \describe{
#'   \item{clade}{...}
#'   \item{species}{...}
#'   \item{kcal.per.g}{...}
#'   \item{perc.fat}{...}
#'   \item{perc.protein}{...}
#'   \item{perc.lactose}{...}
#'   \item{mass}{...}
#'   \item{neocortex.perc}{...}
#'   }
#' @source Adapted from the `rethinking` package.
"milk"

#' Tulips data...
#'
#' Tulips data...
#'
#' @usage data(tulips)
#' @format A data frame with 29 rows and 8 variables:
#' \describe{
#'   \item{bed}{...}
#'   \item{water}{...}
#'   \item{shade}{...}
#'   \item{blooms}{...}
#'   }
#' @source Adapted from the `rethinking` package.
"tulips"

#' Rugged data...
#'
#' Rugged data...
#'
#' @usage data(rugged)
#' @format A data frame with 234 rows and 5 variables:
#' \describe{
#'   \item{isocode}{...}
#'   \item{country}{...}
#'   \item{rugged}{...}
#'   \item{cont_africa}{...}
#'   \item{rgdppc_2000}{...}
#'   }
#' @source Adapted from the `rethinking` package.
"rugged"

#' Meta-analysis data...
#'
#' Meta-analysis data...
#'
#' @usage data(meta)
#' @format A data frame with 32 rows and 4 variables:
#' \describe{
#'   \item{study}{...}
#'   \item{experiment}{...}
#'   \item{yi}{...}
#'   \item{vi}{...}
#'   }
#' @source ...
"meta"

#' Popularity data...
#'
#' Popularity data...
#'
#' @usage data(popular)
#' @format A data frame with 2000 rows and 6 variables:
#' \describe{
#'   \item{pupil}{...}
#'   \item{school}{...}
#'   \item{popular}{...}
#'   \item{sex}{...}
#'   \item{texp}{...}
#'   \item{teachpop}{...}
#'   }
#' @source ...
"popular"

#' Admission data...
#'
#' Admission data...
#'
#' @usage data(admission)
#' @format A data frame with 12 rows and 5 variables:
#' \describe{
#'   \item{dept}{...}
#'   \item{gender}{...}
#'   \item{admit}{...}
#'   \item{reject}{...}
#'   \item{applications}{...}
#'   }
#' @source Adapted from...
"admission"

#' Chimpanzees data...
#'
#' Chimpanzees data...
#'
#' @usage data(chimpanzees)
#' @format A data frame with 504 rows and 8 variables:
#' \describe{
#'   \item{actor}{...}
#'   \item{recipient}{...}
#'   \item{condition}{...}
#'   \item{block}{...}
#'   \item{trial}{...}
#'   \item{prosoc_left}{...}
#'   \item{chose_prosoc}{...}
#'   \item{pulled_left}{...}
#'   }
#' @source Adapted from the `rethinking` package.
"chimpanzees"
