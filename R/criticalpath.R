#' criticalpath: Object Oriented Critical Path Method
#'
#' An object oriented implementation of the Critical Path Method in R with R6 library.
#'
#' The goal of this package is to calculate the project duration
#' and to find its critical activities through application of Critical
#' Path Method to activities and precedence relation between them.
#'
#' @section Parameters calculated from CPM:
#'
#' Schedule duration:
#'
#' Critical Path
#'
#' Critical Activities
#'
#' @section Topological indicators:
#'
#' SP
#'
#' DA
#'
#' CA
#'
#' TF
#'
#' @section What else ONE:
#' Do not operate heavy machinery within 8 hours of using this function.
#'
#' @section What else TWO:
#' Do not operate heavy machinery within 8 hours of using this function.
#'
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @references
#'
#' Csardi, G. & Nepusz, T. (2005).
#' The Igraph Software Package for Complex Network Research.
#' *InterJournal*. Complex Systems. 1695.
#' [Article](https://www.researchgate.net/publication/221995787_The_Igraph_Software_Package_for_Complex_Network_Research)
#'  / [igraph](https://igraph.org/)
#'
#' Vanhoucke, M. (2009) *Measuring Time*:
#' Improving Project Performance Using Earned Value Management.
#' Springer-Verlag US.
#' doi: [10.1007/978-1-4419-1014-1](https://doi.org/10.1007/978-1-4419-1014-1).
#'
#' Vanhoucke, M. (2013) *Project Management with Dynamic Scheduling*:
#' Baseline Scheduling, Risk Analysis and Project Control.
#' Springer-Verlag Berlin Heidelberg.
#' doi: [10.1007/978-3-642-40438-2](https://doi.org/10.1007/978-3-642-40438-2)
#'
#' Vanhoucke, M. (2014) *Integrated Project Management and Control*:
#' First Comes the Theory, then the Practice.
#' Springer International Publishing Switzerland.
#' doi: [10.1007/978-3-319-04331-9](https://doi.org/10.1007/978-3-319-04331-9)
#'
#' @docType package
#' @name criticalpath
#' @aliases CPM cpm critical path
NULL


#' @title Create a schedule object from data frame or vice-versa
#' @name from_data_frame
#' @aliases schedule_from_data_frame from_data_frame
#'          as_data_frame relations_as_data_frame
#' @description The method \code{from_data_frame} creates an schedule object
#' from one or two data frames, one containing activities lists and the order the
#' precedence relations between activities. After creation, it is applied
#' the Critical Path Method (CPM).
#' The method \code{as_data_frame} does the opposite: it returns the activities
#' data frame, the relation data frame or both.
#' @param activities data frame with activities.
#' @param relations data frame with precedence relations between activities.
#' @param title A project title for identification.
#' @param reference A reference from project origin,
#' for example, a book, a paper, a corporation, or nothing.
#' @param what Character constant, whether to return info about
#' "activities", "relations" or "both". The default is "activities".
#' @return A Schedule object with an activities and relations,
#' and either a data frame or a list two data frames named
#' \code{activites} and \code{relations}.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @examples
#' activities <- data.frame(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
#' )
#' relations <- data.frame(
#'   from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,  7,
#'   8,  9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
#'   to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11, 12,
#'   13, 14, 15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
#' )
#' schedule <- Schedule$new()$from_data_frame(
#'   activities,
#'   relations,
#'   "Project 1: Cost Information System",
#'   "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice.
#'   Gent: Springer, 2014, p. 6"
#' )
#' schedule$as_data_frame()
#' schedule$as_data_frame("activities")
#' schedule$as_data_frame("relations")
#' schedule$as_data_frame("both")
NULL


#' @title Number of  activities / Has any activity
#' @name nr_activities
#' @aliases activities has_any_activity any_activity
#' @description Number of activities in a schedule as an integer value,
#' or a logical value that indicate if the schedule has any activity.
#' @return An integer indicating the number of activities
#' or a logical value TRUE, meaning that the schedule has any activity
#' or FALSE, for a empty schedule.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @examples
#' schedule <- Schedule$new()
#' schedule$has_any_activity # FALSE
#' schedule$nr_activities # 0
#'
#' schedule <- Schedule$new()
#' schedule$add_act_rel(1, "A" , 3, c(2,3))
#' schedule$add_act_rel(2, "B" , 4, c(4))
#' schedule$add_act_rel(3, "C" , 7, c(4))
#' schedule$add_act_rel(4, "D" , 5)
#' schedule$has_any_activity # TRUE
#' schedule$nr_activities # 4
NULL


#' @title Number of  relations / Has any relation
#' @name nr_relations
#' @aliases relations has_any_relations any_relations
#'
#' @description Number of relations (arcs) in a schedule as an integer value,
#' or a logical value that indicates if the schedule has any relation.
#' @return An integer indicating the number of relations
#' or a logical value TRUE, meaning that the schedule has any activity
#' or FALSE, for a empty schedule.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com), Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @examples
#' schedule <- Schedule$new()
#' schedule$has_any_relation # FALSE
#' schedule$nr_relations # 0
#'
#' schedule <- Schedule$new()
#' schedule$add_act_rel(1, "A" , 3, c(2,3))
#' schedule$add_act_rel(2, "B" , 4, c(4))
#' schedule$add_act_rel(3, "C" , 7, c(4))
#' schedule$add_act_rel(4, "D" , 5)
#' schedule$has_any_relation # TRUE
#' schedule$nr_relations # 4
NULL


#' @title Duration of a schedule
#' @name schedule_duration
#' @aliases duration
#' @description An integer value that indicate the duration of a schedule.
#' @return An integer indicating the duration of of a schedule.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com), Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @examples
#' schedule <- Schedule$new()
#' schedule$duration # 0
#'
#' schedule <- Schedule$new()
#' schedule$add_act_rel(1, "A" , 3, c(2,3))
#' schedule$add_act_rel(2, "B" , 4, c(4))
#' schedule$add_act_rel(3, "C" , 7, c(4))
#' schedule$add_act_rel(4, "D" , 5)
#' schedule$duration # 15
NULL

#' @title Topological Indicators: SP, AD, LA, TF
#' @name topological_indicators
#' @aliases topoi_sp topoi_ad topoi_la topoi_tf
#' topological indicator indicators
#' @description Shows information about network structure.
#' It may be of four type:
#'
#' **SP Serial or Parallel:**
#' It shows the closeness of a network to a serial or parallel graph.
#' As the network becomes serial, the SP increase, until one,
#' when the network totally serial.
#'
#' **AD Activity Distribution:**
#' Measures the distribution of the activities over the levels.
#' If AD is approximately equal zero, each level has same numbers of activities.
#' Otherwise, if AD is equal one, the quantity of each level is not
#' uniformly distributed.
#'
#' **LA Length of Arcs:**
#' Measures the presence of long arcs based on the difference between
#' the progressive level of the end activity and the start node
#' of each relation.
#' If LA is approximately equal zero, the progressive level between
#' activities are as far as possible.
#' Otherwise, if LA is equal one, the relation distance are one.
#'
#' **TF Topological Float Indicator:**
#' Measures the topological float of each activity.
#' If TF = 0, there is no float between activities.
#' If TF = 1, there is float between activities
#' and they be shift without affecting other activities.
#' @return A number between 0 and 1, inclusive.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @examples
#' schedule <- Schedule$new()
#' schedule$add_act_rel(1, "A" , 3, c(2,3))
#' schedule$add_act_rel(2, "B" , 4, c(4))
#' schedule$add_act_rel(3, "C" , 7, c(4))
#' schedule$add_act_rel(4, "D" , 5)
#' schedule$topoi_sp()
#' schedule$topoi_ad()
#' schedule$topoi_la()
#' schedule$topoi_tf()
NULL

