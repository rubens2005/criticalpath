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
#'
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
#' @seealso
#'
#' More information about:
#'
#' Schedule object [Schedule]
#'
#' Package: [criticalpath]
#'
#' How to create a schedule from scratch: [from_data_frame]
#'
#' @docType package
#' @name criticalpath
#' @aliases CPM cpm critical path
NULL


#' @title Create a schedule object from data frames
#' @name schedule_from_data_frame
#' @aliases from_data_frame new_schedule schedule_new new
#' @description
#' Make a schedule with activities and relations between activities.
#' The method \code{Schedule$new(activities, relations)}
#' creates an schedule object from two data frames,
#' one containing activities lists and the other the precedence relations
#' between activities.
#' After creation, it is applied the Critical Path Method (CPM).
#'
#' It is possible to create a empty schedule, without any activity or relation
#' with the constructor \code{Schedule$new()}.
#' After that, it is possible to add activity with \code{add_activity}
#' and relation with \code{add_relation} methods.
#'
#' @param activities Data frame with activities.
#' If it is not informed, the schedule will be created without any activity.
#' Its structure is:
#' - **id:** Activity id, an integer number that
#' must be unique within a schedule.
#' It will be used to make relation between activities.
#' - **name:** The name of activity. The default is an empty string.
#' - **duration:** A number that represents the activity's duration.
#' It is integer number without time unit.
#' It must be equal or grater than zero. The default value is zero.
#' @param relations Data frame with precedence relations between activities.
#' If it is informed, the activities has to be informed too.
#' If is is not informed, the schedule will be created without any relation.
#' It is formed by predecessor activity e successor activity.
#' Its structure is:
#' - **from:** The id of predecessor activity. Must exist a activity with from id.
#' - **to:** The id of successor activity. Must exist a activity with to id.
#' - **type:** Specifies the type of relation between activities.
#' The default type is FS and its value may be: FS, FF, SS, SF, that means:
#'     - **FS:** Finish-Start relation.
#' Activity to_id can only start after the finish of activity from_id.
#'     - **FF:** Finish-Finish relation.
#' Activity to_id must finish together with activity from_id.
#'     - **SS:** Start-Start relation.
#' Activity to_id must start together with activity from_id.
#'     - **SF:** Start-Finish relation.
#' Activity to_id must finish when activity from_id starts.
#' - **lag:** The time period between activities that the successor activity
#' must be advanced, or lated, after activity from_id.
#' It must be an integer, less than, equal or greater than zero.
#' @return A Schedule object with CPM parameters calculated.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [add_activity], [add_relation], [criticalpath], [Schedule].
#' @examples
#'
#' # An empty schedule.
#' schedule <- Schedule$new()
#'
#' # A schedule with activities and relations.
#' activities <- data.frame(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
#' )
#'
#' relations <- data.frame(
#'   from = c(1, 1, 2, 2, 2, 3, 3, 3,  3,  4,  5,  6,
#'            7,  8,  9, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15, 15),
#'   to   = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11,
#'            12, 13, 14, 15, 16, 17, 16, 17, 16, 17, 16, 17, 16, 17)
#' )
#' schedule <- Schedule$new(activities, relations)
#' schedule$title <- "Project 1: Cost Information System"
#' schedule$reference <- "VANHOUCKE, Mario.
#' Integrated project management and control:
#'   first comes the theory, then the practice.
#'   Gent: Springer, 2014, p. 6"
NULL


#' @title Add an activity to a schedule.
#' @name schedule_from_activities
#' @aliases add_activity add_activities
#' @description
#' \code{Schedule$add_activity(id, name, duraiton)}
#' adds an activity to a schedule.
#'
#' \code{Schedule$add_activities(activities)}
#' adds an activities data frame to a schedule.
#' @param id Activity id, an integer number that
#' must be unique within a schedule.
#' It will be used to make relation between activities.
#' @param name The name of activity. The default is an empty string.
#' @param duration A number that represents the activity's duration.
#' It is integer number without time unit.
#' It must be equal or grater than zero. The default value is zero.
#' @return A Schedule object with an activity added and
#' the critical path calculated.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [schedule_from_data_frame], [add_relation], [criticalpath], [Schedule].
#' @examples
#' # Activities added one by one
#' schedule <- Schedule$new()
#' schedule$add_activity(1, "Task 1", 5)
#' schedule$add_activity(2, "Task 2", 6)
#' schedule$add_activity(3, "Task 3", 8)
#' schedule$add_activity(4, "Task 4", 6)
#' schedule$add_activity(5, "Task 5", 9)
#' schedule$add_activity(6, "Task 6", 3)
#' schedule$add_activity(7, "Task 7", 4)
#'
#' # Activities add once for all
#' activities <- data.frame(
#'   id        = 1:17,
#'   name      = paste("a", as.character(1:17), sep=""),
#'   duration  = c(1,2,2,4,3,3,3,2,1,1,2,1,1,1,1,2,1)
#' )
#' schedule <- Schedule$new()
#' schedule$add_activities(activities)
#'
NULL


#' @title Schedule with relations added one by one
#' @name schedule_from_relations
#' @aliases add_relation add_relations
#' @description
#' - $add_relation(from_id, to_id, type="FS", lag=0)
#' - $add_relations(relations)
NULL


#' @title Schedule with activities and relations added together
#' @name schedule_from_act_rel
#' @aliases add_act_rel
#' @description
#' - $add_act_rel(id, name, duration, relations_id=c(), direction="succ")
NULL


#' @title Title, Reference and Duration
#' @name reference
#' @aliases title reference duration schedule_duration
#' @description
#' **title** A project title for identification.
#' **reference** A reference from project origin,
#' for example, a book, a paper, a corporation, or nothing.
#' - title: A project title for identification.
#' - reference: A reference from project origin, for example, a book, a paper, a corporation, or nothing.
#' - duration: An integer value that indicate the project duration calculated by CPM.
#' An integer value that indicate the duration of a schedule.
NULL


#' @title Activity Properties
#' @name activity_properties
#' @aliases nr_activities has_any_activity any_activity get_activity
#' @description
#' - activities: Return activities in a schedule as data frame.
#' - DESCREVER CADA UMA DAS COLUNAS DO DATA FRAME
#' - $get_activity(id)
#' - nr_activities: Number of activities in a schedule as an integer value.
#' - has_any_activity: A logical value that indicate if the schedule has any activity.
NULL
#' @title Number of  activities / Has any activity
#' @name nr_activities
#' @aliases activities has_any_activity any_activity
#' @description Number of activities in a schedule as an integer value,
#' or a logical value that indicate if the schedule has any activity.
#' The method Schedule$activities and Schedule$relations
#' does the opposite: it returns the activities data frame
#' and the relation data frame from a schedule.
#' @return An integer indicating the number of activities
#' or a logical value TRUE, meaning that the schedule has any activity
#' or FALSE, for a empty schedule.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
NULL


#' @title Gantt Matrix
#' @name gantt_matrix
#' @aliases gantt xy_gantt xy
#' @description
#' - $gantt_matrix()
#' - $xy_gantt_matrix(gantt=NULL)
NULL


#' @title Relation Properties
#' @name relation_properties
#' @aliases nr_relations has_any_relation any_relation
#' @description
#' - relations: Return relations in a schedule as data frame.
#' - DESCREVER CADA UMA DAS COLUNAS DO DATA FRAME
#' - nr_relations: Number of relations (arcs) in a schedule as an integer value.
#' - has_any_relation: A logical value that indicate if the schedule has any relation.
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
NULL

#' @title Successors and Predecessors
#' @name sucessors
#' @aliases all_successors all_predecessors predecessors
#' is_redundant redundant
#' @description
#' - $all_successors(id, ign_to=NULL)
#' - $all_predecessors(id, ign_from=NULL)
#' - $is_redundant(id_from, id_to)
NULL


#' @title Topological Indicators: SP, AD, LA, TF
#' @name topological_indicators
#' @aliases topoi_sp topoi_ad topoi_la topoi_tf sp ad la tf
#' topological indicator indicators
#' @description
#' - $topoi_sp()
#' - $topoi_ad()
#' - $topoi_la()
#' - $topoi_tf()
#'
#'
#' Shows information about network structure.
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
NULL

#' @title Changing Activities Durations
#' @name change_durations
#' @description
#' - $change_durations(new_durations)
NULL

