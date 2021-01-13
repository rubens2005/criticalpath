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
#' Activity to id can only start after the finish of activity from id.
#'     - **FF:** Finish-Finish relation.
#' Activity to id must finish together with activity from id.
#'     - **SS:** Start-Start relation.
#' Activity to id must start together with activity from id.
#'     - **SF:** Start-Finish relation.
#' Activity to id must finish when activity from id starts.
#' - **lag:** The time period between activities that the successor activity
#' must be advanced, or lated, after activity from id.
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


#' @title Add activities to a schedule.
#' @name schedule_from_activities
#' @aliases add_activity add_activities
#' @description
#' \code{Schedule$add_activity(id, name, duration)}
#' adds an activity to a schedule.
#'
#' \code{Schedule$add_activities(activities)}
#' adds an activities data frame to a schedule.
#'
#' Usage
#'
#' Schedule$add_activity(id, name, duration)
#'
#' Schedule$add_activities(activities)
#'
#' @param id Activity id, an integer number that
#' must be unique within a schedule.
#' It will be used to make relation between activities.
#' @param name The name of activity. The default is an empty string.
#' @param duration A number that represents the activity's duration.
#' It is integer number without time unit.
#' It must be equal or grater than zero. The default value is zero.
#' @param activities Data frame with activities.
#' If it is not informed, the schedule will be created without any activity.
#' Its structure is defined in [schedule_from_data_frame]
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


#' @title Add relations to a schedule
#' @name schedule_from_relations
#' @aliases add_relation add_relations
#' @description
#' \code{Schedule$add_relation(from, to, type="FS", lag=0)}
#' adds a relation to a schedule.
#'
#' \code{Schedule$add_relations(relations)}
#' adds a relations data frame to a schedule.
#'
#' Usage
#'
#' Schedule$add_relation(from, to, type="FS", lag=0)
#'
#' Schedule$add_relations(relations)
#' @param from The id of predecessor activity.
#' Must exist a activity with from id.
#' @param to The id of successor activity.
#' Must exist a activity with to.
#' @param type
#' Specifies the type of relation between activities.
#' The default type is FS and its value may be: FS, FF, SS, SF, that means:
#' If type is not defined, it is assumed to be FS.
#'
#' - **FS:** Finish-Start relation.
#' Activity to can only start after the finish of activity from.
#'
#' - **FF:** Finish-Finish relation.
#' Activity to must finish together with activity from.
#'
#' - **SS:** Start-Start relation.
#' Activity to must start together with activity from.
#'
#' - **SF:** Start-Finish relation.
#' Activity to must finish when activity from starts.
#'
#' @param lag
#' The time period between activities that the successor activity
#' must be advanced, or lated, after activity from.
#' It must be an integer, less than, equal or greater than zero.
#' If lag is not informed, it is assumed to be zero.
#' @param relations Data frame with precedence relations between activities.
#' It is formed by predecessor activity e successor activity.
#' Its structure is defined in [schedule_from_data_frame].
#'
#' @return A Schedule object with CPM parameters calculated.
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [schedule_from_data_frame], [add_relation], [criticalpath], [Schedule].
#' @examples
#' # First, create an empty schedule
#' schedule <- Schedule$new()
#' schedule$title <- "Project 3: Old Carriage House Renovation"
#' schedule$reference <-
#'   "VANHOUCKE, Mario. Integrated project management and control:
#'   first comes the theory, then the practice. Gent: Springer, 2014, p. 11"
#'
#' # Second, add activities to it
#' schedule$add_activity(1, "a1" , 2)
#' schedule$add_activity(2, "a2" , 2)
#' schedule$add_activity(3, "a3" , 4)
#' schedule$add_activity(4, "a4" , 3)
#' schedule$add_activity(5, "a5" , 4)
#' schedule$add_activity(6, "a6" , 1)
#' schedule$add_activity(7, "a7" , 1)
#' schedule$add_activity(8, "a8" , 1)
#' schedule$add_activity(9, "a9" , 1)
#' schedule$add_activity(10, "a10", 1)
#' schedule$add_activity(11, "a11", 3)
#' schedule$add_activity(12, "a12", 2)
#' schedule$add_activity(13, "a13", 1)
#' schedule$add_activity(14, "a14", 1)
#' schedule$add_activity(15, "a15", 2)
#' schedule$add_activity(16, "a16", 1)
#' schedule$add_activity(17, "a17", 1)
#'
#' # Finaly, add relations to it
#' schedule$add_relation( 1, 2)
#' schedule$add_relation( 2, 3)
#' schedule$add_relation( 3, 4)
#' schedule$add_relation( 4, 5)
#' schedule$add_relation( 5, 6)
#' schedule$add_relation( 6, 7)
#' schedule$add_relation( 6, 8)
#' schedule$add_relation( 6, 9)
#' schedule$add_relation( 7, 10)
#' schedule$add_relation( 8, 10)
#' schedule$add_relation( 9, 10)
#' schedule$add_relation( 10, 11)
#' schedule$add_relation( 10, 13)
#' schedule$add_relation( 11, 12)
#' schedule$add_relation( 12, 15)
#' schedule$add_relation( 13, 14)
#' schedule$add_relation( 14, 15)
#' schedule$add_relation( 15, 16)
#' schedule$add_relation( 16, 17)
NULL


#' @title Add activities and relations together to an schedule.
#' @name schedule_from_act_rel
#' @aliases add_act_rel
#' @description Add an activity to a schedule
#' in the same time that adds relations.
#'
#' Usage
#'
#' $add_act_rel(id, name, duration, relations_id=c(), direction="succ")
#' @param id Activity id, an integer number that
#' must be unique within a schedule.
#' It will be used to make relation between activities.
#' @param name The name of activity.
#' @param duration A number that represents the activity's duration.
#' It must be equal or grater than zero.
#' @param relations_id A vector of ids such that will be linked with activity.
#' It may be a relations of successor or predecessors.
#' @param direction Direction of relations_id: It may be "succ" or "pred".
#' - **succ:** the relations_id vector will be the successor of the activity.
#' - **pred:** the relations_id vector will be the predecessor of the activity.
#' @return A Schedule object with activities and relations added
#' and CPM's parameters calculated.
#' @examples
#' # Create a schedule
#' schedule <- Schedule$new()
#' schedule$title <- "Fictitious Project Example"
#' schedule$reference <- "VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18"
#'
#' # Add activities and relations to it.
#' schedule$add_act_rel(  1, "a1" , 0, c(2,3,4))
#' schedule$add_act_rel(  2, "a2" , 4, c(5))
#' schedule$add_act_rel(  3, "a3" , 9, c(10))
#' schedule$add_act_rel(  4, "a4" , 1, c(6))
#' schedule$add_act_rel(  5, "a5" , 4, c(9))
#' schedule$add_act_rel(  6, "a6" , 5, c(7))
#' schedule$add_act_rel(  7, "a7" , 1, c(8,11))
#' schedule$add_act_rel(  8, "a8" , 7, c(12))
#' schedule$add_act_rel(  9, "a9" , 8, c(12))
#' schedule$add_act_rel( 10, "a10", 3, c(12))
#' schedule$add_act_rel( 11, "a11", 3, c(12))
#' schedule$add_act_rel( 12, "a12", 0)
NULL


#' @title Title, Reference and Schedule Duration
#' @name reference
#' @aliases title reference duration schedule_duration
#' @description After a schedule is created, it is possible
#' access several type of information:
#'
#' - **title:** A project title for identification. It depends on
#' user. Its use are:
#'    - \code{Sechedule$title <- "A title"} sets a title for a project.
#'    - \code{Sechedule$title} gets the title of the project.
#' - **reference:** A reference from project origin,
#' for example, a book, a paper, a corporation, or nothing.
#'    - \code{Sechedule$reference <- "A reference"} sets a reference for a project.
#'    - \code{Sechedule$title} gets the reference of the project.
#' - **duration:** An integer value that indicate the project duration
#' calculated by CPM. Every time you include an activity or a relation
#' to a schedule, the duration is calculated. You can only read
#' the schedule duration.
#'    - \code{Sechedule$duration} gets the duration of the project.
#' @examples
#' # Create a schedule
#' schedule <- Schedule$new()
#' schedule$title <- "Fictitious Project Example"
#' schedule$reference <- "VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18"
#'
#' # Add activities and relations to it.
#' schedule$add_act_rel(  1, "a1" , 0, c(2,3,4))
#' schedule$add_act_rel(  2, "a2" , 4, c(5))
#' schedule$add_act_rel(  3, "a3" , 9, c(10))
#' schedule$add_act_rel(  4, "a4" , 1, c(6))
#' schedule$add_act_rel(  5, "a5" , 4, c(9))
#' schedule$add_act_rel(  6, "a6" , 5, c(7))
#' schedule$add_act_rel(  7, "a7" , 1, c(8,11))
#' schedule$add_act_rel(  8, "a8" , 7, c(12))
#' schedule$add_act_rel(  9, "a9" , 8, c(12))
#' schedule$add_act_rel( 10, "a10", 3, c(12))
#' schedule$add_act_rel( 11, "a11", 3, c(12))
#' schedule$add_act_rel( 12, "a12", 0)
#' schedule$title
#' schedule$reference
#' schedule$duration
NULL


#' @title Activity Properties
#' @name activity_properties
#' @aliases nr_activities has_any_activity any_activity activities get_activity
#' @description
#' There several methods about activities that you can use to get
#' information about them. After you add activities to a schedule, you may
#' want to know how much activities has in a schedule or when each activity
#' is planned to start or to finish.
#' In this section, we will explain how you can find these information.
#'
#' - **Has Any Activity:** A logical value that indicate if the schedule
#' has any activity. A TRUE value means that the schedule has some
#' activity; a FALSE, means that the schedule is empty.
#'    - Usage: \code{Schedule$has_any_activity}
#'
#' - **Number of Activities:** Number of activities in a schedule
#' as an integer value.
#'    - Usage: \code{Schedule$nr_activities}
#'
#' - **Get activity:** Gets an activity by id. It returns a data
#' frame with one line about activity. The structure of a data is
#' described in next topic.
#'
#'    - Usage: \code{Schedule$get_activity(id)}
#'
#' - **Activities:** Return a data frame with all activities of a schedule
#' in a activity id order. This is the main information calculated by CPM.
#' The data frame is formed by following structure:
#'    - **id:** Activity id.
#'    - **name:** The name of activity.
#'    - **duration:** A number that represents the activity's duration.
#'    - **milestone:** A milestone is an activity with zero duration.
#'    This property indicates if a activity is a milestone or not:
#'    \code{TRUE} indicates it is a milestone; \code{FALSE} indicates it is not.
#'    - **critical:** A critical activity is one with total float minor or equal
#'    to zero. This property indicates if a activity is critical:
#'    \code{TRUE} indicates it is critical;
#'    \code{FALSE} indicates it is not critical.
#'    - **ES:** Early Start: is the earliest start period an activity can begin
#'    after its predecessors without violating precedence relation.
#'    - **EF:** Early Finish: is the early start plus activity duration.
#'    - **LS:** Late Start: is the late finish minus activity duration.
#'    - **LF:** Late Finish: is the latest finish an activity can finish
#'    before their successors without violating precedence relation.
#'    - **total_float:** It is the amount of period an activity can be
#'    delayed without violating the project duration. Its formula is:
#'    LS - ES or LF - EF.
#'    - **free_float:** It is the amount of period an activity can be
#'    delayed without violating the start time of the successors activities.
#'    - **progr_level:** Progressive level is the rank of activities counted
#'    from begin. The level of the activities that don't have predecessor is one;
#'    the level of the other activities, is one plus the maximal level of
#'    their predecessor.
#'    - **regr_level:** Regressive level is the rank of activities counted
#'    from the end. The level of the activities that don't have successor is the
#'    maximal progressive level; the level of the other activities,
#'    is one minus the minimal level of their successor.
#'    - **topo_float:** It is the difference between progressive level
#'     and regressive level.
#'
#'    - Usage: \code{Schedule$activities}
#'
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#'    - Usage: \code{Schedule$get_activity(id)}
#' @seealso [criticalpath], [Schedule].
#'
#' @examples
#' # Create a schedule
#' schedule <- Schedule$new()
#' schedule$title <- "Fictitious Project Example"
#' schedule$reference <- "VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18"
#'
#' schedule$has_any_activity  # FALSE
#' schedule$nr_activities     # 0
#'
#' # Add activities and relations to it.
#' schedule$add_act_rel(  1, "a1" , 0, c(2,3,4))
#' schedule$add_act_rel(  2, "a2" , 4, c(5))
#' schedule$add_act_rel(  3, "a3" , 9, c(10))
#' schedule$add_act_rel(  4, "a4" , 1, c(6))
#' schedule$add_act_rel(  5, "a5" , 4, c(9))
#' schedule$add_act_rel(  6, "a6" , 5, c(7))
#' schedule$add_act_rel(  7, "a7" , 1, c(8,11))
#' schedule$add_act_rel(  8, "a8" , 7, c(12))
#' schedule$add_act_rel(  9, "a9" , 8, c(12))
#' schedule$add_act_rel( 10, "a10", 3, c(12))
#' schedule$add_act_rel( 11, "a11", 3, c(12))
#' schedule$add_act_rel( 12, "a12", 0)
#'
#' schedule$has_any_activity  # TRUE
#' schedule$nr_activities     # 12
#'
#' schedule$get_activity(10)
#' schedule$activities
NULL


#' @title Gantt Matrix
#' @name gantt_matrix
#' @aliases gantt xy_gantt xy
#' @description Create a matrix that represents a Gantt chart,
#' a matrix where "1" indicate that an activity is planned to be
#' in execution.
#'
#' In this matrix, the rows represents activities,
#' Whereas the columns represents the activity execution period.
#' So, the number of columns is equal to project duration.
#'
#' -  \code{Schedule$gantt_matrix()} Return the Gantt matrix.
#'
#' -  \code{Schedule$xy_gantt_matrix(gantt=NULL)}
#' Transform a Gantt matrix in x, y coordinates and the weight one.
#' Each point greater than zero in a Gantt matrix becomes a x, y coordinate.
#'
#' @examples
#' activities <- data.frame(
#'   id        = c( 1,   2,   3,   4 ),
#'   name      = c("A", "B", "C", "D"),
#'   duration  = c( 2,   3,   1,   2 )
#' )
#' relations <- data.frame(
#'   from = c(1, 2, 4, 4),
#'   to   = c(3, 3, 1, 2)
#' )
#' schedule <- Schedule$new(activities, relations)
#' schedule$title <- "A project"
#' schedule$reference <- "From criticalpath"
#' gantt <- schedule$gantt_matrix()
#' gantt
#' # What is the effort by time period?
#' colSums(gantt) # 1 1 2 2 1 1
#' # What is the duration by activities?
#' rowSums(gantt) # 2 3 1 2
#' # what is the S curve
#' cumsum(colSums(gantt))
#' plot(cumsum(colSums(gantt)), type="l", lwd=3)
#' xyw <- schedule$xy_gantt_matrix()
#' xyw
#' plot(xyw[, 1:2])
NULL


#' @title Relation Properties
#' @name relation_properties
#' @aliases nr_relations has_any_relation any_relation relations
#' @description
#' There several methods about relations that you can use to get
#' information about them. After you add activities and relations
#' to a schedule, you may want to know how much relations has in a
#' schedule or which relations belong to critical path.
#' In this section, we will explain how you can find these information.
#'
#' - **Has Any Relation:** A logical value that indicates if the schedule
#' has any relation. A TRUE value means that the schedule has some
#' relation; a FALSE, means that the schedule does not have any relation.
#'    - Usage: \code{Schedule$has_any_relation}
#'
#' - **Number of Relations:** Number of relations in a schedule
#' as an integer value.
#'    - Usage: \code{Schedule$nr_relations}
#'
#' - **Relations:** Return a data frame with all relations of a schedule
#' in inclusion order. This is the main information calculated by CPM.
#' The data frame is formed by following structure:
#'    - **from:** Predecessor activity id from a relation.
#'    - **to:** Successor activity id from a relation.
#'    - **type:** The type of relation between activities.
#'    Its value may be: FS, FF, SS, SF, as described in [schedule_from_data_frame]
#'    - **lag:** The time period between activity predecessor and
#'    activity successor activity
#'    - **critical:** A critical relation formed by two activity critical:
#'    predecessor and successor.
#'    \code{TRUE} indicates it is critical;
#'    \code{FALSE} indicates it is not critical.
#'    - **ord:** Indicates de order that the relation was added in the schedule.
#'    - **i_from:** It the index of predecessor activity in the
#'    activities data frame.
#'    - **i_to:** It the index of successor activity in the
#'    activities data frame.
#'
#'    - Usage: \code{Schedule$relations}
#'
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com), Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#' @examples
#' # Create a schedule
#' schedule <- Schedule$new()
#' schedule$title <- "Fictitious Project Example"
#' schedule$reference <- "VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18"
#'
#' schedule$has_any_relation  # FALSE
#' schedule$nr_relations      # 0
#'
#' # Add activities and relations to it.
#' schedule$add_act_rel(  1, "a1" , 0, c(2,3,4))
#' schedule$add_act_rel(  2, "a2" , 4, c(5))
#' schedule$add_act_rel(  3, "a3" , 9, c(10))
#' schedule$add_act_rel(  4, "a4" , 1, c(6))
#' schedule$add_act_rel(  5, "a5" , 4, c(9))
#' schedule$add_act_rel(  6, "a6" , 5, c(7))
#' schedule$add_act_rel(  7, "a7" , 1, c(8,11))
#' schedule$add_act_rel(  8, "a8" , 7, c(12))
#' schedule$add_act_rel(  9, "a9" , 8, c(12))
#' schedule$add_act_rel( 10, "a10", 3, c(12))
#' schedule$add_act_rel( 11, "a11", 3, c(12))
#' schedule$add_act_rel( 12, "a12", 0)
#'
#' schedule$has_any_relation  # TRUE
#' schedule$nr_relations      # 14
#'
#' schedule$relations
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
#' Shows information about network structure.
#' It may be of four type:
#'
#' **SP Serial or Parallel:**
#' It shows the closeness of a network to a serial or parallel graph.
#' As the network becomes serial, the SP increase, until one,
#' when the network totally serial.
#'    - Usage: \code{Schedule$topoi_sp()}
#'
#' **AD Activity Distribution:**
#' Measures the distribution of the activities over the levels.
#' If AD is approximately equal zero, each level has same numbers of activities.
#' Otherwise, if AD is equal one, the quantity of each level is not
#' uniformly distributed.
#'    - Usage: \code{Schedule$topoi_ad()}
#'
#' **LA Length of Arcs:**
#' Measures the presence of long arcs based on the difference between
#' the progressive level of the end activity and the start node
#' of each relation.
#' If LA is approximately equal zero, the progressive level between
#' activities are as far as possible.
#' Otherwise, if LA is equal one, the relation distance are one.
#'    - Usage: \code{Schedule$topoi_la()}
#'
#' **TF Topological Float Indicator:**
#' Measures the topological float of each activity.
#' If TF = 0, there is no float between activities.
#' If TF = 1, there is float between activities
#' and they be shift without affecting other activities.
#'    - Usage: \code{Schedule$topoi_tf()}
#'
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#' @seealso [criticalpath], [Schedule].
#'
#' @examples
#' # Create a schedule
#' schedule <- Schedule$new()
#' schedule$title <- "Fictitious Project Example"
#' schedule$reference <- "VANHOUCKE, Mario. Measuring time:
#'   improving project performance using earned value management.
#'   Gent: Springer, 2009, p. 18"
#'
#' # Add activities and relations to it.
#' schedule$add_act_rel(  1, "a1" , 0, c(2,3,4))
#' schedule$add_act_rel(  2, "a2" , 4, c(5))
#' schedule$add_act_rel(  3, "a3" , 9, c(10))
#' schedule$add_act_rel(  4, "a4" , 1, c(6))
#' schedule$add_act_rel(  5, "a5" , 4, c(9))
#' schedule$add_act_rel(  6, "a6" , 5, c(7))
#' schedule$add_act_rel(  7, "a7" , 1, c(8,11))
#' schedule$add_act_rel(  8, "a8" , 7, c(12))
#' schedule$add_act_rel(  9, "a9" , 8, c(12))
#' schedule$add_act_rel( 10, "a10", 3, c(12))
#' schedule$add_act_rel( 11, "a11", 3, c(12))
#' schedule$add_act_rel( 12, "a12", 0)
#'
#' schedule$topoi_sp()
#' schedule$topoi_ad()
#' schedule$topoi_la()
#' schedule$topoi_tf()
NULL

#' @title Changing Activities Durations
#' @name change_durations
#' @description
#' - $change_durations(new_durations)
NULL

