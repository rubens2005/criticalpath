#' criticalpath: Object Oriented Critical Path Method Implementation
#'
#' @description
#' criticalpath package is an object oriented implementation of the
#' Critical Path Method (CPM) in R with R6 library. CPM is a method used to estimate
#' the minimum project duration and  determine the amount of scheduling flexibility
#' on the logical network paths within the schedule model. The flexibility is in
#' terms of early start, early finish, late start, late finish, total float and
#' free float. Beside, it permits to quantify the complexity of network diagram
#' through the analysis of topological indicators. Finally, it permits to change
#' the activities duration to perform what-if scenario analysis.
#'
#' With this package, you can calculate the following CPM parameters:
#' - Schedule duration
#' - Early start and finish date of each activity
#' - Late start and finish date of each activity
#' - Critical activities
#' - Critical path
#' - Total float and free float
#' - Gantt Matrix
#' - What-if scenario analysis
#' - Topological indicators
#'
#' @author
#'   Rubens Jose Rosa (\email{rubens@@rubensjoserosa.com}),
#'   Marcos dos Santos (\email{marcosdossantos@@ime.eb.br}),
#'   Thiago Marques (\email{profestathimarques@@gmail.com})
#'
#' @seealso
#' On vignette package there are more information with examples about:
#' - Schedule Class Definition: [Schedule]
#' - How to create a schedule:
#'   - Add activities and relations together to an schedule.
#'   - Add activities to a schedule.
#'   - Add relations to a schedule.
#'   - Create a schedule object from data frames.
#'  - How to get schedule information:
#'    - Title, Reference and Schedule Duration.
#'  - How to get activities properties:
#'    - Activity Properties.
#'    - Gantt Matrix.
#'  - How to change activities duration:
#'    - Change Activities Duration.
#'  - How to get relations properties:
#'    - Relation Properties
#'    - Successors and Predecessors.
#'  - How to get topological properties:
#'    - Topological Indicators.
#'
#' @name criticalpath
#' @docType package
NULL
