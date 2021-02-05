#' criticalpath: Object Oriented Critical Path Method Implementation
#'
#' @description
#' criticalpath package is an object oriented implementation of the
#' Critical Path Method (CPM) in R with R6 library. CPM is a method used to estimate
#' the minimum project duration and  determine the amount of scheduling flexibility
#' on the logical network paths within the schedule model. The flexibility is in
#' terms of early start, early finish, late start, late finish, total float and
#' free float. Besides, it permits to quantify the complexity of network diagram
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
#' @references
#'
#' Csardi, G. & Nepusz, T. (2005).
#' The Igraph Software Package for Complex Network Research.
#' *InterJournal*. Complex Systems. 1695.
#'
#' Project Management Institute (2017)
#' **A Guide to the Project Management Body of Knowledge (PMBOK Guide)**.
#' Sixth Edition.
#'
#' Project Management Institute (2017)
#' **PMI Lexicon of Project Management Terms:** Version 3.2.
#'
#' Vanhoucke, M. (2009) **Measuring Time**:
#' Improving Project Performance Using Earned Value Management.
#' Springer-Verlag US.
#'
#' Vanhoucke, M. (2013) **Project Management with Dynamic Scheduling**:
#' Baseline Scheduling, Risk Analysis and Project Control.
#' Springer-Verlag Berlin Heidelberg.
#'
#' Vanhoucke, M. (2014) **Integrated Project Management and Control**:
#' First Comes the Theory, then the Practice.
#' Springer International Publishing Switzerland.
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
