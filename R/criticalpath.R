#' criticalpath: Critical Path Method R Implementation
#'
#' `criticalpath` package is an R implementation of the
#' Critical Path Method (CPM). CPM is a method used to estimate
#' the minimum project duration and  determine the amount of scheduling
#' flexibility
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
#' - How to create a schedule:
#'   - Create a new schedule without any information.
#'     - [sch_new()]
#'   - Add activities and relations together to an schedule.
#'     - [sch_add_activities()]
#'     - [sch_add_relations()]
#'   - Add activities to a schedule.
#'     - [sch_add_activity()]
#'   - Add relations to a schedule.
#'     - [sch_add_relation()]
#'  - How to get schedule information:
#'    - Title
#'      - [sch_title()]
#'    - Reference
#'      - [sch_reference()]
#'    - Duration
#'      - [sch_duration()]
#'  - How to get activities properties:
#'    - Activity Properties.
#'      - [sch_activities()]
#'      - [sch_get_activity()]
#'    - Gantt Matrix.
#'      - [sch_gantt_matrix()]
#'      - [sch_xy_gantt_matrix()]
#'  - How to change activities duration:
#'    - Change Activities Duration.
#'      - [sch_change_activities_duration()]
#'  - How to get relations properties:
#'    - Relation Properties
#'      - [sch_relations()]
#'    - Successors and Predecessors.
#'      - [sch_all_successors()]
#'      - [sch_successors()]
#'      - [sch_all_predecessors()]
#'      - [sch_predecessors()]
#'  - How to get topological properties:
#'    - Topological Indicators.
#'      - [sch_topoi_sp()]
#'      - [sch_topoi_ad()]
#'      - [sch_topoi_la()]
#'      - [sch_topoi_tf()]
#'
#' @name criticalpath
#' @docType package
NULL
