#' @title R6 Class Representing a Schedule
#' @name Schedule
#' @aliases schedule
#'
#' @description
#'
#' This class is a representation of Precedence Diagramming Method (PDM).
#' PDM is a technique used for constructing a schedule model in which activities
#' are represented by nodes and are graphically linked by one or more logical
#' relationships to show the sequence in which the activities are to be performed.
#'
#' A schedule has activities and relations data-frames. With this class,
#' it is possible to apply critical path method
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
#' @author
#'   Rubens Jose Rosa (\email{rubens@@rubensjoserosa.com}),
#'   Marcos dos Santos (\email{marcosdossantos@@ime.eb.br}),
#'   Thiago Marques (\email{profestathimarques@@gmail.com})
#'
#' @seealso
#' On vignette package there is more information with examples about:
#' - Critical Path Method Package [criticalpath].
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
#' @import R6
#'
#' @export
Schedule <- R6::R6Class("Schedule",

  private = list(

    .activities = NULL,

    .relations = NULL,

    info = NULL,

    config = NULL,

    is_wholenumber = function(x, tol = .Machine$double.eps^0.5) {
      abs(x - round(x)) < tol
    },

    is_valid_data_frame = function(obj) {
      if(base::any(base::is.null(obj)))
        return(FALSE)

      if(base::any(base::is.na(obj)))
        return(FALSE)

      if(!base::is.data.frame(obj))
         return(FALSE)

      if(base::nrow(obj) == 0L)
        return(FALSE)

      TRUE
    },

    new_activity = function(nr_activities = 0L) {
      data.frame(
        id = integer(nr_activities),
        name = character(nr_activities),
        duration = integer(nr_activities),
        milestone = logical(nr_activities),
        critical = logical(nr_activities),
        ES = integer(nr_activities),
        EF = integer(nr_activities),
        LS = integer(nr_activities),
        LF = integer(nr_activities),
        total_float = integer(nr_activities),
        free_float = integer(nr_activities)
      )
    },

    new_relation = function(nr_relations = 0L) {
      data.frame(
        from = integer(nr_relations),
        to   = integer(nr_relations),
        type = character(nr_relations),
        lag = integer(nr_relations),
        critical = logical(nr_relations),
        ord = integer(nr_relations),
        i_from = integer(nr_relations),
        i_to = integer(nr_relations)
      )
    },

    new_info = function() {
      list(
        title = "",
        reference = "",

        nr_activities = 0L,
        has_any_activity = FALSE,

        nr_relations = 0L,
        has_any_relation = FALSE,

        duration = 0L
      )
    },

    new_config = function() {
      list()
    },

    activity_id_exist = function(id) {
      !base::is.na(base::match(id, private$.activities$id))
    },

    assert_activity_id_exist = function(id) {
      if(!private$activity_id_exist(id)){
        stop("Activity id must exist em activities list!")
      }
    },

    assert_activity_id_does_not_exist = function(id) {
      if(private$activity_id_exist(id)){
        stop("Activity id must NOT EXIST em activities list!")
      }
    },

    assert_activity_id_is_valid = function(activity_id) {
      if(base::is.null(activity_id)) {
        stop("Activity id cannot be NULL!")
      }
      if(base::is.na(activity_id)) {
        stop("Activity id cannot be NA!")
      }
      if(!private$is_wholenumber(activity_id)) {
        stop("Activity id must be a whole number!")
      }
    },

    assert_relation_exist = function(from, to) {
      from_exist <- private$.relations$from == from
      to_exist <- private$.relations$to == to
      u <- which(from_exist & to_exist)
      if(length(u) == 0L){
        stop(paste("Relation", from, "->", to, " must exist!"))
      }
    },

    assert_relation_does_not_exist = function(from, to) {
      from_exist <- private$.relations$from == from
      to_exist <- private$.relations$to == to
      u <- which(from_exist & to_exist)
      if(length(u) > 0L){
        stop(paste("Relations", from, "->", to, " must NOT exist!"))
      }
    },

    # Verify is a gantt object is of Gantt matrix class
    is_gantt = function (gantt)  {
      "Gantt" %in% class(gantt)
    },

    assert_is_gantt = function(gantt) {
      if (!private$is_gantt(gantt)) {
        stop("Not a Gantt matrix object!")
      }
    },

    topological_organization = function() {
      if(self$has_any_relation) {
        private$.relations <- private$.relations[order(private$.relations$ord), ]
      }

      # 2) Find starters activities
      ids <- setdiff(private$.activities$id, private$.relations$to)
      private$config$starters <- match(ids, private$.activities$id)

      # 3) Find ends activities
      ids <- setdiff(private$.activities$id, private$.relations$from)
      private$config$ends <- match(ids, private$.activities$id)

      # 4) Topological sorting
      if(self$nr_relations > 0L) {

        private$.relations$i_from <- match(private$.relations$from, private$.activities$id)
        private$.relations$i_to <- match(private$.relations$to, private$.activities$id)

        g <- igraph::graph_from_data_frame(
          private$.relations,
          TRUE,
          private$.activities
        )
        ts <- as.integer(igraph::topo_sort(g, "out"))
        sorted_activity_id <- private$.activities$id[ts]
        topo_order <- order(
          match(private$.relations$from, sorted_activity_id),
          match(private$.relations$to, sorted_activity_id)
        )
        private$.relations <- private$.relations[topo_order, ]
        rownames(private$.relations) <- 1:nrow(private$.relations)
      }

      # 5) Calculate topological levels

      # Progressive Level
      private$info$max_level <- 1L
      private$.activities$progr_level <- private$info$max_level
      # Forward calculate
      if(self$has_any_relation) {
        for(i in 1:self$nr_relations) {
          from <- private$.relations$i_from[i]
          to <- private$.relations$i_to[i]
          next_level <- private$.activities$progr_level[from] + 1L
          if(next_level > private$.activities$progr_level[to]) {
            private$.activities$progr_level[to] <- next_level
            if(next_level > private$info$max_level) {
              private$info$max_level <- next_level
            }
          }
        }
      }

      # Backward calculate
      private$.activities$regr_level <- private$info$max_level
      if(self$has_any_relation) {
        for(i in self$nr_relations:1) {
          from <- private$.relations$i_from[i]
          to <- private$.relations$i_to[i]
          prev_level <- private$.activities$regr_level[to] - 1L
          if(prev_level < private$.activities$regr_level[from]) {
            private$.activities$regr_level[from] <- prev_level
          }
        }
      }

      # Topological Float is the difference between regressive and progressive level
      private$.activities$topo_float <- private$.activities$regr_level - private$.activities$progr_level
    },

    calculate_critical_path = function() {

      relation_type <- list(

        forward = list(

          FS = function(pr, su, lag) {
            proximo =  private$.activities$EF[pr] + lag
            if (proximo > private$.activities$ES[su]) {
              private$.activities$ES[su] <- proximo
              private$.activities$EF[su] <- proximo + private$.activities$duration[su]
            }
          },

          FF = function(pr, su, lag) {
            proximo =  private$.activities$EF[pr] + lag
            if (proximo > private$.activities$EF[su]) {
              private$.activities$EF[su] <- proximo
              private$.activities$ES[su] <- proximo - private$.activities$duration[su]
            }
          },

          SS = function(pr, su, lag) {
            proximo =  private$.activities$ES[pr] + lag
            if (proximo > private$.activities$ES[su]) {
              private$.activities$ES[su] <- proximo
              private$.activities$EF[su] <- proximo + private$.activities$duration[su]
            }
          },

          SF = function(pr, su, lag) {
            proximo =  private$.activities$ES[pr] + lag
            if (proximo > private$.activities$EF[su]) {
              private$.activities$EF[su] <- proximo
              private$.activities$ES[su] <- proximo - private$.activities$duration[su]
            }
          }

        ),

        backward = list(

          FS = function(pr, su, lag) {
            proximo =  private$.activities$LS[su] - lag
            if (proximo < private$.activities$LF[pr]) {
              private$.activities$LF[pr] <- proximo
              private$.activities$LS[pr] <- proximo - private$.activities$duration[pr]
            }
          },

          FF = function(pr, su, lag) {
            proximo =  private$.activities$LF[su] - lag
            if (proximo < private$.activities$LF[pr]) {
              private$.activities$LF[pr] <- proximo
              private$.activities$LS[pr] <- proximo - private$.activities$duration[pr]
            }
          },

          SS = function(pr, su, lag) {
            proximo =  private$.activities$LS[su] - lag
            if (proximo < private$.activities$LS[pr]) {
              private$.activities$LS[pr] <- proximo
              private$.activities$LF[pr] <- proximo + private$.activities$duration[pr]
            }
          },

          SF = function(pr, su, lag) {
            proximo =  private$.activities$LF[su] - lag
            if (proximo < private$.activities$LS[pr]) {
              private$.activities$LS[pr] <- proximo
              private$.activities$LF[pr] <- proximo + private$.activities$duration[pr]
            }
          }

        )

      )

      ##############################

      ##acts <- private$.activities
      rela <- private$.relations

      # Define milestone
      private$.activities$milestone <- private$.activities$duration == 0L

      # Init early start and finish values
      private$.activities$ES <- -2000000000L
      private$.activities$EF <- -2000000000L

      # arrumarPeriodoDasAtividadesIniciais
      private$.activities$ES[private$config$starters] <- 0L
      private$.activities$EF[private$config$starters] <- private$.activities$duration[private$config$starters]

      # Forward calculate
      if(nrow(rela) > 0L) {
        for(i in 1:nrow(rela)) {
          type <- rela$type[i]
          relation_type$forward[[type]](
            rela$i_from[i],
            rela$i_to[i],
            rela$lag[i]
          )
        }
      }

      # Calculate project duration
      private$info$duration <- base::max(private$.activities$EF) - base::min(private$.activities$ES)

      # Init late start and finish values
      private$.activities$LF <- +2000000000L
      private$.activities$LS <- +2000000000L

      # arrumarPeriodoDasAtividadesFinais
      private$.activities$LF[private$config$ends] <- base::max(private$.activities$EF)
      private$.activities$LS[private$config$ends] <- private$.activities$LF[private$config$ends] - private$.activities$duration[private$config$ends]

      # Backward calculate
      if(nrow(rela) > 0L) {
        for(i in nrow(rela):1) {
          type <- rela$type[i]
          relation_type$backward[[type]](
            rela$i_from[i],
            rela$i_to[i],
            rela$lag[i]
          )
        }
      }

      # Calculate total_float
      private$.activities$total_float <- private$.activities$LS - private$.activities$ES

      # Calculate free_float
      private$.activities$free_float <- private$.activities$total_float  ## Acho que aqui tem que ser a TOTAL_FLOAT !!!
      if(nrow(private$.activities) > 0L) {
        for(i_from in 1:nrow(private$.activities)) {
          succesors <- rela[rela$i_from == i_from, ]
          if(nrow(succesors) > 0L) {
            for(j in 1:nrow(succesors)) {
              to <- succesors$i_to[j]
              ff <- private$.activities$ES[to] - private$.activities$EF[i_from]
              if(ff < private$.activities$free_float[i_from]) {
                private$.activities$free_float[i_from] <- ff
              }
            }
          } else {
            private$.activities$free_float[i_from] <- private$.activities$total_float[i_from]
          }
        }
      }

      # Identify critical activity
      private$.activities$critical <- private$.activities$total_float <= 0L

      # Identify critical relation
      rela$critical <- private$.activities$critical[rela$i_from] & private$.activities$critical[rela$i_to]


      ###################
      #####private$.activities <- acts
      private$.relations <- rela
    }

  ),

  active = list(

    #' @field title
    #' A project title for identification. It depends on
    #' user of the class. Its use are:
    #'    - \code{Sechedule$title <- "A title"}
    #'      - sets a title for a project.
    #'    - \code{Sechedule$title}
    #'      - gets the title of the project.
    #'
    title = function(value) {
      .Deprecated("sch_title")

      if(missing(value)) {
        return(private$info$title)
      } else {
        private$info$title = value
      }
      invisible(self)
    },

    #' @field reference
    #' A reference from project origin, for example, a book, a paper, a corporation,
    #' or nothing. Its uses are:
    #'    - \code{Sechedule$reference <- "A reference"}
    #'      - sets a reference for a project.
    #'    - \code{Sechedule$title}
    #'      - gets the reference of the project.
    #'
    reference = function(value) {
      .Deprecated("sch_reference")

      if(missing(value)) {
        return(private$info$reference)
      } else {
        private$info$reference = value
      }
      invisible(self)
    },

    #' @field has_any_activity
    #' A logical value that indicates if the schedule
    #' has any activity. A TRUE value means that the schedule has some
    #' activity; a FALSE, means that the schedule is empty.
    #'    - Usage: \code{Schedule$has_any_activity}
    #'
    has_any_activity = function(value) {
      .Deprecated("sch_has_any_activity")

      if(missing(value)) {
        return(self$nr_activities > 0L)
      }
      stop("Can't set `$has_any_activity`", call. = FALSE)
    },

    #' @field nr_activities
    #' Number of activities in a schedule as an integer value.
    #'    - Usage: \code{Schedule$nr_activities}
    #'
    nr_activities = function(value) {
      .Deprecated("sch_nr_activities")

      if(missing(value)) {
        return(private$info$nr_activities)
      }
      stop("Can't set `$nr_activities`", call. = FALSE)
    },

    #' @field activities
    #' Return a data frame with all activities of a schedule
    #' in an activity id order. This is the main information calculated by CPM.
    #' The data frame is formed by following structure:
    #'    - **id:** Activity id.
    #'    - **name:** The name of activity.
    #'    - **duration:** A number that represents the activity's duration.
    #'    - **milestone:** A milestone is an activity with zero duration.
    #'    This property indicates if an activity is a milestone or not:
    #'    \code{TRUE} indicates it is a milestone; \code{FALSE} indicates it is not.
    #'    - **critical:** A critical activity is one with total float minor or equal
    #'    to zero. This property indicates if an activity is critical:
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
    #'  - Usage: \code{Schedule$activities}
    #'
    activities = function(value) {
      .Deprecated("sch_activities")

      if(missing(value)) {
        if(self$has_any_activity) {
          return(private$.activities[order(private$.activities$id), ])
        } else {
          return(private$.activities)
        }
      }

      stop("Can't set `$activities`", call. = FALSE)
    },

    #' @field has_any_relation
    #' A logical value that indicates if the schedule
    #' has any relation. A TRUE value means that the schedule has some
    #' relation; a FALSE, means that the schedule does not have any relation.
    #'    - Usage: \code{Schedule$has_any_relation}
    #'
    has_any_relation = function(value) {
      .Deprecated("sch_has_any_relation")

      if(missing(value)) {
        return(private$info$has_any_relation)
      }
      stop("Can't set `$has_any_relation`", call. = FALSE)
    },

    #' @field nr_relations
    #' Number of relations in a schedule as an integer value.
    #'    - Usage: \code{Schedule$nr_relations}
    #'
    nr_relations = function(value) {
      .Deprecated("sch_nr_relations")

      if(missing(value)) {
        return(private$info$nr_relations)
      }
      stop("Can't set `$nr_relations`", call. = FALSE)
    },

    #' @field relations
    #' Return a data frame with all relations of a schedule
    #' in topological order. This is the main information calculated by CPM.
    #' The data frame is formed by following structure:
    #'    - **from:** Predecessor activity id from a relation.
    #'    - **to:** Successor activity id from a relation.
    #'    - **type:** The type of relation between activities.
    #'    Its value may be: FS, FF, SS, SF.
    #'    - **lag:** The time period between activity predecessor and
    #'    activity successor activity
    #'    - **critical:** A critical relation formed by two activity critical:
    #'    predecessor and successor.
    #'    \code{TRUE} indicates it is critical;
    #'    \code{FALSE} indicates it is not critical.
    #'    - **ord:** Indicates de order that the relation was added in the schedule.
    #'    - **i_from:** It is the index of predecessor activity in the
    #'    activities data frame.
    #'    - **i_to:** It is the index of successor activity in the
    #'    activities data frame.
    #'
    #'    - Usage: \code{Schedule$relations}
    #'
    relations = function(value) {
      .Deprecated("sch_relations")

      if(missing(value)) {
        return(private$.relations)
      }

      stop("Can't set `$relations`", call. = FALSE)
    },

    #' @field duration
    #' An integer value that indicates the duration of a schedule.
    #'
    duration = function(value) {
      .Deprecated("sch_duration")

      if(base::missing(value)) {
        return(private$info$duration)
      }
      stop("Can't set `$duration`", call. = FALSE)
    }

  ),

  public = list(

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
    #' - **id:** Activity id. It is an integer number that
    #'  must be unique within a schedule.
    #' - **name:** Activity name. It may be empty.
    #' - **duration:** Activity duration. It is integer number without unit time.
    #' It may be zero.
    #' @param relations Data frame with precedence relations between activities.
    #' If it is informed, the activities has to be informed too.
    #' If it is not informed, the schedule will be created without any relation.
    #' It is formed by predecessor activity e successor activity.
    #' Its structure is:
    #' - **from:** The id of predecessor activity. Must exist an activity with from id.
    #' - **to:** The id of successor activity. Must exist an activity with to id.
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
    #'
    initialize = function(activities=NULL, relations=NULL) {
      .Deprecated("sch_new")

      exist_activites <- private$is_valid_data_frame(activities)
      exist_relations <- private$is_valid_data_frame(relations)
      if(exist_relations) {
        # If exist any relation, the activities must exist too!
        if(!exist_activites) {
          stop("Activities cannot be empty, because cannot exist relations without activities.")
        }
      }

      private$.activities <- private$new_activity()
      private$.relations <- private$new_relation()
      private$info <- private$new_info()
      private$config <- private$new_config()

      if(exist_activites) {
        self$add_activities(activities)
        if(exist_relations) {
          self$add_relations(relations)
        }
      }
    },

    #' @description Add an activity to a schedule.
    #' @param id Activity id that will be used to make
    #' relation between activities. It must be unique.
    #' @param name The name of activity. The default is an empty string.
    #' @param duration A number that represents the activity's duration.
    #' It must be equal or greater than zero. The default value is zero.
    #' @return A Schedule object with an activity added and
    #' the critical path calculated.
    #'
    add_activity = function(id, name="", duration = 0L) {
      .Deprecated("sch_add_activity")

      private$assert_activity_id_is_valid(id)
      private$assert_activity_id_does_not_exist(id)

      old_activities <- private$.activities

      new_activity <- data.frame(
        id        = id,
        name      = ifelse(base::is.null(name), base::paste0("a", id), name),
        duration  = ifelse(base::is.null(duration), 0L, duration),
        milestone = FALSE,
        critical = FALSE,
        ES =  -2000000000L,
        EF =  -2000000000L,
        LS =  +2000000000L,
        LF =  +2000000000L,
        total_float = 0L,
        free_float = 0L,
        progr_level = -2000000000L,
        regr_level = +2000000000L,
        topo_float = 0L
      )

      private$.activities <- rbind(old_activities, new_activity)
      private$info$nr_activities <- nrow(private$.activities)
      private$info$has_any_activity <- TRUE

      ## Topological organization
      private$topological_organization()

      ## Critical Path
      private$calculate_critical_path()

      invisible(self)
    },

    #' @description Add activities from a data frame to a schedule.
    #' @param activities A data frame with the activities to be added.
    #' @return A Schedule object with activities added and CPM calculated.
    #'
    add_activities = function(activities) {
      .Deprecated("sch_add_activities")

      if(!private$is_valid_data_frame(activities)) {
        stop("Activities is empty.")
      }

      for(i in 1:base::nrow(activities)) {
        self$add_activity(
          activities$id[i],
          activities$name[i],
          activities$duration[i]
        )
      }
      invisible(self)
    },

    #' @description
    #' Gets an activity by id. It returns a data
    #' frame with one line about activity.
    #' @param id An activity id as defined by the user.
    #' @return A data frame with one line with the activity,
    #' or an error if activity id doesn't exist.
    #'
    get_activity = function(id) {
      .Deprecated("sch_get_activity")

      private$assert_activity_id_exist(id)
      private$.activities[match(id, private$.activities$id), ]
    },

    #' @description Add a relation to a schedule.
    #' @param from
    #' The id of predecessor activity.
    #' Must exist an activity with from.
    #' @param to
    #' The id of successor activity.
    #' Must exist an activity with to.
    #' @param type
    #' Specifies the type of relation between activities.
    #' The default type is FS and its value may be: FS, FF, SS, SF, that means:
    #' If type is not defined, it is assumed to be FS.
    #'
    #' **FS:** Finish-Start relation.
    #' Activity 'to' id can only start after the finish of activity 'from' id.
    #'
    #' **FF:** Finish-Finish relation.
    #' Activity 'to' id must finish together with activity 'from' id.
    #'
    #' **SS:** Start-Start relation.
    #' Activity 'to' id must start together with activity 'from' id.
    #'
    #' **SF:** Start-Finish relation.
    #' Activity 'to' id must finish when activity 'from' id starts.
    #'
    #' @param lag
    #' The time period between activities that the successor activity
    #'  'to' must be advanced after activity 'from' has been finished.
    #'  The value may be negative, in such case, the activity 'to' will be
    #'  anticipated 'lag' time periods.
    #' It must be an integer, less than, equal or greater than zero.
    #' If lag is not defined, it is assumed to be zero.
    #' @return A Schedule object with CPM parameters calculated.
    #'
    add_relation = function(from, to, type = "FS", lag = 0L) {
      .Deprecated("sch_add_relation")

      private$assert_activity_id_is_valid(from)
      private$assert_activity_id_exist(from)

      private$assert_activity_id_is_valid(to)
      private$assert_activity_id_exist(to)

      if(base::is.na(base::match(type, c("FS", "FF", "SS", "SF" )))) {
        stop("type must be FS, FF, SS or SF!")
      }

      old_relations <- private$.relations

      new_relation = data.frame(
        from = from,
        to = to,
        type = type,
        lag = lag,
        critical = FALSE,
        ord = base::nrow(old_relations) + 1L,
        i_from = NA_integer_,
        i_to = NA_integer_
      )

      private$.relations <- base::rbind(old_relations, new_relation)

      private$info$nr_relations <- base::nrow(private$.relations)
      private$info$has_any_relation <- private$info$nr_relations > 0L

      ## Topological organization
      private$topological_organization()

      ## Critical Path
      private$calculate_critical_path()

      invisible(self)
    },

    #' @description Add relations between activities from a data frame
    #' to a schedule.
    #' @param relations A data frame with the relations to be added.
    #' @return A Schedule object with relations added and CPM calculated.
    #'
    add_relations = function(relations) {
      .Deprecated("sch_add_relations")

      if(!private$is_valid_data_frame(relations)) {
        stop("Relations is empty.")
      }

      if(base::is.null(relations$type)) {
        relations$type <- "FS"
      }
      if(base::is.null(relations$lag)) {
        relations$lag <- 0L
      }
      relations$critical <- FALSE
      relations$redundant <- FALSE
      relations$from <- as.integer(relations$from)
      relations$to <- as.integer(relations$to)
      relations$lag <- as.integer(relations$lag)
      for(i in 1:base::nrow(relations)) {
        self$add_relation(
          relations$from[i],
          relations$to[i],
          relations$type[i],
          relations$lag[i]
        )
      }
      invisible(self)
    },

    #' @description Add an activity and her relations to a schedule.
    #' @param id
    #' Activity id. The id will be used to make relation between activities.
    #' @param name The name of activity.
    #' @param duration A number that represents the activity's duration.
    #' It must be equal or greater than zero.
    #' @param relations_id A vector of ids such that will be linked with activity id.
    #' It may be relations of successor or predecessors.
    #' @param direction Direction of relations_id: It may be "succ" or "pred".
    #' If dir="succ" the relations_id will be the successor of the activity.
    #' If dir="pred" the relations_id will be the predecessor of the activity.
    #' @return A Schedule object.
    #'
    add_act_rel = function(id, name, duration, relations_id=c(), direction="succ") {
      .Deprecated("sch_add_activity")

      if(direction != "succ" && direction != "pred") {
        msg <- base::paste(
          "Invalid '", direction, "' direction!",
          "It must be 'succ' or 'pred'!"
          )
        stop(msg)
      }

      self$add_activity(id, name, duration)

      n <- length(relations_id)
      if(n > 0L) {
        if(any(duplicated(relations_id))) {
          stop("Must NOT EXISTS duplicated id in relations_id!")
        }

        #1 Add temp relations
        if(base::is.null(private$config$temp_relations)) {
          private$config$temp_relations <- data.frame(
            from = integer(),
            to   = integer(),
            type = character(),
            lag = integer(),
            critical = logical(),
            redundant = logical(),
            ord = integer(),
            i_from = integer(),
            i_to = integer()
          )
        }
        if(direction == "succ") {
          for(i in 1:n) {
            private$config$temp_relations <- rbind(
              private$config$temp_relations,
              data.frame(
                from = id,
                to   = relations_id[i],
                type = "FS",
                lag = 0L,
                critical = FALSE,
                redundant = FALSE,
                ord = NA_integer_,
                i_from = NA_integer_,
                i_to = NA_integer_
              )
            )
          }
        } else if(direction == "pred") {
          for(i in 1:n) {
            private$config$temp_relations <- rbind(
              private$config$temp_relations,
              data.frame(
                from = relations_id[i],
                to   = id,
                type = "FS",
                lag = 0L,
                critical = FALSE,
                redundant = FALSE,
                ord = NA_integer_,
                i_from = NA_integer_,
                i_to = NA_integer_
              )
            )
          }
        }
      }

      #2 Add temp_relations to relations if activity id exist

      temp_n <- ifelse(
        base::is.null(private$config$temp_relations),
        0L,
        nrow(private$config$temp_relations)
      )
      if(temp_n > 0L) {
        private$config$temp_relations$keep <- TRUE
        for(i in 1:temp_n) {
          from <- private$config$temp_relations$from[i]
          to   <- private$config$temp_relations$to[i]
          type <- private$config$temp_relations$type[i]
          lag  <- private$config$temp_relations$lag[i]
          j <- base::match(from, private$.activities$id)
          k <- base::match(to, private$.activities$id)
          if(!is.na(j) && !is.na(k)) {
            self$add_relation(from, to, type, lag)
            private$config$temp_relations$keep[i] <- FALSE
          }
        }

        #2 Remove temp_relations add
        if(base::any(private$config$temp_relations$keep)) {
          private$config$temp_relations <- private$config$temp_relations[private$config$temp_relations$keep, ]
          private$config$temp_relations$keep <- NULL
        } else {
          private$config$temp_relations <- NULL
        }
      }
      invisible(self)
    },

    #' @description Print a description of the class
    #' @param ... Variable parameters
    #' @return A String .
    #'
    print = function(...) {
      cat("Schedule: \n")
      cat("      Title: ", private$info$title, "\n", sep = "")
      cat("  Reference: ", private$info$reference, "\n", sep = "")

      invisible(self)
    },

    #' @description
    #' List all successors from an activity: direct and indirect successors.
    #' @param id Activity id to be listed.
    #' @param ign_to A relation to be ignored: id -> ign_to.
    #' Activities from this relation will be ignored.
    #' @return A vector whith all activities ids.
    #'
    all_successors = function(id, ign_to=NULL) {
      .Deprecated("sch_all_successors")

      private$assert_activity_id_exist(id)
      if(!base::is.null(ign_to)) {
        private$assert_activity_id_exist(ign_to)
        private$assert_relation_exist(id, ign_to)
      }

      temp_relation <- private$.relations
      if(!base::is.null(ign_to)) {
        u <- which(temp_relation$from == id & temp_relation$to == ign_to)
        temp_relation <- temp_relation[-u, ]
      }

      a_list <- c(id)
      for(i in 1:nrow(temp_relation)) {
        if(temp_relation$from[i] %in% a_list) {
          a_list <- c(a_list, temp_relation$to[i])
        }
      }
      base::rev(base::unique(base::rev(a_list[-1L])))
    },

    #' @description
    #' List all predecessors from an activity: direct or indirect predecessors.
    #' @param id Activity id to be listed.
    #' @param ign_from A relation to be ignored: ign_from -> id.
    #' Activities from this relation will be ignored.
    #' @return A vector with all activities ids.
    all_predecessors = function(id, ign_from=NULL) {
      .Deprecated("sch_all_predecessors")

      private$assert_activity_id_exist(id)
      if(!base::is.null(ign_from)) {
        private$assert_activity_id_exist(ign_from)
        private$assert_relation_exist(ign_from, id)
      }

      temp_relation <- private$.relations
      if(!base::is.null(ign_from)) {
        u <- base::which(temp_relation$from == ign_from & temp_relation$to == id)
        temp_relation <- temp_relation[-u, ]
      }

      a_list <- c(id)
      for(i in base::nrow(temp_relation):1) {
        if(temp_relation$to[i] %in% a_list) {
          a_list <- c(a_list, temp_relation$from[i])
        }
      }
      rev(unique(rev(a_list[-1L])))
    },

    #' @description
    #' Verify if a relation between two activities is redundant.
    #' A relation A->C is redundant if there are A->C, A->B, B->C relations.
    #' @param id_from From activity id.
    #' @param id_to To activity id.
    #' @return A logical \code{TRUE} if an arc is redundant;
    #' \code{FALSE} if it is not.
    #'
    is_redundant = function(id_from, id_to) {
      .Deprecated("sch_is_redundant")

      private$assert_activity_id_exist(id_from)
      private$assert_activity_id_exist(id_to)
      private$assert_relation_exist(id_from, id_to)

      succ <- self$all_successors(id_from, id_to)
      id_to %in% succ
    },

    #' @description
    #' Change activities duration and calculate critical path.
    #' This way is faster than creating a new schedule with new durations.
    #' @param new_durations A vector with new activities' duration.
    #' @return A Schedule object.
    #'
    change_durations = function(new_durations) {
      .Deprecated("sch_change_activities_duration")

      # TODO verificar se os tamanhos são os mesmos
      # TODO verificar se não tem nenhum NULL ou NA
      private$.activities$duration <- as.integer(new_durations)
      private$calculate_critical_path()

      invisible(self)
    },

    #' @description
    #' Create a matrix that represents a Gantt chart,
    #' a matrix where "1" indicates that an activity is planned to be
    #' in execution.
    #'
    #' In this matrix, the rows represent activities,
    #' whereas the columns represents the activity execution period.
    #' So, the number of columns is equal to project duration.
    #' @return A matrix where "1" indicates that an activity is in execution.
    #'
    gantt_matrix = function() {
      .Deprecated("sch_gantt_matrix")

      if(self$duration == 0L) {
        stop("There is no Gantt Matrix for a schedule with zero duration!")
      }
      atvs <- private$.activities
      iii <- which(atvs$duration > 0L)

      delta <- abs(min(atvs$ES))

      duration <- private$info$duration
      qtdatvs <- private$info$nr_activities
      gantt <- base::matrix(base::integer(duration * qtdatvs), nrow = qtdatvs)
      for (i in iii) {
        start  <- atvs$ES[i] + delta + 1L
        finish <- atvs$EF[i] + delta
        gantt[i, start:finish] <- 1L
      }
      class(gantt) <- base::unique(c("Gantt", class(gantt)))

      row.names(gantt) <- atvs$id
      colnames(gantt) <- (min(atvs$ES) + 1L) : (max(atvs$EF))

      return(gantt)
    },

    #' @description
    #' Transform a Gantt matrix in x, y coordinates and the weight one.
    #' Each point greater than zero in a Gantt matrix becomes a x, y coordinate.
    #' @param gantt A Gantt Matrix. If it is not informed, it will use
    #' \code{gantt_matrix()} before this function.
    #' @return A matrix x, y and weight.
    #'
    xy_gantt_matrix = function(gantt = NULL) {
      .Deprecated("sch_xy_gantt_matrix")

      if(base::is.null(gantt)) {
        gantt <- self$gantt_matrix()
      } else {
        private$assert_is_gantt(gantt)
      }
      qtdatvs <- base::nrow(gantt)
      pdur <- base::ncol(gantt)
      v <- as.integer(t(gantt))
      ii <- which(v > 0L) - 1L
      y <- base::floor(ii / pdur)
      x <- ii - y * pdur
      peso <- v[ii + 1L]
      base::matrix(c(x + 1L, y + 1L, peso), ncol = 3L)
    },


    #' @description **SP Serial or Parallel Topological Indicator:**
    #' It shows the closeness of a network to a serial or parallel graph.
    #' As the network becomes serial, the SP increase, until one,
    #' when the network totally serial.
    #' @return A number between 0 and 1, inclusive.
    #'
    topoi_sp = function() {
      .Deprecated("sch_topoi_sp")

      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0L) {
        return(NA)
      }

      if(nr_act == 1L) {
        return(1)
      }

      ((max_level - 1) / (nr_act - 1))
    },

    #' @description **AD Activity Distribution Topological Indicator:**
    #' Measures the distribution of the activities over the levels.
    #' If AD is approximately equal zero, each level has same numbers of activities.
    #' Otherwise, if AD is equal one, the quantity of each level is not
    #' uniformly distributed.
    #' @return A number between 0 and 1, inclusive.
    #'
    topoi_ad = function() {
      .Deprecated("sch_topoi_ad")

      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0L) {
        return(NA)
      }

      if(max_level == 1L || max_level == nr_act) {
        return(0);
      }

      # Activities quantities by level: they are called width, width by level
      wi <- table(private$.activities$progr_level)
      # w mean
      wbar <- nr_act / max_level
      absolute_mean_deviation <- base::sum(base::abs(wi - wbar))

      absolute_mean_deviation / (2 * (max_level - 1) * (wbar - 1));
    },

    #' @description **LA Length of Arcs Topological Indicator:**
    #' Measures the presence of long arcs based on the difference between
    #' the progressive level of the end activity and the start node
    #' of each relation.
    #' If LA is approximately equal zero, the progressive level between
    #' activities is as far as possible.
    #' Otherwise, if LA is equal one, the relation distance are one.
    #' @return A number between 0 and 1, inclusive.
    #'
    topoi_la = function() {
      .Deprecated("sch_topoi_la")

      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0L) {
        return(NA)
      }

      # Activities quantities by level: they are called width, width by level
      wi <- as.integer(base::table(private$.activities$progr_level))

      D <- base::sum(wi[-base::length(wi)] * wi[-1L])

      if(D == nr_act - wi[1L]) {
        return(1)
      }

      len <- 1L
      levels_from <- private$.activities$progr_level[private$.relations$i_from]
      levels_to <- private$.activities$progr_level[private$.relations$i_to]
      arcs_qty <- base::sum(levels_to - levels_from == len)

      (arcs_qty - nr_act + wi[1L]) / (D - nr_act + wi[1L])
    },

    #' @description **TF Topological Float Indicator:**
    #' Measures the topological float of each activity.
    #' If TF = 0, there is no float between activities.
    #' If TF = 1, there is float between activities
    #' and they be shift without affecting other activities.
    #' @return A number between 0 and 1, inclusive.
    #'
    topoi_tf = function() {
      .Deprecated("sch_topoi_tf")

      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0L) {
        return(NA)
      }

      if(max_level == 1L || max_level == nr_act) {
        return(0);
      }

      level_diff <- private$.activities$regr_level - private$.activities$progr_level

      base::sum(level_diff) / ((max_level - 1) * (nr_act - max_level))
    }

  ),

  lock_class = TRUE

)
