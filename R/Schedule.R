#' @title R6 Class Representing a Schedule
#' @name Schedule
#' @aliases schedule
#'
#' @export
#'
#' @description
#' A schedule has activities and relations data-frames.
#' With a Schedule, you can calculate the project critical path.
#'
#' @details
#' Alguns detalhes a mais.
#' Vamos ver no que vai dar.
#'
#' @section More Information:
#' There is a **See Also** section in the end of this page for more information.
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
#' @author Rubens Jose Rosa (rubens@rubensjoserosa.com),
#' Marcos dos Santos, Thiago Marques
#'
#' @seealso
#' Copiar o seealso do pacote **criticalpath**
#'
#' E, claro, apontar para o pacote!
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

      if(base::nrow(obj) == 0)
        return(FALSE)

      TRUE
    },

    new_activity = function(nr_activities=0) {
      data.frame(
        id = integer(nr_activities),
        name = character(nr_activities),
        duration = numeric(nr_activities),
        milestone = logical(nr_activities),
        critical = logical(nr_activities),
        ES = numeric(nr_activities),
        EF = numeric(nr_activities),
        LS = numeric(nr_activities),
        LF = numeric(nr_activities),
        total_float = numeric(nr_activities),
        free_float = numeric(nr_activities)
      )
    },

    new_relation = function(nr_relations=0) {
      data.frame(
        from = numeric(nr_relations),
        to   = numeric(nr_relations),
        type = character(nr_relations),
        lag = numeric(nr_relations),
        critical = logical(nr_relations),
        ord = numeric(nr_relations),
        i_from = numeric(nr_relations),
        i_to = numeric(nr_relations)
      )
    },

    new_info = function() {
      list(
        title = "",
        reference = "",

        nr_activities = 0,
        has_any_activity = FALSE,

        nr_relations = 0,
        has_any_relation = FALSE,

        duration = 0
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
      if(length(u) == 0){
        stop(paste("Relation", from, "->", to, " must exist!"))
      }
    },

    assert_relation_does_not_exist = function(from, to) {
      from_exist <- private$.relations$from == from
      to_exist <- private$.relations$to == to
      u <- which(from_exist & to_exist)
      if(length(u) > 0){
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
      if(self$nr_relations > 0) {

        private$.relations$i_from <- match(private$.relations$from, private$.activities$id)
        private$.relations$i_to <- match(private$.relations$to, private$.activities$id)

        g <- igraph::graph_from_data_frame(
          private$.relations,
          TRUE,
          private$.activities
        )
        ts <- as.numeric(igraph::topo_sort(g, "out"))
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
      private$info$max_level <- 1
      private$.activities$progr_level <- private$info$max_level
      # Forward calculate
      if(self$has_any_relation) {
        for(i in 1:self$nr_relations) {
          from <- private$.relations$i_from[i]
          to <- private$.relations$i_to[i]
          next_level <- private$.activities$progr_level[from] + 1
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
          prev_level <- private$.activities$regr_level[to] - 1
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

          FS = function(acts, pr, su, lag) {
            proximo =  acts$EF[pr] + lag
            if (proximo > acts$ES[su]) {
              acts$ES[su] <<- proximo
              acts$EF[su] <<- proximo + acts$duration[su]
            }
          },

          FF = function(acts, pr, su, lag) {
            proximo =  acts$EF[pr] + lag
            if (proximo > acts$EF[su]) {
              acts$EF[su] <<- proximo
              acts$ES[su] <<- proximo - acts$duration[su]
            }
          },

          SS = function(acts, pr, su, lag) {
            proximo =  acts$ES[pr] + lag
            if (proximo > acts$ES[su]) {
              acts$ES[su] <<- proximo
              acts$EF[su] <<- proximo + acts$duration[su]
            }
          },

          SF = function(acts, pr, su, lag) {
            proximo =  acts$ES[pr] + lag
            if (proximo > acts$EF[su]) {
              acts$EF[su] <<- proximo
              acts$ES[su] <<- proximo - acts$duration[su]
            }
          }

        ),

        backward = list(

          FS = function(acts, pr, su, lag) {
            proximo =  acts$LS[su] - lag
            if (proximo < acts$LF[pr]) {
              acts$LF[pr] <<- proximo
              acts$LS[pr] <<- proximo - acts$duration[pr]
            }
          },

          FF = function(acts, pr, su, lag) {
            proximo =  acts$LF[su] - lag
            if (proximo < acts$LF[pr]) {
              acts$LF[pr] <<- proximo
              acts$LS[pr] <<- proximo - acts$duration[pr]
            }
          },

          SS = function(acts, pr, su, lag) {
            proximo =  acts$LS[su] - lag
            if (proximo < acts$LS[pr]) {
              acts$LS[pr] <<- proximo
              acts$LF[pr] <<- proximo + acts$duration[pr]
            }
          },

          SF = function(acts, pr, su, lag) {
            proximo =  acts$LF[su] - lag
            if (proximo < acts$LS[pr]) {
              acts$LS[pr] <<- proximo
              acts$LF[pr] <<- proximo + acts$duration[pr]
            }
          }

        )

      )

      ##############################

      acts <- private$.activities
      rela <- private$.relations

      # Define milestone
      acts$milestone <- acts$duration == 0

      # Init early start and finish values
      acts$ES <- -Inf
      acts$EF <- -Inf

      # arrumarPeriodoDasAtividadesIniciais
      acts$ES[private$config$starters] <- 0
      acts$EF[private$config$starters] <- acts$duration[private$config$starters]

      # Forward calculate
      if(nrow(rela) > 0) {
        for(i in 1:nrow(rela)) {
          type <- rela$type[i]
          relation_type$forward[[type]](
            acts,
            rela$i_from[i],
            rela$i_to[i],
            rela$lag[i]
          )
        }
      }

      # Calculate project duration
      private$info$duration <- base::max(acts$EF) - base::min(acts$ES)

      # Init late start and finish values
      acts$LF <- +Inf
      acts$LS <- +Inf

      # arrumarPeriodoDasAtividadesFinais
      acts$LF[private$config$ends] <- base::max(acts$EF)
      acts$LS[private$config$ends] <- acts$LF[private$config$ends] - acts$duration[private$config$ends]

      # Backward calculate
      if(nrow(rela) > 0) {
        for(i in nrow(rela):1) {
          type <- rela$type[i]
          relation_type$backward[[type]](
            acts,
            rela$i_from[i],
            rela$i_to[i],
            rela$lag[i]
          )
        }
      }

      # Calculate total_float
      acts$total_float <- acts$LS - acts$ES

      # Calculate free_float
      acts$free_float <- acts$total_float  ## Acho que aqui tem que ser a TOTAL_FLOAT !!!
      #acts$free_float[private$config$ends] <- 0
      if(nrow(acts) > 0) {
        for(i_from in 1:nrow(acts)) {
          succesors <- rela[rela$i_from == i_from, ]
          if(nrow(succesors) > 0) {
            for(j in 1:nrow(succesors)) {
              to <- succesors$i_to[j]
              ff <- acts$ES[to] - acts$EF[i_from]
              if(ff < acts$free_float[i_from]) {
                acts$free_float[i_from] <- ff
              }
            }
          } else {
            acts$free_float[i_from] <- acts$total_float[i_from]
          }
        }
      }

      # Identify critical activity
      acts$critical <- acts$total_float <= 0

      # Identify critical relation
      rela$critical <- acts$critical[rela$i_from] & acts$critical[rela$i_to]


      ###################
      private$.activities <- acts
      private$.relations <- rela
    }

  ),

  active = list(

    #' @field title
    #' A project title for identification.
    title = function(value) {
      if(missing(value)) {
        return(private$info$title)
      } else {
        private$info$title = value
      }
      invisible(self)
    },

    #' @field reference
    #' A reference from project origin,
    #' for example, a book, a paper, a corporation, or nothing.
    reference = function(value) {
      if(missing(value)) {
        return(private$info$reference)
      } else {
        private$info$reference = value
      }
      invisible(self)
    },

    #' @field activities
    #' Return activities in a schedule as data frame.
    activities = function(value) {
      if(missing(value)) {
        if(self$has_any_activity) {
          return(private$.activities[order(private$.activities$id), ])
        } else {
          return(private$.activities)
        }
      }

      stop("Can't set `$activities`", call. = FALSE)
    },

    #' @field nr_activities
    #' Number of activities in a schedule as an integer value.
    nr_activities = function(value) {
      if(missing(value)) {
        return(private$info$nr_activities)
      }
      stop("Can't set `$nr_activities`", call. = FALSE)
    },

    #' @field has_any_activity
    #' A logical value that indicate if the schedule has any activity.
    has_any_activity = function(value) {
      if(missing(value)) {
        return(self$nr_activities > 0)
      }
      stop("Can't set `$has_any_activity`", call. = FALSE)
    },

    #' @field relations
    #' Return relations in a schedule as data frame.
    relations = function(value) {
      if(missing(value)) {
        if(self$has_any_relation) {
          return(private$.relations[order(private$.relations$ord), ])
        } else {
          return(private$.relations)
        }
      }

      stop("Can't set `$relations`", call. = FALSE)
    },

    #' @field nr_relations
    #' Number of relations (arcs) in a schedule as an integer value.
    nr_relations = function(value) {
      if(missing(value)) {
        return(private$info$nr_relations)
      }
      stop("Can't set `$nr_relations`", call. = FALSE)
    },

    #' @field has_any_relation
    #' A logical value that indicate if the schedule has any relation.
    has_any_relation = function(value) {
      if(missing(value)) {
        return(private$info$has_any_relation)
      }
      stop("Can't set `$has_any_relation`", call. = FALSE)
    },

    #' @field duration
    #' An integer value that indicate the duration of a schedule.
    duration = function(value) {
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
    initialize = function(activities=NULL, relations=NULL) {
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
    #' It must be equal or grater than zero. The default value is zero.
    #' @return A Schedule object with an activity added and
    #' the critical path calculated.
    add_activity = function(id, name="", duration=0) {
      private$assert_activity_id_is_valid(id)
      private$assert_activity_id_does_not_exist(id)

      old_activities <- private$.activities

      new_activity <- data.frame(
        id        = id,
        name      = ifelse(base::is.null(name), base::paste0("a", id), name),
        duration  = ifelse(base::is.null(duration), 0, duration),
        milestone = FALSE,
        critical = FALSE,
        ES =  -Inf,
        EF =  -Inf,
        LS =  +Inf,
        LF =  +Inf,
        total_float = 0,
        free_float = 0,
        progr_level = -Inf,
        regr_level = +Inf,
        topo_float = 0
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
    add_activities = function(activities) {
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

    #' @description Get a activity by id.
    #' @param id An activity id as defined by the user.
    #' @return A data frame with one line with the activity,
    #' or an error if activity id doesn't exist.
    get_activity = function(id) {
      private$assert_activity_id_exist(id)
      private$.activities[match(id, private$.activities$id), ]
    },

    #' @description Add a relation to a schedule.
    #' @param from
    #' The id of predecessor activity.
    #' Must exist a activity with from.
    #' @param to
    #' The id of successor activity.
    #' Must exist a activity with to.
    #' @param type
    #' Specifies the type of relation between activities.
    #' The default type is FS and its value may be: FS, FF, SS, SF, that means:
    #' If type is not defined, it is assumed to be FS.
    #'
    #' **FS:** Finish-Start relation.
    #' Activity to can only start after the finish of activity from.
    #'
    #' **FF:** Finish-Finish relation.
    #' Activity to must finish together with activity from.
    #'
    #' **SS:** Start-Start relation.
    #' Activity to must start together with activity from.
    #'
    #' **SF:** Start-Finish relation.
    #' Activity to must finish when activity from starts.
    #'
    #' @param lag
    #' If lag is not defined, it is assumed to be zero.
    #' The time period between activities that the successor activity
    #' must be advanced, or lated, after activity from.
    #' It must be an integer, less than, equal or greater than zero.
    #' @return A Schedule object with CPM parameters calculated.
    add_relation = function(from, to, type="FS", lag=0) {
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
        ord = base::nrow(old_relations) + 1,
        i_from = NA,
        i_to = NA
      )

      private$.relations <- base::rbind(old_relations, new_relation)

      private$info$nr_relations <- base::nrow(private$.relations)
      private$info$has_any_relation <- private$info$nr_relations > 0

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
    add_relations = function(relations) {
      if(!private$is_valid_data_frame(relations)) {
        stop("Relations is empty.")
      }

      if(base::is.null(relations$type)) {
        relations$type <- "FS"
      }
      if(base::is.null(relations$lag)) {
        relations$lag <- 0
      }
      relations$critical <- FALSE
      relations$redundant <- FALSE
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
    #' It must be equal or grater than zero.
    #' @param relations_id A vector of ids such that will be linked with activity id.
    #' It may be a relations of successor or predecessors.
    #' @param direction Direction of relations_id: It may be "succ" or "pred".
    #' If dir="succ" the relations_id will be the successor of the activity.
    #' If dir="pred" the relations_id will be the predecessor of the activity.
    #' @return A Schedule object.
    add_act_rel = function(id, name, duration, relations_id=c(), direction="succ") {
      if(direction != "succ" && direction != "pred") {
        msg <- base::paste(
          "Invalid '", direction, "' direction!",
          "It must be 'succ' or 'pred'!"
          )
        stop(msg)
      }

      self$add_activity(id, name, duration)

      n <- length(relations_id)
      if(n > 0) {
        if(any(duplicated(relations_id))) {
          stop("Must NOT EXISTS duplicated id in relations_id!")
        }

        #1 Add temp relations
        if(base::is.null(private$config$temp_relations)) {
          private$config$temp_relations <- data.frame(
            from = numeric(),
            to   = numeric(),
            type = character(),
            lag = numeric(),
            critical = logical(),
            redundant = logical(),
            ord = numeric(),
            i_from = numeric(),
            i_to = numeric()
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
                lag = 0,
                critical = FALSE,
                redundant = FALSE,
                ord = NA,
                i_from = NA,
                i_to = NA
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
                lag = 0,
                critical = FALSE,
                redundant = FALSE,
                ord = NA,
                i_from = NA,
                i_to = NA
              )
            )
          }
        }
      }

      #2 Add temp_relations to relations if activity id exist

      temp_n <- ifelse(
        base::is.null(private$config$temp_relations),
        0,
        nrow(private$config$temp_relations)
      )
      if(temp_n > 0) {
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
    #' @return A Schedule object.
    print = function(...) {
      cat("Schedule: \n")
      cat("      Title: ", private$info$title, "\n", sep = "")
      cat("  Reference: ", private$info$reference, "\n", sep = "")

      invisible(self)
    },

    #' @description List all successors from an activity:
    #' direct or indirect successors.
    #' @param id Activity id to be listed.
    #' @param ign_to A relation to be ignored: id -> ign_to.
    #' Activities from this relation will be ignored.
    #' @return A vector whith all activities ids.
    all_successors = function(id, ign_to=NULL) {
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
      base::rev(base::unique(base::rev(a_list[-1])))
    },

    #' @description List all predecessors from an activity:
    #' direct or indirect predecessors.
    #' @param id Activity id to be listed.
    #' @param ign_from A relation to be ignored: ign_from -> id.
    #' Activities from this relation will be ignored.
    #' @return A vector with all activities ids.
    all_predecessors = function(id, ign_from=NULL) {
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
      rev(unique(rev(a_list[-1])))
    },

    #' @description Change activities duration and calculate critical path.
    #' This way is faster than creating a new schedule with new durations.
    #' @param new_durations A vector with new activities' duration.
    #' @return A Schedule object.
    change_durations = function(new_durations) {
      # TODO verificar se os tamanhos são os mesmos
      # TODO verificar se não tem nenhum NULL ou NA
      private$.activities$duration <- new_durations
      private$calculate_critical_path()

      invisible(self)
    },

    #' @description Create a matrix that represents a Gantt chart.
    #' @return A matrix where "1" indicate that an activity is in execution.
    #' In this matrix, the rows represents activities,
    #' Whereas the columns represents the activity execution period.
    gantt_matrix = function() {
      if(self$duration == 0) {
        stop("There is no Gantt Matrix for a schedule with zero duration!")
      }
      atvs <- private$.activities
      duration <- self$duration
      qtdatvs <- self$nr_activities
      gantt <- base::matrix(base::rep(0, duration * qtdatvs), nrow=qtdatvs)
      for(i in 1:qtdatvs) {
        inicio <- atvs$ES[i] + 1
        termino <- atvs$EF[i]
        gantt[i, inicio:termino] <- 1
      }
      class(gantt) <- base::unique(c("Gantt", class(gantt)))

      row.names(gantt) <- atvs$id
      colnames(gantt) <- 1:duration

      gantt
    },

    #' @description Transform a Gantt matrix in x, y coordinates.
    #' Each point greater than zero in a Gantt matrix becomes a x, y coordinate.
    #' @param gantt A Gantt Matrix.
    #' @return A matrix x, y and weight.
    xy_gantt_matrix = function(gantt=NULL) {
      if(base::is.null(gantt)) {
        gantt <- self$gantt_matrix()
      } else {
        private$assert_is_gantt(gantt)
      }
      qtdatvs <- base::nrow(gantt)
      pdur <- base::ncol(gantt)
      v <- as.numeric(t(gantt))
      ii <- which(v > 0) - 1
      y <- base::floor(ii / pdur)
      x <- ii - y * pdur
      peso <- v[ii + 1]
      base::matrix(c(x, y + 1, peso), ncol=3)
    },


    #' @description Verify if a relation between two activities is redundant.
    #' A relation A->C is redundant if there are A->C, A->B, B->C relations.
    #' @param id_from Activity id from.
    #' @param id_to Activity id to.
    #' @return A logical TRUE if an arc is redundant; FALSE if not.
    is_redundant = function(id_from, id_to) {
      private$assert_activity_id_exist(id_from)
      private$assert_activity_id_exist(id_to)
      private$assert_relation_exist(id_from, id_to)

      succ <- self$all_successors(id_from, id_to)
      id_to %in% succ
    },

    #' @description **SP Serial or Parallel Topological Indicator:**
    #' It shows the closeness of a network to a serial or parallel graph.
    #' As the network becomes serial, the SP increase, until one,
    #' when the network totally serial.
    #' @return A number between 0 and 1, inclusive.
    topoi_sp = function() {
      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0) {
        return(NA)
      }

      if(nr_act == 1) {
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
    topoi_ad = function() {
      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0) {
        return(NA)
      }

      if(max_level == 1 || max_level == nr_act) {
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
    #' activities are as far as possible.
    #' Otherwise, if LA is equal one, the relation distance are one.
    #' @return A number between 0 and 1, inclusive.
    topoi_la = function() {
      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0) {
        return(NA)
      }

      # Activities quantities by level: they are called width, width by level
      wi <- as.numeric(base::table(private$.activities$progr_level))

      D <- base::sum(wi[-base::length(wi)] * wi[-1])

      if(D == nr_act - wi[1]) {
        return(1)
      }

      len <- 1
      levels_from <- private$.activities$progr_level[private$.relations$i_from]
      levels_to <- private$.activities$progr_level[private$.relations$i_to]
      arcs_qty <- base::sum(levels_to - levels_from == len)

      (arcs_qty - nr_act + wi[1]) / (D - nr_act + wi[1])
    },

    #' @description **TF Topological Float Indicator:**
    #' Measures the topological float of each activity.
    #' If TF = 0, there is no float between activities.
    #' If TF = 1, there is float between activities
    #' and they be shift without affecting other activities.
    #' @return A number between 0 and 1, inclusive.
    topoi_tf = function() {
      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0) {
        return(NA)
      }

      if(max_level == 1 || max_level == nr_act) {
        return(0);
      }

      level_diff <- private$.activities$regr_level - private$.activities$progr_level

      base::sum(level_diff) / ((max_level - 1) * (nr_act - max_level))
    }

  ),

  lock_class = TRUE

)
