#' R6 Class Representing a Schedule
#'
#' A schedule has activities and relations data-frames.
#'
#' @details
#' With a Schedule, you can calculate the project critical path.
#'
#' @export
#'
Schedule <- R6::R6Class("Schedule",

  private = list(

    activities = NULL,

    relations = NULL,

    info = NULL,

    config = NULL,

    activity_id_exist = function(id) {
      !is.na(base::match(id, private$activities$id))
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

    assert_relation_exist = function(from_id, to_id) {
      from_exist <- private$relations$from == from_id
      to_exist <- private$relations$to == to_id
      u <- which(from_exist & to_exist)
      if(length(u) == 0){
        stop(paste("Relation", from_id, "->", to_id, " must exist!"))
      }
    },

    assert_relation_does_not_exist = function(from_id, to_id) {
      from_exist <- private$relations$from == from_id
      to_exist <- private$relations$to == to_id
      u <- which(from_exist & to_exist)
      if(length(u) > 0){
        stop(paste("Relations", from_id, "->", to_id, " must NOT exist!"))
      }
    },


    topological_organization = function() {
      if(self$has_any_relation) {
        private$relations <- private$relations[order(private$relations$ord), ]
      }

      # 2) Find starters activities
      ids <- setdiff(private$activities$id, private$relations$to)
      private$config$starters <- match(ids, private$activities$id)

      # 3) Find ends activities
      ids <- setdiff(private$activities$id, private$relations$from)
      private$config$ends <- match(ids, private$activities$id)

      # 4) Topological sorting
      if(self$nr_relations > 0) {

        private$relations$i_from <- match(private$relations$from, private$activities$id)
        private$relations$i_to <- match(private$relations$to, private$activities$id)

        g <- igraph::graph_from_data_frame(
          private$relations,
          TRUE,
          private$activities
        )
        ts <- as.numeric(igraph::topo_sort(g, "out"))

        topo_order <- order(
          match(private$relations$from, ts),
          match(private$relations$to, ts)
        )
        private$relations <- private$relations[topo_order,]
        rownames(private$relations) <- 1:nrow(private$relations)
      }

      # 5) Calculate topological levels

      # Progressive Level
      private$info$max_level <- 1
      private$activities$progr_level <- private$info$max_level
      # Forward calculate
      if(self$has_any_relation) {
        for(i in 1:self$nr_relations) {
          from_id <- private$relations$i_from[i]
          to_id <- private$relations$i_to[i]
          next_level <- private$activities$progr_level[from_id] + 1
          if(next_level > private$activities$progr_level[to_id]) {
            private$activities$progr_level[to_id] <- next_level
            if(next_level > private$info$max_level) {
              private$info$max_level <- next_level
            }
          }
        }
      }

      # Backward calculate
      private$activities$regr_level <- private$info$max_level
      if(self$has_any_relation) {
        for(i in self$nr_relations:1) {
          from_id <- private$relations$i_from[i]
          to_id <- private$relations$i_to[i]
          prev_level <- private$activities$regr_level[to_id] - 1
          if(prev_level < private$activities$regr_level[from_id]) {
            private$activities$regr_level[from_id] <- prev_level
          }
        }
      }

      # Topological Float is the difference between regressive and progressive level
      private$activities$topo_float <- private$activities$regr_level - private$activities$progr_level
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

          SS = function(acts, pr, su, lag) {
            proximo =  acts$LS[su] - lag
            if (proximo < acts$LF[pr]) {
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
          },

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
          }

        )

      )

      ##############################

      acts <- private$activities
      rela <- private$relations

      # Define milestone
      acts$milestone <- acts$duration == 0

      # Init ealy, late, start and finish values
      acts$ES <- -Inf
      acts$EF <- -Inf
      acts$LS <- Inf
      acts$LF <- Inf

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

      # arrumarPeriodoDasAtividadesFinais
      acts$LF[private$config$ends] <- max(acts$EF)
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

      # Calculate free_flot
      acts$free_float <- +Inf
      acts$free_float[private$config$ends] <- 0
      if(nrow(acts) > 0) {
        for(from_id in 1:nrow(acts)) {
          succesors <- rela[rela$from == from_id, ]
          if(nrow(succesors) > 0) {
            for(j in 1:nrow(succesors)) {
              to_id <- succesors$i_to[j]
              ff <- acts$ES[to_id] - acts$EF[from_id]
              if(ff < acts$free_float[from_id]) {
                acts$free_float[from_id] <- ff
              }
            }
          }
        }
      }


      # Identify critical activity
      acts$critical <- acts$total_float <= 0

      # Identify critical relation
      rela$critical <- acts$critical[rela$i_from] & acts$critical[rela$i_to]


      ###################
      private$activities <- acts
      private$relations <- rela
    }



  ),

  active = list(

    nr_activities = function(value) {
      if(missing(value)) {
        return(private$info$nr_activities)
      }
      stop("Can't set `$nr_activities`", call. = FALSE)
    },

    has_any_activity = function(value) {
      if(missing(value)) {
        return(self$nr_activities > 0)
      }
      stop("Can't set `$has_any_activity`", call. = FALSE)
    },

    nr_relations = function(value) {
      if(missing(value)) {
        return(private$info$nr_relations)
      }
      stop("Can't set `$nr_relations`", call. = FALSE)
    },

    has_any_relation = function(value) {
      if(missing(value)) {
        return(private$info$has_any_relation)
      }
      stop("Can't set `$has_any_relation`", call. = FALSE)
    },

    duration = function(value) {
      if(base::missing(value)) {
        return(private$info$duration)
      }
      stop("Can't set `$duration`", call. = FALSE)
    }

  ),

  public = list(

#' New Schedule
#'
#' Create a new empty schedule.
#'
#' @param title title ...
#' @param reference reference ...
#' @return An Schedule object.
    initialize = function(title="", reference="") {
      private$activities <- data.frame(
        id = integer(),
        name = character(),
        duration = numeric(),
        milestone = logical(),
        critical = logical(),
        ES = numeric(),
        EF = numeric(),
        LS = numeric(),
        LF = numeric(),
        total_float = numeric(),
        free_float = numeric()
      )

      private$relations <- data.frame(
        from = numeric(),
        to   = numeric(),
        type = character(),
        lag = numeric(),
        critical = logical(),
        ord = numeric(),
        i_from = numeric(),
        i_to = numeric()
      )

      private$info <- list(
        title = title,
        reference = reference,

        nr_activities = 0,
        has_any_activity = FALSE,

        nr_relations = 0,
        has_any_relation = FALSE,

        duration = 0
      )

      private$config <- list()

    },

    #' Add an activity to a schedule
    #'
    #' Add an activity to a schedule that must exist.
    #'
    #' @usage
    #' add_activity(schedule, id, name="", duration=0)
    #' @param id
    #' Activity id. The id will be used to make relation between activities.
    #' @param name
    #' The name of activity. The default is a empty string.
    #' @param duration
    #' A number that represents the activity's duration.
    #' It must be equal or grater than zero. The default value is zero.
    #' @return A Schedule object with an activity added and critical path calculated.
    #'
    add_activity = function(id, name="", duration=0) {
      assert_activity_id_is_valid(id)
      private$assert_activity_id_does_not_exist(id)

      old_activities <- private$activities


      new_activity <- data.frame(
        id        = id,
        name      = ifelse(is.null(name), base::paste0("act", id), name),
        duration  = ifelse(is.null(duration), 0, duration),
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

      private$activities <- rbind(old_activities, new_activity)
      private$info$nr_activities <- nrow(private$activities)
      private$info$has_any_activity <- TRUE

      ## Topological organization
      private$topological_organization()

      ## Critical Path
      private$calculate_critical_path()

    },

    add_relation = function(from_id, to_id, type="FS", lag=0) {
      assert_activity_id_is_valid(from_id)
      private$assert_activity_id_exist(from_id)

      assert_activity_id_is_valid(to_id)
      private$assert_activity_id_exist(to_id)

      if(is.na(base::match(type, c("FS", "FF", "SS", "SF" )))) {
        stop("type must be FS, FF, SS or SF!")
      }

      old_relations <- private$relations

      new_relation = data.frame(
        from = from_id,
        to = to_id,
        type = type,
        lag = lag,
        critical = FALSE,
        ord = base::nrow(old_relations) + 1,
        i_from = NA,
        i_to = NA
      )

      private$relations <- base::rbind(old_relations, new_relation)

      private$info$nr_relations <- base::nrow(private$relations)
      private$info$has_any_relation <- TRUE

      ## Topological organization
      private$topological_organization()

      ## Critical Path
      private$calculate_critical_path()
    },

    add_act_rel = function(id, name, duration, relations_id=c(), dir="succ") {
      self$add_activity(id, name, duration)

      n <- length(relations_id)
      if(n > 0) {
        if(any(duplicated(relations_id))) {
          stop("Must NOT EXISTS duplicated id in relations_id!")
        }

        #1 Add temp relations
        if(is.null(private$config$temp_relations)) {
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
        if(dir == "succ") {
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
        } else if(dir == "pred") {
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
        } else {
          message(base::paste("Invalid '", dir, "' direction!"))
        }
      }

      #2 Add temp_relations to relations if activity id exist

      temp_n <- ifelse(
        is.null(private$config$temp_relations),
        0,
        nrow(private$config$temp_relations)
      )
      if(temp_n > 0) {
        private$config$temp_relations$keep <- TRUE
        for(i in 1:temp_n) {
          from_id <- private$config$temp_relations$from[i]
          to_id <- private$config$temp_relations$to[i]
          type <- private$config$temp_relations$type[i]
          lag <- private$config$temp_relations$lag[i]
          j <- base::intersect(private$activities$id, from_id)
          k <- base::intersect(private$activities$id, to_id)
          if(length(j) > 0 && length(k) > 0) {
            self$add_relation(from_id, to_id, type, lag)
            private$config$temp_relations$keep[i] <- FALSE
          }
        }

        #2 Remove temp_relations add
        if(sum(private$config$temp_relations$keep) > 0) {
          private$config$temp_relations <- private$config$temp_relations[private$config$temp_relations$keep, ]
          private$config$temp_relations$keep <- NULL
        } else {
          private$config$temp_relations <- NULL
        }
      }
    },

    print = function(...) {
      cat("Schedule: \n")
      cat("      Title: ", private$info$title, "\n", sep = "")
      cat("  Reference: ", private$info$reference, "\n", sep = "")
    },

    activities_as_data_frame = function() {
      private$activities[order(private$activities$id), ]
    },

    all_successors = function(id, ign_to=NULL) {
      private$assert_activity_id_exist(id)
      if(!is.null(ign_to)) {
        private$assert_activity_id_exist(ign_to)
        private$assert_relation_exist(id, ign_to)
      }

      temp_relation <- private$relations
      if(!is.null(ign_to)) {
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

    all_predecessors = function(id, ign_from=NULL) {
      private$assert_activity_id_exist(id)
      if(!base::is.null(ign_from)) {
        private$assert_activity_id_exist(ign_from)
        private$assert_relation_exist(ign_from, id)
      }

      temp_relation <- private$relations
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

    change_durations = function(new_durations) {
      # verificar se os tamanhos são os mesmos
      # verificar se não tem nenhum NULL ou NA
      private$activities$duration <- new_durations
      private$calculate_critical_path()
    },

    get_activity = function(id) {
      private$assert_activity_id_exist(id)
      private$activities[match(id, private$activities$id), ]
    },

    gantt_matrix = function() {
      if(self$duration == 0) {
        stop("There is no Gantt Matrix for a schedule with zero duration!")
      }

      atvs <- private$activities
      duration <- self$duration
      qtdatvs <- self$nr_activities
      gantt <- base::matrix(base::rep(0, duration * qtdatvs), nrow=qtdatvs)
      for(i in 1:qtdatvs) {
        inicio <- atvs$ES[i] + 1
        termino <- atvs$EF[i]
        gantt[i, inicio:termino] <- 1
      }

      class(gantt) <- base::unique(c("Gantt", class(gantt)))
      gantt
    },

    xy_gantt_matrix = function(gantt=NULL) {
      if(base::is.null(gantt)) {
        gantt <- self$gantt_matrix()
      } else {
        assert_is_gantt(gantt)
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

    is_redundant = function(id_from, id_to) {
      private$assert_activity_id_exist(id_from)
      private$assert_activity_id_exist(id_to)
      private$assert_relation_exist(id_from, id_to)

      succ <- self$all_successors(id_from, id_to)
      id_to %in% succ
    },

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
      wi <- table(private$activities$progr_level)
      # w mean
      wbar <- nr_act / max_level
      absolute_mean_deviation <- base::sum(base::abs(wi - wbar))

      absolute_mean_deviation / (2 * (max_level - 1) * (wbar - 1));
    },

    topoi_la = function() {
      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0) {
        return(NA)
      }

      # Activities quantities by level: they are called width, width by level
      wi <- as.numeric(base::table(private$activities$progr_level))

      D <- base::sum(wi[-base::length(wi)] * wi[-1])

      if(D == nr_act - wi[1]) {
        return(1)
      }

      len <- 1
      levels_from <- private$activities$progr_level[private$relations$from]
      levels_to <- private$activities$progr_level[private$relations$to]
      arcs_qty <- base::sum(levels_to - levels_from == len)

      (arcs_qty - nr_act + wi[1]) / (D - nr_act + wi[1])
    },

    topoi_tf = function() {
      max_level <- private$info$max_level
      nr_act <- self$nr_activities

      if(nr_act == 0) {
        return(NA)
      }

      if(max_level == 1 || max_level == nr_act) {
        return(0);
      }

      level_diff <- private$activities$regr_level - private$activities$progr_level

      base::sum(level_diff) / ((max_level - 1) * (nr_act - max_level))
    }

  ),

  lock_class = TRUE

)


