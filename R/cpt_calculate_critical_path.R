cpt_calculate_critical_path <- function(sch) {
  if(!cpt_is_schedule_created(sch)) {
    stop("Error: Schedule withou topological sort!")
  }

  relation_type <- list(

    forward = list(

      FS = function(pr, su, lag) {
        proximo =  sch$activities$early_finish[pr] + lag
        if (proximo > sch$activities$early_start[su]) {
          return(list(
            ok = TRUE,
            early_start = proximo,
            early_finish = proximo + sch$activities$duration[su]
          ))
        }
        return(list(ok = FALSE))
      },

      FF = function(pr, su, lag) {
        proximo =  sch$activities$early_finish[pr] + lag
        if (proximo > sch$activities$early_finish[su]) {
          return(list(
            ok = TRUE,
            early_finish = proximo,
            early_start = proximo - sch$activities$duration[su]
          ))
        }
        return(list(ok = FALSE))
      },

      SS = function(pr, su, lag) {
        proximo =  sch$activities$early_start[pr] + lag
        if (proximo > sch$activities$early_start[su]) {
          return(list(
            ok = TRUE,
            early_start = proximo,
            early_finish = proximo + sch$activities$duration[su]
          ))
        }
        return(list(ok = FALSE))
      },

      SF = function(pr, su, lag) {
        proximo =  sch$activities$early_start[pr] + lag
        if (proximo > sch$activities$early_finish[su]) {
          return(list(
            ok = TRUE,
            early_finish = proximo,
            early_start = proximo - sch$activities$duration[su]
          ))
        }
        return(list(ok = FALSE))
      }

    ),

    backward = list(

      FS = function(pr, su, lag) {
        proximo =  sch$activities$late_start[su] - lag
        if (proximo < sch$activities$late_finish[pr]) {
          return(list(
            ok = TRUE,
            late_finish = proximo,
            late_start = proximo - sch$activities$duration[pr]
          ))
        }
        return(list(ok = FALSE))
      },

      FF = function(pr, su, lag) {
        proximo =  sch$activities$late_finish[su] - lag
        if (proximo < sch$activities$late_finish[pr]) {
          return(list(
            ok = TRUE,
            late_finish = proximo,
            late_start = proximo - sch$activities$duration[pr]
          ))
        }
        return(list(ok = FALSE))
      },

      SS = function(pr, su, lag) {
        proximo =  sch$activities$late_start[su] - lag
        if (proximo < sch$activities$late_start[pr]) {
          return(list(
            ok = TRUE,
            late_start = proximo,
            late_finish = proximo + sch$activities$duration[pr]
          ))
        }
        return(list(ok = FALSE))
      },

      SF = function(pr, su, lag) {
        proximo =  sch$activities$late_finish[su] - lag
        if (proximo < sch$activities$late_start[pr]) {
          return(list(
            ok = TRUE,
            late_start = proximo,
            late_finish = proximo + sch$activities$duration[pr]
          ))
        }
        return(list(ok = FALSE))
      }

    )

  )

  ##############################

  ##acts <- sch$activities
  rela <- sch$relations

  # Define milestone
  sch$activities$milestone <- sch$activities$duration == 0L

  # Init early start and finish values
  sch$activities$early_start <- -2000000000L
  sch$activities$early_finish <- -2000000000L

  # arrumarPeriodoDasAtividadesIniciais
  sch$activities$early_start[sch$config$starters] <- 0L
  sch$activities$early_finish[sch$config$starters] <- sch$activities$duration[sch$config$starters]

  # Forward calculate
  if(sch$info$has_any_relation) {
    for(i in 1:sch$info$nr_relations) {
      type <- rela$type[i]
      early <- relation_type$forward[[type]](
        rela$i_from[i],
        rela$i_to[i],
        rela$lag[i]
      )
      if(early$ok) {
        sch$activities$early_start[rela$i_to[i]] <- early$early_start
        sch$activities$early_finish[rela$i_to[i]] <- early$early_finish
      }
    }
  }

  # Calculate project duration
  sch$info$duration <- base::max(sch$activities$early_finish) - base::min(sch$activities$early_start)

  # Init late start and finish values
  sch$activities$late_finish <- +2000000000L
  sch$activities$late_start <- +2000000000L

  # arrumarPeriodoDasAtividadesFinais
  sch$activities$late_finish[sch$config$ends] <- base::max(sch$activities$early_finish)
  sch$activities$late_start[sch$config$ends] <- sch$activities$late_finish[sch$config$ends] - sch$activities$duration[sch$config$ends]

  # Backward calculate
  if(sch$info$has_any_relation) {
    for(i in sch$info$nr_relations:1) {
      type <- rela$type[i]
      late <- relation_type$backward[[type]](
        rela$i_from[i],
        rela$i_to[i],
        rela$lag[i]
      )
      if(late$ok) {
        sch$activities$late_start[rela$i_from[i]] <- late$late_start
        sch$activities$late_finish[rela$i_from[i]] <- late$late_finish
      }
    }
  }

  # Calculate total_float
  sch$activities$total_float <- sch$activities$late_start - sch$activities$early_start

  # Calculate free_float
  sch$activities$free_float <- sch$activities$total_float  ## Acho que aqui tem que ser a TOTAL_FLOAT !!!
  if(nrow(sch$activities) > 0L) {
    for(i_from in 1:nrow(sch$activities)) {
      succesors <- rela[rela$i_from == i_from, ]
      if(nrow(succesors) > 0L) {
        for(j in 1:nrow(succesors)) {
          to <- succesors$i_to[j]
          ff <- sch$activities$early_start[to] - sch$activities$early_finish[i_from]
          if(ff < sch$activities$free_float[i_from]) {
            sch$activities$free_float[i_from] <- ff
          }
        }
      } else {
        sch$activities$free_float[i_from] <- sch$activities$total_float[i_from]
      }
    }
  }

  # Identify critical activity
  sch$activities$critical <- sch$activities$total_float <= 0L

  # Identify critical relation
  rela$critical <- sch$activities$critical[rela$i_from] & sch$activities$critical[rela$i_to]


  ###################
  #####sch$activities <- acts
  sch$relations <- rela


  return(sch)
}
