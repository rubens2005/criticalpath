calculate_critical_path <- function(schedule) {

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

  acts <- schedule$activities
  rela <- schedule$relations

  # Define milestone
  acts$milestone <- acts$duration == 0

  # Init ealy, late, start and finish values
  acts$ES <- -Inf
  acts$EF <- -Inf
  acts$LS <- Inf
  acts$LF <- Inf

  # arrumarPeriodoDasAtividadesIniciais
  acts$ES[schedule$config$starters] <- 0
  acts$EF[schedule$config$starters] <- acts$duration[schedule$config$starters]

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
  #TODO Put here the algorithm to calculate schedule duration!
  schedule$info$duration <- max(acts$EF)

  # arrumarPeriodoDasAtividadesFinais
  acts$LF[schedule$config$ends] <- max(acts$EF)
  acts$LS[schedule$config$ends] <- acts$LF[schedule$config$ends] - acts$duration[schedule$config$ends]

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
  acts$free_float[schedule$config$ends] <- 0
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
  schedule$activities <- acts
  schedule$relations <- rela

  schedule
}
