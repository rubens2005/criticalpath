# References
#
# Csardi, G. & Nepusz, T. (2005).
# The Igraph Software Package for Complex Network Research.
# *InterJournal*. Complex Systems. 1695.
# [Article](https://www.researchgate.net/publication/221995787_The_Igraph_Software_Package_for_Complex_Network_Research)
#  / [igraph](https://igraph.org/)
#
# Vanhoucke, M. (2009) *Measuring Time*:
# Improving Project Performance Using Earned Value Management.
# Springer-Verlag US.
# doi: [10.1007/978-1-4419-1014-1](https://doi.org/10.1007/978-1-4419-1014-1).
#
# Vanhoucke, M. (2013) *Project Management with Dynamic Scheduling*:
# Baseline Scheduling, Risk Analysis and Project Control.
# Springer-Verlag Berlin Heidelberg.
# doi: [10.1007/978-3-642-40438-2](https://doi.org/10.1007/978-3-642-40438-2)
#
# Vanhoucke, M. (2014) *Integrated Project Management and Control*:
# First Comes the Theory, then the Practice.
# Springer International Publishing Switzerland.
# doi: [10.1007/978-3-319-04331-9](https://doi.org/10.1007/978-3-319-04331-9)
#

calculate_critical_path <- function(schedule) {
  assert_is_schedule(schedule)

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
  schedule$info$duration <- base::max(acts$EF) - base::min(acts$ES)

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
