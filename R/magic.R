#' Start Game
#'
#' This function can start to paly game on R
#'
#' @param players.id.play Player's id
#' @param players.n Number of players
#' @return plot
#' @export
#' @examples
#' start_game(players.id.play = 1, players.n = 5)
start_game <- function(players.id.play = 1, players.n = 5 ){
#Function======================================================================
    setdiff.f <- function(x=c(2,2), y=c(1,1,2,2,8)){

      diff.table <- table(factor(y, 1:8)) -  table(factor(x, 1:8))
      return(rep(1:8, diff.table))

    }

    check.life.f <- function(players.list, now){
      temp<-c("", "", "")

      if(any(unlist(lapply( players.list, function(x) length(x$players.magic) <= 0)))){

        players.list[[now]]$players.grade <- players.list[[now]]$players.grade + 3

        if(any(unlist(lapply( players.list, function(x) length(x$players.owl) > 0)))){
          for(x in (1:players.n)[unlist(lapply( players.list, function(x) length(x$players.owl) > 0))]){
            players.list[[x]]$players.grade <- players.list[[x]]$players.grade + length(players.list[[x]]$players.owl)
            temp[3] <- paste(temp[3], x)
          }
        }

        cat(paste0("---------------------------------------\nPlayer ", players.list[[now]]$players.id, " get 5 point, Player ",temp[3]  ," get 1 point for owl\n---------------------------------------"))
        players.list <<- players.list
        return(FALSE)
      }

      if(any(unlist(lapply( players.list,function(x)x$players.life <= 0)))){

        for(x in (1:players.n)[unlist(lapply( players.list, function(x) x$players.life > 0))]){
          if(x != now){
            players.list[[x]]$players.grade <- players.list[[x]]$players.grade + 1
            temp[1] <- paste(temp[1], x)
          }else{
            players.list[[x]]$players.grade <- players.list[[x]]$players.grade + 3
            temp[2] <- x
          }
        }
        if(any(unlist(lapply( players.list, function(x) length(x$players.owl) > 0 & x$players.life > 0)))){
          for(x in (1:players.n)[unlist(lapply( players.list, function(x) length(x$players.owl) > 0))]){
            if(players.list[[x]]$players.life > 0){
            players.list[[x]]$players.grade <- players.list[[x]]$players.grade + length(players.list[[x]]$players.owl)
            temp[3] <- paste(temp[3], x)}
          }
        }

        cat(paste0("---------------------------------------\nPlayer ", temp[2], " get 3 point, Player",temp[1]  ," get 1 point, Player",temp[3]  ," get 1 point for owl\n---------------------------------------"))
        players.list <<- players.list
        return(FALSE)
      }

      return(TRUE)
    }

    check.grade.f <- function(players.list){

      if(any(unlist(lapply( players.list,function(x) x$players.grade >= 8)))){

        temp <- paste((1:players.n)[unlist(lapply( players.list,function(x) x$players.grade >= 8))], collapse=" ")
        message(paste0("\nooooooooooooooooooooooooooooooooooooooo\n The Winner is Player ", temp, " \nooooooooooooooooooooooooooooooooooooooo"))
        return(FALSE)
      }

      return(TRUE)
    }

    readinteger <- function(past.guess){
      n <- readline(prompt="Enter your magic num: ")
      if(n %in%1:9){
        if (past.guess !=9){
          if( n <= past.guess | n==9){
            return(as.integer(n))
          }else{
            return(readinteger(past.guess))
          }
        }else{
          if( n < 9){
            return(as.integer(n))
          }else{
            return(readinteger(past.guess))
          }
        }
      }
      return(readinteger(past.guess))

    }

    #plot situation
    draw.situation <- function(players.list){
      op <- par(mar = rep(2, 4))
      layout(matrix(c(1,1,2,1,1,3),3,2))
      #par(mar=c(0,0,0,0))
        #par.old <- par(mfrow = c(2,1))
          plot(0,0,xlim = c(1, 13), ylim =c(-1, 6), axes =F, xlab= "", ylab= "", main = "Play Stadium")
          for(j in 1:players.n){
            players <- players.list[[j]]
              for(i in 1: length(players$players.magic)){
                if(length(players$players.magic) > 0 ){
                  if(players$players.id != players.id.play){

                    rasterImage(pic.v[[players$players.magic[i]]], i, players$players.id, i+1, players$players.id+1)

                    a <- 0.008
                    rect(i+a, players$players.id+a, i+1-a, players$players.id+1-a, col = "transparent", border = players$players.magic[i])

                    text(i+.5, players$players.id+.2, text.v[players$players.magic[i]], cex = .7, col ="orange")
                  }else{
                    a <- 0.008
                    rect(i+a, players$players.id+a, i+1-a, players$players.id+1-a, col = "transparent", border = 1)



                  }
                }
              }


              for(i in 1: length(players$players.owl)){
                if(length(players$players.owl) > 0 ){
                  if(players$players.id == players.id.play){

                    rasterImage(pic.v[[players$players.owl[i]]], 8+i, players$players.id, 8+i+1, players$players.id+1)

                    a <- 0.008
                    rect(8+i+a, players$players.id+a, 8+i+1-a, players$players.id+1-a, col = "transparent", border = players$players.owl[i])

                    text(8+i+.5, players$players.id+.2, text.v[players$players.owl[i]], cex = .7, col ="orange")
                  }else{

                    a <- 0.008
                    rect(8+i+a, players$players.id+a, 8+i+1-a, players$players.id+1-a, col = "transparent", border = 1)
                  }

                }
              }

              points(6.5, players$players.id+.5 , pch = players$players.id, col = players$players.id)
              text(7.5, players$players.id+.5, paste("Life = ",players$players.life))

              for(i in 1: length(pool.magic )){
                if(length(pool.magic) > 0 ){

                a <- 0.008
                rect(i+a, -1+a, i+1-a, -a, col = "transparent", border = 1)
                }
              }

              for(i in 1: length(owl.magic )){
                if(length(owl.magic) > 0 ){

                a <- 0.008
                rect(8+i+a, -1+a, 8+i+1-a, -a, col = "transparent", border = 1)
                }
              }



          }
          barpos <- barplot(table(factor( use.magic, 1:8)), col = 1:8, horiz =T, xlim = c(-1,9), names.arg = NULL, las = 2, main = "Magic Stat")
          for(i in 1:8){
            rasterImage(pic.v[[i]], -1, barpos[i]-0.5, 0, barpos[i]+.5)
            #text( 0-.5, barpos[i], text.v[i], cex = .7, col ="orange")
          }

          barpos <-barplot(unlist(lapply( players.list, function(x) x$players.grade)), ylim = c(0,9), col = 1:players.n, main = "Grade Stat")
          for(i in 1:players.n){
          points( barpos[i],  players.list[[i]]$players.grade +1, pch = i, col = i)}
          abline(h = 8 ,lty = 2, col="orange")
         par(op)

    }


 #==========================================================


    pic.v <- NULL
    for(i in 1:8){
    #  pic.v <- c(pic.v,  magick::image_resize(magick::image_read(paste0('C:/Users/Kent_Tung/Desktop/magic pic/',i,'.jpg')), "x100"))
       pic.v <- c(pic.v, magick::image_resize(magick::image_read( system.file("extdata", paste0(i, ".jpg"), package = "ABRACAWHAT"), "x100")))
    }
    text.v <-c("Dragon","Phantom","Rainbow","Owl","Thunder","Blizzard","Fire","Medicine")


    loop <- 1


        pool.magic <- rep(1:8,1:8)
        owl.magic <- sample(pool.magic, 4, replace = F)
        pool.magic <- setdiff.f(owl.magic, pool.magic)

        players.magic.v <- sample(pool.magic, players.n * 5, replace = F)
        players.magic.m <- matrix(players.magic.v , ncol = 5)

        pool.magic  <- setdiff.f(players.magic.v , pool.magic)

        use.magic <- NULL

        players.list <- vector("list", length = length(players.n))
        for(i in 1:players.n){
          players.list[[i]] <- list(players.id = i,
                                    players.life = 6,
                                    players.magic = players.magic.m[i,],
                                    players.grade = 0,
                                    players.owl = NULL,
                                    players.guess = 9)

        }
        draw.situation(players.list)
        next.one <- 0
        id <- 1
        while(check.grade.f(players.list)){
        if(loop != 1 ){
            pool.magic <- rep(1:8,1:8)
            owl.magic <- sample(pool.magic, 4, replace = F)
            pool.magic <- setdiff.f(owl.magic, pool.magic)

            players.magic.v <- sample(pool.magic, players.n * 5, replace = F)
            players.magic.m <- matrix(players.magic.v , ncol = 5)


            pool.magic  <- setdiff.f(players.magic.v , pool.magic)

            use.magic <- NULL

            #players.list <- vector("list", length = length(players.n))
            for(i in 1:players.n){
              players.list[[i]]$players.life = 6
              players.list[[i]]$players.magic = players.magic.m[i,]
              players.list[[i]]$players.owl = NULL
              players.list[[i]]$players.guess = 9

            }
            draw.situation(players.list)
        }
        loop <- loop + 1

            while( check.life.f(players.list, id) ){

              id <- next.one %% players.n + 1

              know.magic <- unlist(sapply( setdiff(1:players.n, id), function(x) players.list[[x]]$players.magic))
              unknown.magic <- setdiff.f( c(use.magic,  know.magic, players.list[[id]]$players.owl), rep(1:8,1:8))
              prob.magic <- table(factor(unknown.magic, 1:8))/length(unknown.magic)
              guess.temp <- (1:8)[prob.magic == max(prob.magic)][ sample(1:length((1:8)[prob.magic==max(prob.magic)]),1)]

              if( players.list[[id]]$players.life == 6){
                prob.magic <- table(factor(unknown.magic, c(1, 2, 4, 5, 6, 7))) / length(unknown.magic[ !unknown.magic%in%c(3,8)])
                guess.temp <-  as.numeric(names(which.max(rev(prob.magic))))

              }
              if(all( prob.magic < 0.2) & tail( players.list[[id]]$players.guess, 1) != 9){
                guess.temp <- 9
              }
              if( guess.temp > tail( players.list[[id]]$players.guess, 1)){
                guess.temp <- 9
              }

              if(id != players.id.play){
                guess1 <- guess.temp
              }else{
                guess1 <- readinteger(tail(players.list[[id]]$players.guess, 1))
              }

                if (guess1 %in% players.list[[id]]$players.magic | guess1 == 9){
                  print(paste("Player",id, c(text.v, "pass")[guess1], "sucess"))
                  switch(guess1,{
                         #1
                         fire <- sample((-1:-6), 1)
                         for(x in setdiff(1:players.n, id)){
                           players.list[[x]]$players.life <- players.list[[x]]$players.life + fire
                         }}, {
                         #2
                         fire <- -1
                         for(x in setdiff(1:players.n, id)){
                           players.list[[x]]$players.life <- players.list[[x]]$players.life + fire
                         }}, {
                         #3
                         fire <- sample(1:6, 1)
                         players.list[[id]]$players.life <- players.list[[id]]$players.life + fire
                         if(players.list[[id]]$players.life > 6){
                           players.list[[id]]$players.life <- 6
                         }
                         }, {
                         #4
                          if(length(owl.magic) > 0){
                            owl <- sample(owl.magic, 1, replace = F)
                            players.list[[id]]$players.owl <- c(players.list[[id]]$players.owl, owl)
                            owl.magic <- setdiff.f(owl, owl.magic)
                          }
                         }, {
                         #5
                         fire <- -1
                         if(id == 1){
                           players.list[[id+1]]$players.life <- players.list[[id+1]]$players.life + fire
                           players.list[[players.n]]$players.life <- players.list[[players.n]]$players.life + fire
                         }else{ if(id == players.n){
                           players.list[[1]]$players.life <- players.list[[1]]$players.life + fire
                           players.list[[id-1]]$players.life <- players.list[[id-1]]$players.life + fire
                                }else{
                           players.list[[id+1]]$players.life <- players.list[[id+1]]$players.life + fire
                           players.list[[id-1]]$players.life <- players.list[[id-1]]$players.life + fire
                         }}}, {
                         #6
                         fire <- -1
                         if(id == 1){
                           players.list[[players.n]]$players.life <- players.list[[players.n]]$players.life + fire
                         }else{
                           players.list[[id-1]]$players.life <- players.list[[id-1]]$players.life + fire
                         }}, {
                         #7
                         fire <- -1
                         if(id == players.n){
                           players.list[[1]]$players.life <- players.list[[1]]$players.life + fire
                         }else{
                           players.list[[id+1]]$players.life <- players.list[[id+1]]$players.life + fire
                         }}, {
                         #8
                         fire <- 1
                         players.list[[id]]$players.life <- players.list[[id]]$players.life + fire
                         if(players.list[[id]]$players.life > 6){
                           players.list[[id]]$players.life <- 6
                         }
                         },{
                           next.one <- next.one + 1
                         })

                  players.list[[id]]$players.magic <- setdiff.f(guess1, players.list[[id]]$players.magic)
                  players.list[[id]]$players.guess <- c(players.list[[id]]$players.guess, guess1)
                  use.magic <- c(use.magic, guess1)
                }else{
                  print(paste("Player",id, c(text.v, "pass")[guess1], "fail"))
                  if(guess1 == 1){
                    fire <- sample((-1:-6), 1)
                  }else{
                    fire <- -1
                  }
                  players.list[[id]]$players.life <- players.list[[id]]$players.life + fire
                  players.list[[id]]$players.guess <- c(players.list[[id]]$players.guess, 9)
                  next.one <- next.one + 1

                }
              if(length(players.list[[id]]$players.magic) < 5 & length(pool.magic) >0 & tail( players.list[[id]]$players.guess,1) == 9){
                if(length(pool.magic) == 1){
                  complement.magic <- pool.magic
                }else{
                  complement.magic <- sample(pool.magic, min(5-length(players.list[[id]]$players.magic), length(pool.magic) ), replace = F)

                }
                  pool.magic <- setdiff.f( complement.magic, pool.magic )
                  players.list[[id]]$players.magic <- c(players.list[[id]]$players.magic,  complement.magic )

              }

              draw.situation(players.list)
              Sys.sleep(1)

            }
        draw.situation(players.list)
        Sys.sleep(1)
        readline(prompt="Press any key to continue new start:")

      }


}
















