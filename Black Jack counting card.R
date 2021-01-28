

# 牌數
desk <- function(num_of_desk=1){
  card_num <- rep(c(1,2,3,4,5,6,7,8,9,10,10,10,10), num_of_desk*4)
  card_num2 <- sample(card_num, length(card_num), replace = F)
  return(card_num2)
} 

# 起始兩張牌
select_card_start <- function(desk_card){
  hand_card <- desk_card[1:2]
  return(hand_card)
}

# 加牌
pop_card <- function(desk_card){
  hand_card <- desk_card[1]
  return(hand_card)
}

# 新牌組
new_desk <- function(desk_card){
  desk_card <- desk_card[-1]
  return(desk_card)
}

# 檢查是否加牌
check_pop <- function(my_hand, point, my_sum, desk_card){
  while (my_sum <= point) { 
    my_hand <- c(my_hand, pop_card(desk_card))
    desk_card <- new_desk(desk_card)
    Ato11 <- my_hand
    Ato11[Ato11==1] <- 11
    my_sum <- sum(Ato11)
  }
  return(list("x" = my_sum, "y" = desk_card, "z" = my_hand))
}

# 超過21點 將ACE轉1
check_A21 <- function(my_sum,my_hand){
  if (my_sum > 21 && 1 %in% my_hand) {
    my_sum <- sum(my_hand)
  }
  return(list("x" = my_sum, "y" = my_hand))
}

# 轉後檢查是否加牌
check_pop_A <- function(my_hand, point, my_sum, desk_card){
  while ( my_sum <= point) {
    my_hand <- c(my_hand, pop_card(desk_card))
    desk_card <- new_desk(desk_card)
    my_sum <- sum(my_hand)
  }
  return(list("x" = my_sum, "y" = desk_card, "z" = my_hand))
}

# 算牌

serial_count <- function(my_hand,ch_hand){
  for (i in 1:length(my_hand)){
    if(my_hand[i] == 2 | my_hand[i] == 3 | my_hand[i] == 4 | my_hand[i] == 5 | my_hand[i] == 6){
      serial_num = serial_num + 1 
      
    } else if(my_hand[i] == 10 | my_hand[i] == 1){
      serial_num = serial_num - 1 
    }
  }
  for(j in 1:length(ch_hand)){
    if(ch_hand[j] == 2 | ch_hand[j] == 3 | ch_hand[j] == 4 | ch_hand[j] == 5 | ch_hand[j] == 6){
      serial_num = serial_num + 1 
      
    } else if(ch_hand[j] == 10 | ch_hand[j] == 1){
      serial_num = serial_num - 1  
    }
  }
  return(serial_num)
}

# 初始牌
desk_card1 <- desk(2)  


serial_num = 0 ; winloss = NULL ; serial_all = NULL ; money = NULL ; jen2 = NULL

# 模擬 2人遊戲過程
game <- function(point1, desk_card){

  # 玩家手牌
  my_hand <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 莊家手牌
  ch_hand <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
 

  # 判斷 ACE
  Ato11 <- my_hand ; Ato112 <- ch_hand
  Ato11[Ato11==1] <- 11 ; Ato112[Ato112==1] <- 11
  my_sum <- sum(Ato11) ; ch_sum <- sum(Ato112)
  
  ## 玩家
  # 判斷玩家是否補牌
  check <- check_pop(my_hand, point1, my_sum, desk_card)
  my_sum <- check$x ; desk_card <- check$y ; my_hand <- check$z
  
  # 若點數合超過21且包含A, 將A變為1
  check2 <- check_A21(my_sum, my_hand)
  my_sum <- check2$x ; my_hand <- check2$y

  # A轉為1後再次判斷是否要牌
  check1 <- check_pop_A(my_hand, point1, my_sum, desk_card)
  my_sum <- check1$x ; desk_card <- check1$y ; my_hand <- check1$z
  
  # 超過21點爆牌
  
  my_sum <- ifelse(( my_sum <= 21), my_sum, 0 )
  
  ## 莊家
  # 判斷玩家是否補牌
  check10 <- check_pop(ch_hand, point=16, ch_sum, desk_card)
  ch_sum <- check10$x ; desk_card <- check10$y ; ch_hand <- check10$z
  
  # 若點數合超過21且包含A, 將A變為1
  check11 <- check_A21(ch_sum, ch_hand)
  ch_sum <- check11$x ; ch_hand <- check11$y
  
  # A轉為1後再次判斷是否要牌
  check12 <- check_pop_A(ch_hand, point=16, ch_sum, desk_card)
  ch_sum <- check12$x ; desk_card <- check12$y ; ch_hand <- check12$z
  
  # 超過21點爆牌
  
  ch_sum <- ifelse(( ch_sum <= 21), ch_sum, 0 )
  
  
  ## 勝敗
  result <- ifelse(my_sum> ch_sum, 1, ifelse(my_sum < ch_sum, -1, 0) )
  
  # 算牌
  serial_num <- serial_count(my_hand, ch_hand)
  
  desk_final = desk_card
  return(list("result" = result, "serial" = serial_num, "desk" = desk_final))

}

# 模擬一局
game_start <- function(point1,num_of_desk){
  
  serial_num = 0
  desk_card1 <- desk(num_of_desk)
  last_card = 52 * num_of_desk
  while(last_card >= 52 * num_of_desk/2){
    end <- game(point1, desk_card1)
    
    winloss <- c(winloss, end[[1]])
    serial_all <- c(serial_all, end[[2]])
    desk_card1 <- end$desk
    
    last_card <- length(end$desk)
    jen <- end[[2]]/(last_card/52)
    jen2 <- c(jen2,jen)
  }
  
  return(list("winloss" = winloss, "serial_all" = serial_all, "jen" = round(jen2,2)))
}


#########       討論各種情況       ######### 
################  2人 50次  ################

trygame <- function(point1, num_of_desk, money5){
  output = c()
  
  for (m in 1:50){
    final2 = c()
    serial_all2 = NULL
    
    for(j in 1:100){
      serial_num = 0
      winloss = NULL ; serial_all = NULL ; money = NULL ; jen2 = NULL
      
      a = game_start(point1,num_of_desk)
      a[1]
      a[2]
      money2 = (floor(a[[3]]+1)-1) 
      for (i in 1:length(money2)){
        if (money2[i] <= 2){
          money[i] = 1 * money5
        } else if (money2[i] == 3){
          money[i] = 2 * money5
        } else if (money2[i] == 4){
          money[i] = 3 * money5
        } else if (money2[i] == 5){
          money[i] = 4 * money5
        } else if (money2[i] >= 6){
          money[i] = 5 * money5
        }
      }
      final = sum(money*a[[1]])
      final2 = c(final2, final)
      serial_all2 = c(serial_all2, a[[2]])
    }
    output = c(output, sum(final2))
  }
  
  return(output)
  
}

##############################
# 牌數4 閥值13 $10 
a <- trygame(13,4,10)
c(mean(a),sd(a))

# 牌數5 閥值13 $10 
a <- trygame(13,5,10)
c(mean(a),sd(a))

# 牌數6 閥值13 $10 
a <- trygame(13,6,10)
c(mean(a),sd(a))

# 牌數7 閥值13 $10 
a <- trygame(13,7,10)
c(mean(a),sd(a))

# 牌數8 閥值13 $10 
a <- trygame(13,8,10)
c(mean(a),sd(a))

####

# 牌數4 閥值14 $10 
a <- trygame(14,4,10)
c(mean(a),sd(a))

# 牌數5 閥值14 $10 
a <- trygame(14,5,10)
c(mean(a),sd(a))

# 牌數6 閥值14 $10 
a <- trygame(14,6,10)
c(mean(a),sd(a))

# 牌數7 閥值14 $10 
a <- trygame(14,7,10)
c(mean(a),sd(a))

# 牌數8 閥值14 $10 
a <- trygame(14,8,10)
c(mean(a),sd(a))


####

# 牌數4 閥值15 $10 
a <- trygame(15,4,10)
c(mean(a),sd(a))

# 牌數5 閥值15 $10 
a <- trygame(15,5,10)
c(mean(a),sd(a))

# 牌數6 閥值15 $10 
a <- trygame(15,6,10)
c(mean(a),sd(a))

# 牌數7 閥值15 $10 
a <- trygame(15,7,10)
c(mean(a),sd(a))

# 牌數8 閥值15 $10 
a <- trygame(15,8,10)
c(mean(a),sd(a))


####

# 牌數4 閥值16 $10 
a <- trygame(16,4,10)
c(mean(a),sd(a))

# 牌數5 閥值16 $10 
a <- trygame(16,5,10)
c(mean(a),sd(a))

# 牌數6 閥值16 $10 
a <- trygame(16,6,10)
c(mean(a),sd(a))

# 牌數7 閥值16 $10 
a <- trygame(16,7,10)
c(mean(a),sd(a))

# 牌數8 閥值16 $10 
a <- trygame(16,8,10)
c(mean(a),sd(a))


###########################
####
# 牌數4 閥值13 $50 
a <- trygame(13,4,50)
c(mean(a),sd(a))

# 牌數5 閥值13 $50
a <- trygame(13,5,50)
c(mean(a),sd(a))

# 牌數6 閥值13 $50 
a <- trygame(13,6,50)
c(mean(a),sd(a))

# 牌數7 閥值13 $50 
a <- trygame(13,7,50)
c(mean(a),sd(a))

# 牌數8 閥值13 $50 
a <- trygame(13,8,50)
c(mean(a),sd(a))

####

# 牌數4 閥值14 $50
a <- trygame(14,4,50)
c(mean(a),sd(a))

# 牌數5 閥值14 $50
a <- trygame(14,5,50)
c(mean(a),sd(a))

# 牌數6 閥值14 $50 
a <- trygame(14,6,50)
c(mean(a),sd(a))

# 牌數7 閥值14 $50
a <- trygame(14,7,50)
c(mean(a),sd(a))

# 牌數8 閥值14 $50
a <- trygame(14,8,50)
c(mean(a),sd(a))


####

# 牌數4 閥值15 $50
a <- trygame(15,4,50)
c(mean(a),sd(a))

# 牌數5 閥值15 $50
a <- trygame(15,5,50)
c(mean(a),sd(a))

# 牌數6 閥值15 $50 
a <- trygame(15,6,50)
c(mean(a),sd(a))

# 牌數7 閥值15 $50
a <- trygame(15,7,50)
c(mean(a),sd(a))

# 牌數8 閥值15 $50
a <- trygame(15,8,50)
c(mean(a),sd(a))


####

# 牌數4 閥值16 $50
a <- trygame(16,4,50)
c(mean(a),sd(a))

# 牌數5 閥值16 $50
a <- trygame(16,5,50)
c(mean(a),sd(a))

# 牌數6 閥值16 $50
a <- trygame(16,6,50)
c(mean(a),sd(a))

# 牌數7 閥值16 $50
a <- trygame(16,7,50)
c(mean(a),sd(a))

# 牌數8 閥值16 $50
a <- trygame(16,8,50)
c(mean(a),sd(a))


################  4人 50次  ################
# 模擬 4人遊戲過程
game <- function(point1, desk_card){
  
  # 玩家手牌
  my_hand <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家2手牌
  my_hand2 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家3手牌
  my_hand3 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 莊家手牌
  ch_hand <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  
  
  # 判斷 ACE
  Ato11 <- my_hand ; Ato112 <- ch_hand ; Ato22 <- my_hand2 ; Ato33 <- my_hand3 
  Ato11[Ato11==1] <- 11 ; Ato112[Ato112==1] <- 11 ; Ato22[Ato22==1] <- 11 ; Ato33[Ato33==1] <- 11
  my_sum <- sum(Ato11) ; ch_sum <- sum(Ato112) ; my_sum2 <- sum(Ato22) ; my_sum3 <- sum(Ato33)
  
  ## 玩家
  # 判斷玩家是否補牌
  check <- check_pop(my_hand, point1, my_sum, desk_card)
  my_sum <- check$x ; desk_card <- check$y ; my_hand <- check$z
  
  # 若點數合超過21且包含A, 將A變為1
  check2 <- check_A21(my_sum, my_hand)
  my_sum <- check2$x ; my_hand <- check2$y
  
  # A轉為1後再次判斷是否要牌
  check1 <- check_pop_A(my_hand, point1, my_sum, desk_card)
  my_sum <- check1$x ; desk_card <- check1$y ; my_hand <- check1$z
  
  # 超過21點爆牌
  
  my_sum <- ifelse(( my_sum <= 21), my_sum, 0 )
  
  ## 玩家2
  # 判斷玩家是否補牌
  check22 <- check_pop(my_hand2, point1, my_sum, desk_card)
  my_sum22 <- check22$x ; desk_card <- check22$y ; my_hand22 <- check22$z
  
  # 若點數合超過21且包含A, 將A變為1
  check222 <- check_A21(my_sum22, my_hand22)
  my_sum22 <- check222$x ; my_hand22 <- check222$y
  
  # A轉為1後再次判斷是否要牌
  check122 <- check_pop_A(my_hand22, point1, my_sum22, desk_card)
  my_sum22 <- check122$x ; desk_card <- check1$y ; my_hand <- check122$z
  
  # 超過21點爆牌
  
  my_sum22 <- ifelse(( my_sum22 <= 21), my_sum22, 0 )
  
  ## 玩家3
  # 判斷玩家是否補牌
  check33 <- check_pop(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check33$x ; desk_card <- check33$y ; my_hand33 <- check33$z
  
  # 若點數合超過21且包含A, 將A變為1
  check233 <- check_A21(my_sum33, my_hand33)
  my_sum33 <- check233$x ; my_hand33 <- check233$y
  
  # A轉為1後再次判斷是否要牌
  check133 <- check_pop_A(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check133$x ; desk_card <- check133$y ; my_hand33 <- check133$z
  
  # 超過21點爆牌
  
  my_sum33 <- ifelse(( my_sum33 <= 21), my_sum33, 0 )
  
  ## 莊家
  # 判斷玩家是否補牌
  check10 <- check_pop(ch_hand, point=16, ch_sum, desk_card)
  ch_sum <- check10$x ; desk_card <- check10$y ; ch_hand <- check10$z
  
  # 若點數合超過21且包含A, 將A變為1
  check11 <- check_A21(ch_sum, ch_hand)
  ch_sum <- check11$x ; ch_hand <- check11$y
  
  # A轉為1後再次判斷是否要牌
  check12 <- check_pop_A(ch_hand, point=16, ch_sum, desk_card)
  ch_sum <- check12$x ; desk_card <- check12$y ; ch_hand <- check12$z
  
  # 超過21點爆牌
  
  ch_sum <- ifelse(( ch_sum <= 21), ch_sum, 0 )
  
  
  ## 勝敗
  result <- ifelse(my_sum> ch_sum, 1, ifelse(my_sum < ch_sum, -1, 0) )
  
  # 算牌
  serial_num <- serial_count(my_hand, ch_hand)
  
  desk_final = desk_card
  return(list("result" = result, "serial" = serial_num, "desk" = desk_final))
  
}

# 模擬一局
game_start <- function(point1,num_of_desk){
  
  serial_num = 0
  desk_card1 <- desk(num_of_desk)
  last_card = 52 * num_of_desk
  while(last_card >= 52 * num_of_desk/2){
    end <- game(point1, desk_card1)
    
    winloss <- c(winloss, end[[1]])
    serial_all <- c(serial_all, end[[2]])
    desk_card1 <- end$desk
    
    last_card <- length(end$desk)
    jen <- end[[2]]/(last_card/52)
    jen2 <- c(jen2,jen)
  }
  
  return(list("winloss" = winloss, "serial_all" = serial_all, "jen" = round(jen2,2)))
}

trygame <- function(point1, num_of_desk, money5){
  output = c()
  
  for (m in 1:50){
    final2 = c()
    serial_all2 = NULL
    
    for(j in 1:100){
      serial_num = 0
      winloss = NULL ; serial_all = NULL ; money = NULL ; jen2 = NULL
      
      a = game_start(point1,num_of_desk)
      a[1]
      a[2]
      money2 = (floor(a[[3]]+1)-1) 
      for (i in 1:length(money2)){
        if (money2[i] <= 2){
          money[i] = 1 * money5
        } else if (money2[i] == 3){
          money[i] = 2 * money5
        } else if (money2[i] == 4){
          money[i] = 3 * money5
        } else if (money2[i] == 5){
          money[i] = 4 * money5
        } else if (money2[i] >= 6){
          money[i] = 5 * money5
        }
      }
      final = sum(money*a[[1]])
      final2 = c(final2, final)
      serial_all2 = c(serial_all2, a[[2]])
    }
    output = c(output, sum(final2))
  }
  
  return(output)
  
}

##############################
# 牌數4 閥值13 $10 
a <- trygame(13,4,10)
c(mean(a),sd(a))

# 牌數5 閥值13 $10 
a <- trygame(13,5,10)
c(mean(a),sd(a))

# 牌數6 閥值13 $10 
a <- trygame(13,6,10)
c(mean(a),sd(a))

# 牌數7 閥值13 $10 
a <- trygame(13,7,10)
c(mean(a),sd(a))

# 牌數8 閥值13 $10 
a <- trygame(13,8,10)
c(mean(a),sd(a))

####

# 牌數4 閥值14 $10 
a <- trygame(14,4,10)
c(mean(a),sd(a))

# 牌數5 閥值14 $10 
a <- trygame(14,5,10)
c(mean(a),sd(a))

# 牌數6 閥值14 $10 
a <- trygame(14,6,10)
c(mean(a),sd(a))

# 牌數7 閥值14 $10 
a <- trygame(14,7,10)
c(mean(a),sd(a))

# 牌數8 閥值14 $10 
a <- trygame(14,8,10)
c(mean(a),sd(a))


####

# 牌數4 閥值15 $10 
a <- trygame(15,4,10)
c(mean(a),sd(a))

# 牌數5 閥值15 $10 
a <- trygame(15,5,10)
c(mean(a),sd(a))

# 牌數6 閥值15 $10 
a <- trygame(15,6,10)
c(mean(a),sd(a))

# 牌數7 閥值15 $10 
a <- trygame(15,7,10)
c(mean(a),sd(a))

# 牌數8 閥值15 $10 
a <- trygame(15,8,10)
c(mean(a),sd(a))


####

# 牌數4 閥值16 $10 
a <- trygame(16,4,10)
c(mean(a),sd(a))

# 牌數5 閥值16 $10 
a <- trygame(16,5,10)
c(mean(a),sd(a))

# 牌數6 閥值16 $10 
a <- trygame(16,6,10)
c(mean(a),sd(a))

# 牌數7 閥值16 $10 
a <- trygame(16,7,10)
c(mean(a),sd(a))

# 牌數8 閥值16 $10 
a <- trygame(16,8,10)
c(mean(a),sd(a))


###########################
####
# 牌數4 閥值13 $50 
a <- trygame(13,4,50)
c(mean(a),sd(a))

# 牌數5 閥值13 $50
a <- trygame(13,5,50)
c(mean(a),sd(a))

# 牌數6 閥值13 $50 
a <- trygame(13,6,50)
c(mean(a),sd(a))

# 牌數7 閥值13 $50 
a <- trygame(13,7,50)
c(mean(a),sd(a))

# 牌數8 閥值13 $50 
a <- trygame(13,8,50)
c(mean(a),sd(a))

####

# 牌數4 閥值14 $50
a <- trygame(14,4,50)
c(mean(a),sd(a))

# 牌數5 閥值14 $50
a <- trygame(14,5,50)
c(mean(a),sd(a))

# 牌數6 閥值14 $50 
a <- trygame(14,6,50)
c(mean(a),sd(a))

# 牌數7 閥值14 $50
a <- trygame(14,7,50)
c(mean(a),sd(a))

# 牌數8 閥值14 $50
a <- trygame(14,8,50)
c(mean(a),sd(a))


####

# 牌數4 閥值15 $50
a <- trygame(15,4,50)
c(mean(a),sd(a))

# 牌數5 閥值15 $50
a <- trygame(15,5,50)
c(mean(a),sd(a))

# 牌數6 閥值15 $50 
a <- trygame(15,6,50)
c(mean(a),sd(a))

# 牌數7 閥值15 $50
a <- trygame(15,7,50)
c(mean(a),sd(a))

# 牌數8 閥值15 $50
a <- trygame(15,8,50)
c(mean(a),sd(a))


####

# 牌數4 閥值16 $50
a <- trygame(16,4,50)
c(mean(a),sd(a))

# 牌數5 閥值16 $50
a <- trygame(16,5,50)
c(mean(a),sd(a))

# 牌數6 閥值16 $50
a <- trygame(16,6,50)
c(mean(a),sd(a))

# 牌數7 閥值16 $50
a <- trygame(16,7,50)
c(mean(a),sd(a))

# 牌數8 閥值16 $50
a <- trygame(16,8,50)
c(mean(a),sd(a))


################  7人 50次  ################
# 模擬 7人遊戲過程
game <- function(point1, desk_card){
  
  # 玩家手牌
  my_hand <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家2手牌
  my_hand2 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家3手牌
  my_hand3 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家4手牌
  my_hand4 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家5手牌
  my_hand5 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 玩家6手牌
  my_hand6 <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  # 莊家手牌
  ch_hand <- c(select_card_start(desk_card)) #初始手牌
  desk_card <- new_desk(desk_card) ; desk_card <- new_desk(desk_card) #更新
  
  
  
  # 判斷 ACE
  Ato11 <- my_hand ; Ato112 <- ch_hand ; Ato22 <- my_hand2 ; Ato33 <- my_hand3 
  Ato11[Ato11==1] <- 11 ; Ato112[Ato112==1] <- 11 ; Ato22[Ato22==1] <- 11 ; Ato33[Ato33==1] <- 11
  my_sum <- sum(Ato11) ; ch_sum <- sum(Ato112) ; my_sum2 <- sum(Ato22) ; my_sum3 <- sum(Ato33)
  
  ## 玩家
  # 判斷玩家是否補牌
  check <- check_pop(my_hand, point1, my_sum, desk_card)
  my_sum <- check$x ; desk_card <- check$y ; my_hand <- check$z
  
  # 若點數合超過21且包含A, 將A變為1
  check2 <- check_A21(my_sum, my_hand)
  my_sum <- check2$x ; my_hand <- check2$y
  
  # A轉為1後再次判斷是否要牌
  check1 <- check_pop_A(my_hand, point1, my_sum, desk_card)
  my_sum <- check1$x ; desk_card <- check1$y ; my_hand <- check1$z
  
  # 超過21點爆牌
  
  my_sum <- ifelse(( my_sum <= 21), my_sum, 0 )
  
  ## 玩家2
  # 判斷玩家是否補牌
  check22 <- check_pop(my_hand2, point1, my_sum, desk_card)
  my_sum22 <- check22$x ; desk_card <- check22$y ; my_hand22 <- check22$z
  
  # 若點數合超過21且包含A, 將A變為1
  check222 <- check_A21(my_sum22, my_hand22)
  my_sum22 <- check222$x ; my_hand22 <- check222$y
  
  # A轉為1後再次判斷是否要牌
  check122 <- check_pop_A(my_hand22, point1, my_sum22, desk_card)
  my_sum22 <- check122$x ; desk_card <- check1$y ; my_hand <- check122$z
  
  # 超過21點爆牌
  
  my_sum22 <- ifelse(( my_sum22 <= 21), my_sum22, 0 )
  
  ## 玩家3
  # 判斷玩家是否補牌
  check33 <- check_pop(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check33$x ; desk_card <- check33$y ; my_hand33 <- check33$z
  
  # 若點數合超過21且包含A, 將A變為1
  check233 <- check_A21(my_sum33, my_hand33)
  my_sum33 <- check233$x ; my_hand33 <- check233$y
  
  # A轉為1後再次判斷是否要牌
  check133 <- check_pop_A(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check133$x ; desk_card <- check133$y ; my_hand33 <- check133$z
  
  # 超過21點爆牌
  
  my_sum33 <- ifelse(( my_sum33 <= 21), my_sum33, 0 )
  
  ## 玩家4
  # 判斷玩家是否補牌
  check33 <- check_pop(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check33$x ; desk_card <- check33$y ; my_hand33 <- check33$z
  
  # 若點數合超過21且包含A, 將A變為1
  check233 <- check_A21(my_sum33, my_hand33)
  my_sum33 <- check233$x ; my_hand33 <- check233$y
  
  # A轉為1後再次判斷是否要牌
  check133 <- check_pop_A(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check133$x ; desk_card <- check133$y ; my_hand33 <- check133$z
  
  # 超過21點爆牌
  
  my_sum33 <- ifelse(( my_sum33 <= 21), my_sum33, 0 )
  
  ## 玩家5
  # 判斷玩家是否補牌
  check33 <- check_pop(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check33$x ; desk_card <- check33$y ; my_hand33 <- check33$z
  
  # 若點數合超過21且包含A, 將A變為1
  check233 <- check_A21(my_sum33, my_hand33)
  my_sum33 <- check233$x ; my_hand33 <- check233$y
  
  # A轉為1後再次判斷是否要牌
  check133 <- check_pop_A(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check133$x ; desk_card <- check133$y ; my_hand33 <- check133$z
  
  # 超過21點爆牌
  
  my_sum33 <- ifelse(( my_sum33 <= 21), my_sum33, 0 )
  
  ## 玩家6
  # 判斷玩家是否補牌
  check33 <- check_pop(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check33$x ; desk_card <- check33$y ; my_hand33 <- check33$z
  
  # 若點數合超過21且包含A, 將A變為1
  check233 <- check_A21(my_sum33, my_hand33)
  my_sum33 <- check233$x ; my_hand33 <- check233$y
  
  # A轉為1後再次判斷是否要牌
  check133 <- check_pop_A(my_hand3, point1, my_sum3, desk_card)
  my_sum33 <- check133$x ; desk_card <- check133$y ; my_hand33 <- check133$z
  
  # 超過21點爆牌
  
  my_sum33 <- ifelse(( my_sum33 <= 21), my_sum33, 0 )
  
  ## 莊家
  # 判斷玩家是否補牌
  check10 <- check_pop(ch_hand, point=16, ch_sum, desk_card)
  ch_sum <- check10$x ; desk_card <- check10$y ; ch_hand <- check10$z
  
  # 若點數合超過21且包含A, 將A變為1
  check11 <- check_A21(ch_sum, ch_hand)
  ch_sum <- check11$x ; ch_hand <- check11$y
  
  # A轉為1後再次判斷是否要牌
  check12 <- check_pop_A(ch_hand, point=16, ch_sum, desk_card)
  ch_sum <- check12$x ; desk_card <- check12$y ; ch_hand <- check12$z
  
  # 超過21點爆牌
  
  ch_sum <- ifelse(( ch_sum <= 21), ch_sum, 0 )
  
  
  ## 勝敗
  result <- ifelse(my_sum> ch_sum, 1, ifelse(my_sum < ch_sum, -1, 0) )
  
  # 算牌
  serial_num <- serial_count(my_hand, ch_hand)
  
  desk_final = desk_card
  return(list("result" = result, "serial" = serial_num, "desk" = desk_final))
  
}

# 模擬一局
game_start <- function(point1,num_of_desk){
  
  serial_num = 0
  desk_card1 <- desk(num_of_desk)
  last_card = 52 * num_of_desk
  while(last_card >= 52 * num_of_desk/2){
    end <- game(point1, desk_card1)
    
    winloss <- c(winloss, end[[1]])
    serial_all <- c(serial_all, end[[2]])
    desk_card1 <- end$desk
    
    last_card <- length(end$desk)
    jen <- end[[2]]/(last_card/52)
    jen2 <- c(jen2,jen)
  }
  
  return(list("winloss" = winloss, "serial_all" = serial_all, "jen" = round(jen2,2)))
}

trygame <- function(point1, num_of_desk, money5){
  output = c()
  
  for (m in 1:50){
    final2 = c()
    serial_all2 = NULL
    
    for(j in 1:100){
      serial_num = 0
      winloss = NULL ; serial_all = NULL ; money = NULL ; jen2 = NULL
      
      a = game_start(point1,num_of_desk)
      a[1]
      a[2]
      money2 = (floor(a[[3]]+1)-1) 
      for (i in 1:length(money2)){
        if (money2[i] <= 2){
          money[i] = 1 * money5
        } else if (money2[i] == 3){
          money[i] = 2 * money5
        } else if (money2[i] == 4){
          money[i] = 3 * money5
        } else if (money2[i] == 5){
          money[i] = 4 * money5
        } else if (money2[i] >= 6){
          money[i] = 5 * money5
        }
      }
      final = sum(money*a[[1]])
      final2 = c(final2, final)
      serial_all2 = c(serial_all2, a[[2]])
    }
    output = c(output, sum(final2))
  }
  
  return(output)
  
}

##############################
# 牌數4 閥值13 $10 
a <- trygame(13,4,10)
c(mean(a),sd(a))

# 牌數5 閥值13 $10 
a <- trygame(13,5,10)
c(mean(a),sd(a))

# 牌數6 閥值13 $10 
a <- trygame(13,6,10)
c(mean(a),sd(a))

# 牌數7 閥值13 $10 
a <- trygame(13,7,10)
c(mean(a),sd(a))

# 牌數8 閥值13 $10 
a <- trygame(13,8,10)
c(mean(a),sd(a))

####

# 牌數4 閥值14 $10 
a <- trygame(14,4,10)
c(mean(a),sd(a))

# 牌數5 閥值14 $10 
a <- trygame(14,5,10)
c(mean(a),sd(a))

# 牌數6 閥值14 $10 
a <- trygame(14,6,10)
c(mean(a),sd(a))

# 牌數7 閥值14 $10 
a <- trygame(14,7,10)
c(mean(a),sd(a))

# 牌數8 閥值14 $10 
a <- trygame(14,8,10)
c(mean(a),sd(a))


####

# 牌數4 閥值15 $10 
a <- trygame(15,4,10)
c(mean(a),sd(a))

# 牌數5 閥值15 $10 
a <- trygame(15,5,10)
c(mean(a),sd(a))

# 牌數6 閥值15 $10 
a <- trygame(15,6,10)
c(mean(a),sd(a))

# 牌數7 閥值15 $10 
a <- trygame(15,7,10)
c(mean(a),sd(a))

# 牌數8 閥值15 $10 
a <- trygame(15,8,10)
c(mean(a),sd(a))


####

# 牌數4 閥值16 $10 
a <- trygame(16,4,10)
c(mean(a),sd(a))

# 牌數5 閥值16 $10 
a <- trygame(16,5,10)
c(mean(a),sd(a))

# 牌數6 閥值16 $10 
a <- trygame(16,6,10)
c(mean(a),sd(a))

# 牌數7 閥值16 $10 
a <- trygame(16,7,10)
c(mean(a),sd(a))

# 牌數8 閥值16 $10 
a <- trygame(16,8,10)
c(mean(a),sd(a))


###########################
####
# 牌數4 閥值13 $50 
a <- trygame(13,4,50)
c(mean(a),sd(a))

# 牌數5 閥值13 $50
a <- trygame(13,5,50)
c(mean(a),sd(a))

# 牌數6 閥值13 $50 
a <- trygame(13,6,50)
c(mean(a),sd(a))

# 牌數7 閥值13 $50 
a <- trygame(13,7,50)
c(mean(a),sd(a))

# 牌數8 閥值13 $50 
a <- trygame(13,8,50)
c(mean(a),sd(a))

####

# 牌數4 閥值14 $50
a <- trygame(14,4,50)
c(mean(a),sd(a))

# 牌數5 閥值14 $50
a <- trygame(14,5,50)
c(mean(a),sd(a))

# 牌數6 閥值14 $50 
a <- trygame(14,6,50)
c(mean(a),sd(a))

# 牌數7 閥值14 $50
a <- trygame(14,7,50)
c(mean(a),sd(a))

# 牌數8 閥值14 $50
a <- trygame(14,8,50)
c(mean(a),sd(a))


####

# 牌數4 閥值15 $50
a <- trygame(15,4,50)
c(mean(a),sd(a))

# 牌數5 閥值15 $50
a <- trygame(15,5,50)
c(mean(a),sd(a))

# 牌數6 閥值15 $50 
a <- trygame(15,6,50)
c(mean(a),sd(a))

# 牌數7 閥值15 $50
a <- trygame(15,7,50)
c(mean(a),sd(a))

# 牌數8 閥值15 $50
a <- trygame(15,8,50)
c(mean(a),sd(a))


####

# 牌數4 閥值16 $50
a <- trygame(16,4,50)
c(mean(a),sd(a))

# 牌數5 閥值16 $50
a <- trygame(16,5,50)
c(mean(a),sd(a))

# 牌數6 閥值16 $50
a <- trygame(16,6,50)
c(mean(a),sd(a))

# 牌數7 閥值16 $50
a <- trygame(16,7,50)
c(mean(a),sd(a))

# 牌數8 閥值16 $50
a <- trygame(16,8,50)
c(mean(a),sd(a))


###############  收益圖  ################

# 牌數8 閥值16 $50

final2 = 0 ; serial_all2 = c()
  for(j in 1:100){
    serial_num = 0
    winloss = NULL ; serial_all = NULL ; money = NULL ; jen2 = NULL
    
    a = game_start(16,8)
    a[1]
    a[2]
    money2 = (floor(a[[3]]+1)-1) 
    for (i in 1:length(money2)){
      if (money2[i] <= 2){
        money[i] = 1 * 50
      } else if (money2[i] == 3){
        money[i] = 2 * 50
      } else if (money2[i] == 4){
        money[i] = 3 * 50
      } else if (money2[i] == 5){
        money[i] = 4 * 50
      } else if (money2[i] >= 6){
        money[i] = 5 * 50
      }
    }
    final = sum(money * a[[1]])
    final2 = c(final2, final)
    serial_all2 = c(serial_all2, a[[2]])
  }


sum(final2)
profit3 = cumsum(final2)

plot(profit, type = "l", xlab = "time", main = "profit plot ( 8副牌 叫牌閥值16 下注金額$50)", ylim = c(-1500,2000))
lines(profit2, col = "red", lty = 2 )
lines(profit3, col = "blue", lty = 4 )
legend("topleft", lty = c(1,2,4), c("1","2","3"), col = c("black","red","blue"))


###############  流水數次數與勝率  ################

# 牌數8 閥值16 $50

win_list = c() ; water_list = c() ; final2 = 0 ; serial_all2 = c()

for(j in 1:100){
  serial_num = 0
  winloss = NULL ; serial_all = NULL ; money = NULL ; jen2 = NULL
  
  a = game_start(16,8)
  win = a[[1]]
  water = a[[2]]
  win_list = c(win_list, win)
  water_list = c(water_list, water)
  
  # 計算報酬
  money2 = (floor(a[[3]]+1)-1) 
  for (i in 1:length(money2)){
    if (money2[i] <= 2){
      money[i] = 1 * 50
    } else if (money2[i] == 3){
      money[i] = 2 * 50
    } else if (money2[i] == 4){
      money[i] = 3 * 50
    } else if (money2[i] == 5){
      money[i] = 4 * 50
    } else if (money2[i] >= 6){
      money[i] = 5 * 50
    }
  }
  final = sum(money * a[[1]])
  final2 = c(final2, final)
  serial_all2 = c(serial_all2, a[[2]])
}
sum(final2)


# 出現次數
table(water_list)
total = rbind(win_list, water_list)

# 勝率
sum(total[1,which(total[2,]==6)]==1) / length(total[1,which(total[2,]==6)])
sum(total[1,which(total[2,]==5)]==1) / length(total[1,which(total[2,]==5)])
sum(total[1,which(total[2,]==4)]==1) / length(total[1,which(total[2,]==4)])
sum(total[1,which(total[2,]==3)]==1) / length(total[1,which(total[2,]==3)])
sum(total[1,which(total[2,]==2)]==1) / length(total[1,which(total[2,]==2)])
sum(total[1,which(total[2,]==1)]==1) / length(total[1,which(total[2,]==1)])
sum(total[1,which(total[2,]==0)]==1) / length(total[1,which(total[2,]==0)])
sum(total[1,which(total[2,]==-1)]==1) / length(total[1,which(total[2,]==-1)])
sum(total[1,which(total[2,]==-2)]==1) / length(total[1,which(total[2,]==-2)])
sum(total[1,which(total[2,]==-3)]==1) / length(total[1,which(total[2,]==-3)])
sum(total[1,which(total[2,]==-4)]==1) / length(total[1,which(total[2,]==-4)])
sum(total[1,which(total[2,]==-5)]==1) / length(total[1,which(total[2,]==-5)])
sum(total[1,which(total[2,]==-6)]==1) / length(total[1,which(total[2,]==-6)])

# 敗率
sum(total[1,which(total[2,]==6)]==0) / length(total[1,which(total[2,]==6)])
sum(total[1,which(total[2,]==5)]==0) / length(total[1,which(total[2,]==5)])
sum(total[1,which(total[2,]==4)]==0) / length(total[1,which(total[2,]==4)])
sum(total[1,which(total[2,]==3)]==0) / length(total[1,which(total[2,]==3)])
sum(total[1,which(total[2,]==2)]==0) / length(total[1,which(total[2,]==2)])
sum(total[1,which(total[2,]==1)]==0) / length(total[1,which(total[2,]==1)])
sum(total[1,which(total[2,]==0)]==0) / length(total[1,which(total[2,]==0)])
sum(total[1,which(total[2,]==-1)]==0) / length(total[1,which(total[2,]==-1)])
sum(total[1,which(total[2,]==-2)]==0) / length(total[1,which(total[2,]==-2)])
sum(total[1,which(total[2,]==-3)]==0) / length(total[1,which(total[2,]==-3)])
sum(total[1,which(total[2,]==-4)]==0) / length(total[1,which(total[2,]==-4)])
sum(total[1,which(total[2,]==-5)]==0) / length(total[1,which(total[2,]==-5)])
sum(total[1,which(total[2,]==-6)]==0) / length(total[1,which(total[2,]==-6)])



# 勝率
g = c(sum(total[1,which(total[2,]==-6)]==1) / length(total[1,which(total[2,]==-6)]),
sum(total[1,which(total[2,]==-5)]==1) / length(total[1,which(total[2,]==-5)]),
sum(total[1,which(total[2,]==-4)]==1) / length(total[1,which(total[2,]==-4)]),
sum(total[1,which(total[2,]==-3)]==1) / length(total[1,which(total[2,]==-3)]),
sum(total[1,which(total[2,]==-2)]==1) / length(total[1,which(total[2,]==-2)]),
sum(total[1,which(total[2,]==-1)]==1) / length(total[1,which(total[2,]==-1)]),
sum(total[1,which(total[2,]==-0)]==1) / length(total[1,which(total[2,]==0)]),
sum(total[1,which(total[2,]==1)]==1) / length(total[1,which(total[2,]==1)]),
sum(total[1,which(total[2,]==2)]==1) / length(total[1,which(total[2,]==2)]),
sum(total[1,which(total[2,]==3)]==1) / length(total[1,which(total[2,]==3)]),
sum(total[1,which(total[2,]==4)]==1) / length(total[1,which(total[2,]==4)]),
sum(total[1,which(total[2,]==5)]==1) / length(total[1,which(total[2,]==5)]),
sum(total[1,which(total[2,]==6)]==1) / length(total[1,which(total[2,]==6)]))

plot(g, x = c(-6:6), type = "o", ylim = c(0,1), xlab = "流水數", ylab = "勝率", main = "各流水數勝率圖")

