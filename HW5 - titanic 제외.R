#143~144

#1. 통계 자료를 accident 벡터에 저장합니다.
accident <- c(31, 26, 42,47, 50,54,70,66,44,32,32,22)
names(accident) <- c('M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12')
accident

#2. 1년간 총 감전 사고 건수를 알아봅니다.
sum(accident)


#3. 가장 사고가 많은 달과 가장 적은 달의 건수를 알아 봅니다. 
max(accident)
min(accident)

#4. 만일 사고율이 10% 감소 한다면 사고 건수가 어떻게 변화하는지 구해봅니다. 
accident*0.9

#5. 사고 건수가 50건을 넘는 달의 통계만 출력해 봅니다.
accident[accident>=50]

#6.사고 건수가 50건을 넘는 달의 이름을 출력해 봅니다. 
month.50 <- accident[accident>=50]
names[month.50]
names(accident[accident >=50])

#7. 사고 건수가 50미만인 달은 1년 중 몇 개월인지 구해봅니다. 
length(accident[accident<50])

#8. 6월보다 사고가 많은 달과 사고 건수를 구해봅니다. 
M6.acc <- accident[6]
accident[accident >M6.acc]
  

#p147
#4. 벡터a 의 값 중 3의 배수이면서 100보다 작은 수의 갯수를 구하는 
#명령문을 완성하시오 
a <- 25:150
condi <- a[(a%%3 ==0) & (a<100)]
print(length(condi))


#148p
#8. 어떤 엘리베이터의 탑승 가능 총 무겔는 600kg 인데 마지막 사람이 타는 
#순간 운행 무게가 초과했다.
#weight는 엘리베이터에 탑승한 사람들의 이름과 몸무게이다. 

# 조건 한 사람을 내리게 하여 운행이 가능하도록 한다. 
# 조건 내려야 하는 대상 중 가장 몸무게가 적은 사람을 선택한다. 
weight <- c(56,23,89,46,76,14,97,72,68,62,35)
names(weight) <- c('a','b','c','d','e','f','g','h','i','j','k')
sum(weight)
weight[1]
weight[2]
# out_weight 는 누가 나가야지 엘베 무게가 600 이하가 되는지 알수 있음. 
out_weight <- weight[sum(weight) - weight <600]
print(out_weight)
# 나가야되는 사람들의 무게 중 제일 가벼운 사람
min_out_weight <- min(out_weight)
# 최종적으로 누가 나가야되는가? 나가야되는 사람들 중 몸무게가 제일 작은 
# 사람을 내보내자! 
out_ppl <- out_weight[out_weight == min_out_weight]
out_ppl
