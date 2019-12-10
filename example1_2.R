#
# R 프로그래밍 1일차
#
# first.R
#
# 작성자 : wolee777
#
# 최초작성일 : 2019. 11. 26
#
#
print( "Hello, World!!!" )

number <- 10
number2 = 100

number3 <- number 


numberValue <- 1              # camel 표기법
str_value <- "R Language"     # snake 표기법
booleanvalue <- TRUE

numberValue <- "R script"
numberValue <- 1

print( numberValue )
print( str_value )
print( booleanvalue )


cat( "Numeric number : ", numberValue, "\n" )
cat( "String : ", str_value, "\n" )
cat( "Boolean value : ", booleanvalue, "\n" )


numberValue <- scan()
cat( "Numeric number : ", numberValue, "\n" )

number1 <- 10
number2 <- 20
resultAdd <- number1 + number2
resultSub <- number1 - number2
resultMul <- number1 * number2
resultDiv <- number1 / number2
resultRem <- number2 %% number1
resultSec <- number2 ** 5

print( resultAdd )
print( resultSub )
print( resultMul )
print( resultDiv )
print( resultRem )
print( resultSec )

number1 <- 0
number1 <- number1 + 10
number1
number1 <- number1 + 10
number1
number1 <- number1 + 10
number1

number2 <- 100
number1 <- number2 + 10
number1
number2


print( ( number1 + 10 ) * number2 / 2 )

number300 <- 10
number300 <- number300 + 10
number300

number301 <- number300 * 2
number301

number1 <- 10.5
number2 <- 10
print( number1 > number2 )
print( number1 >= number2 )
print( number1 < number2 )
print( number1 <= number2 )
print( number1 == number2 )
print( number1 != number2 )

print( number1 > 10 & number2 > 10 )  # AND
print( number1 > 10 | number2 > 10 )  # OR
print( !( number1 > 10 ) )            # NOT

number <- "100"
str <- "R Language"
result = number + str 
print( result )

#
# 제어구조 - 선택구조
#
job.type <- 'A'

if ( job.type == 'B' ) {
  bonus <- 200            # 참일때
} else {
  bonus <- 100            # 거짓 일때
}
cat( "joy type : ", job.type, "\t\tbonus : ", bonus )




job.type <- 'B'

if ( job.type == 'A' ){
  bonus <- 200
}
cat( "joy type : ", job.type, "\t\tbonus : ", bonus )


score <- 85

if ( score >= 90 ) {
  grade <- 'A'
} else if ( score >= 80 ) {
  grade <- 'B'
} else if ( score >= 70 ) {
  grade <- 'C'
} else if ( score >= 60 ) {
  grade <- 'D'
} else {
  grade <- 'F'
}
cat( "score : ", score, "\t\tgrade : ", grade )




number <- 10
remainder <- number %% 2

#if ( remainder == 0 ) {
if ( number %% 2 == 0 ) {
  oddeven <- '짝수'
} else {
  oddeven <- '홀수'
}

cat( "Number : ", number, "는", oddeven, " 이다." )



a <- 5
b <- 20

if ( a > 5 & b > 5 ) {
  cat( a, " > 5 and ", b, " > 5 " )
} else {
  cat( a, " <= 5 or ", b, " <= 5 " )
}


a <- 10
b <- 20

if ( a > b ) {
  c <- a
} else {
  c <- b
}
cat( "a = ", a, "\tb = ", b, "\tc = ", c )

c <- ifelse( a > b, a, b )
cat( "a = ", a, "\tb = ", b, "\tc = ", c )



a <- 8
b <- 5
c <- 10
max <- a

if ( b > max ) {
  max = b
}
if ( c > max ) {
  max = c
}
cat( "a = ", a, " b = ", b, " c = ", c, " max = ", max )

#
# 반복구조
#
# for문
for ( i in 1:10 ) {
  print( '*' )
}

multiple = 2
for ( i in 2:9 ) {
  cat( multiple, ' X ', i, ' = ', multiple * i, '\n' )
}

# while문
i <- 1
while ( i <= 10 ) {
  print( i )
  i <- i + 1
}

multiple <- 2
i <- 2
while ( i <= 9 ) {
  cat( multiple, ' X ', i, ' = ', multiple * i, '\n' )
  i <- i + 1
}

# 1
lineCount <- 1                # 초기화
for ( i in 1:100 ) {
  cat( i, ' ' )
  lineCount <- lineCount + 1
  if ( lineCount > 10 ) {
    print( '\n' )
    lineCount <- 1            # Reset
  }
}

# 2
for ( i in 1:100 ) {
  cat( i, ' ' )
  if ( i %% 10 == 0 ) {
    print( '\n' )
  }
}



#
# 1 ~ 1000사이의 3의배수와 5의 배수를 한 줄에 10개씩 출력하고
# 마지막에 개수를 출력
#
i <- 1
count <- 0
lineCount <- 1
multiple3 <- NULL
multiple5 <- NULL
while ( i <= 1000 ) {
  multiple3 <- i %% 3
  multiple5 <- i %% 5
  if ( multiple3 == 0 | multiple5 == 0 ) {
    count <- count + 1
    cat( i, " " )
    lineCount <- lineCount + 1
    if ( lineCount > 10 ) {
      lineCount <- 1
      print( '\n' )
    }
  }
  i <- i + 1
}
print( '\n' )
cat( '1 ~ 1000사이의 3의 배수와 5의 배수의 개수 : ', count, '\n' )

i <- 2
j <- 2
while ( i <= 9 ) {
  for ( j in 2:9 ) {
    cat( i, ' X ', j, ' = ', i * j, '\n' )
  }
  print( '\n')
  i <- i + 1
}









































  
  
  
  
  
)


































