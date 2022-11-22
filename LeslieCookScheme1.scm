;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LESLIE COOK 
; LISP Scheme Program 1
; CMPS 5113 Fall 2022
; The goal of this program is to determine if a value is a perfect number by summing a list of the values proper divisors. 
; if the sum of the list is equal to the value input by the user, then it is a perfect number
; if it is greater than the input value, it is abundant 
; if it is less than the input value, it is deficient
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Leslie Cook\n")
(print "LISP Scheme program 1\n")

; ask user to input a target value
(print "Enter a positive integer: ")
; read in the target value for further computation
(define target (read))

; funtion name: divisors
; parameters: target value from user input
(define (divisors target)
  
; procedure generator: divisorList 
; parameter: target input by user
; decrement the target value by 1 to continue checking divisor values
(divisorsList target (- target 1)))
; gives list of all divisor of target less than or equal to the divisor values


; function name: divisorsList
; parameters: target input by user and divisor values
(define (divisorsList target values)
  
; conditional: if the divisor values is less than 1 
; return nothing
  (cond ((< values 1)  ())
      
; if the modular of the target and divisor values
; is equal to zero it is a proper divisor
    ((= 0 (mod target values))

; if the value is a proper divisor 
; add the value  (cons) to divsorsList 
; subtract 1 from the divisor values
; recursively call divisorList again
      (cons values (divisorsList target (- values 1))))
; else: recursive call divisors list to continue to determine the proper divisors
      (else (divisorsList target (- values 1)))))

; redefine the list of diviors in
; another list names l1
(define l1 (divisors target))

; function name: sum
; parameters: list of divisors and a running total 
(define (sum l1 total)
; if the list is null return the total
  (if (null? l1)
        total
; else recursively call sum and add it to the total 
          (sum  (cdr l1) ( + (car l1) total))))

; statement to display the list of proper divisors
(print "\nThe proper divisors are: ")
(display l1)
(print "\n")

; redefine the sum function to pass in as a parameter
; for comparison with the target value
(define SumDivisors (sum l1 0))
; conditional statements to check for perfect numbers
    (cond 
; if the sum of the proper divisors is equal to the target number, it is a perfect number
        (( = target SumDivisors ) (print "This number is Perfect\n"))
; if the sum of the proper divisors is less than the target number, it is a deficient number
        (( > target SumDivisors) (print "\nThis number is Deficient\n"))
; if the sum of the proper divisors is greater thab the target number, it is an abundant number
        (( < target SumDivisors) (print "\nThis number is Abundant\n")))

; function call to execute the program
(sum l1 0)