(load "interpreter.scm")
(load "classParser.scm")
(require rackunit)

; Part Four Tests ******************************************************

(check-eq? (interpret "tests/4/1.txt" "A") 15)
(check-eq? (interpret "tests/4/2.txt" "A") 12)
(check-eq? (interpret "tests/4/3.txt" "A") 125)
(check-eq? (interpret "tests/4/4.txt" "A") 36)
(check-eq? (interpret "tests/4/5.txt" "A") 54)
(check-eq? (interpret "tests/4/6.txt" "A") 110)
(check-eq? (interpret "tests/4/7.txt" "C") 26)
(check-eq? (interpret "tests/4/8.txt" "Square") 117)
(check-eq? (interpret "tests/4/9.txt" "Square") 32)
(check-eq? (interpret "tests/4/10.txt" "List") 15)
(check-eq? (interpret "tests/4/11.txt" "List") 123456)
(check-eq? (interpret "tests/4/12.txt" "List") 5285)
(check-eq? (interpret "tests/4/13.txt" "C") -716)

(display "All tests completed!")