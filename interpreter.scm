; Jesse Cavendish

; Project Part 4
; Passes 4/13 tests

; Need to create a better try-catch-finally function that can interpret nested try-catch-finally

; Interpreter *************************************************************************************************************

; Loads the function parser which parses the C/Java like code into code Scheme can interpret
(load "classParser.scm")

; Begins interpreting a user-input text file that contains C/Java like code
(define interpret
  (lambda (file_name class_name)
    (interpret_outer_environment (parser file_name) class_name new_state (lambda (break) break) (lambda (continue) continue) (lambda (throw) throw))))

; Interprets the outer environment of the file which includes global variables and function by storing them in the state
; Once everything has stored, the main function is called
(define interpret_outer_environment
  (lambda (code class_name state break continue throw)
    (call/cc
     (lambda (return)
       (cond
         ((null? code) (interpret_function (main_body (state_lookup_variable 'main (state_lookup_variable (string->symbol class_name) state))) (cons new_layer state) break continue throw (string->symbol class_name) empty (string->symbol class_name)))
         (else (interpret_outer_environment (next_code code) class_name (M_state (current_code code) state break continue throw return empty empty)  break continue throw)))))))

; Used to interpret functions such as the main and other functions
(define interpret_function
  (lambda (body state break continue throw parent_name instance_name function_type_name)
    (call/cc
     (lambda (return)
       (cond
         ((null? body) state)
         (else (interpret_function (next_statements body) (M_state (current_statement body) state break continue throw return parent_name instance_name) break continue throw parent_name instance_name function_type_name)))))))

; Used to interpret functions such as the main and other functions
(define interpret_class
  (lambda (code state break continue throw)
    (call/cc
     (lambda (return)
       (cond
         ((null? code) state)
         (else (interpret_class (next_code code) (M_state (current_code code) state break continue throw return empty empty) break continue throw)))))))

; Interpreter Abstractions
(define new_state (list (list empty empty)))
(define new_layer (list empty empty))
(define main_body cadr)
(define current_code car)
(define next_code cdr)
(define current_statement car)
(define next_statements cdr)

; M_state *************************************************************************************************************

; Updates the state based on the the operator that is passed by a statement
(define M_state
  (lambda (statement state break continue throw return parent_name instance_name)
    (cond
      ((null? statement) state)
      ((number? statement) statement)
      ((state_declared? (operator statement) state) (state_lookup_variable (operator statement) state))
      ((eq? (operator statement) 'var) (M_state_declare (rhs statement) (lhs statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) '=) (M_state_assign (rhs statement) (lhs statement) state break continue throw return parent_name instance_name))   
      ((eq? (operator statement) 'if) (if (null? (else_exists statement))
                                          (M_state_if_else (conditional statement) (if* statement) empty state break continue throw return parent_name instance_name)
                                          (M_state_if_else (conditional statement) (if* statement) (else statement) state break continue throw return parent_name instance_name)))
      ((eq? (operator statement) 'return) (M_state_return (rhs statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'while) (M_state_while (conditional statement) (loop statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'begin) (M_state_begin (block statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'break) (M_state_break state break parent_name instance_name))
      ((eq? (operator statement) 'continue) (M_state_continue state continue parent_name instance_name))
      ((eq? (operator statement) 'throw) (M_state_throw (cadr statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'try) (M_state_try (cadr statement) (caddr statement) (cadddr statement) state break continue throw return parent_name instance_name))
      ((or (eq? (operator statement) 'function) (eq? (operator statement) 'static-function)) (M_state_function_add (name statement) (parameters statement) (body statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'funcall) (M_state_function (name statement) (arguments statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'class) (M_state_class_add (name statement) (cddr statement) state break continue throw return parent_name instance_name))
      ((eq? (operator statement) 'extends) (M_state_declare 'extends (cdr statement) state break continue throw return parent_name instance_name))
      (else (error "Invalid operator:" (operator statement))))))

(define M_state_class_add
  (lambda (name code state break continue throw return parent_name instance_name)
    (if (null? (car code))
        (state_bind name (interpret_class (cadr code) state break continue throw) state)
        (state_bind name (interpret_class (cons (car code) (cadr code)) state break continue throw) state))))
  

; Declares a function in the state
; If the variable does not have an expression, the variable is takes the value ? to show it is unassigned
(define M_state_declare
  (lambda (variable expression state break continue throw return parent_name instance_name)
    (if (null? expression)
        (state_bind variable '? state)
        (state_bind variable (M_value (car expression) state break continue throw return parent_name instance_name) state))))

; Assigns one or more variables the rightmost expression
(define M_state_assign
  (lambda (variable expression state break continue throw return parent_name instance_name)
    (if (and (list? (car expression)) (eq? '= (caar expression)))
        (state_update
         variable
         (state_lookup_variable
          (next_variable expression)
          (M_state_assign (next_variable expression) (next_expression expression) state break continue throw return parent_name instance_name))
         (M_state_assign (next_variable expression) (next_expression expression) state break continue throw return parent_name instance_name))
        (if (eq? (car variable) 'dot)
            (state_update_instance (caddr variable) instance_name (M_value (car expression) state break continue throw return parent_name instance_name) state)
            (state_update variable (M_value (car expression) state  break continue throw return parent_name instance_name) state)))))

; If statements first check to see if the conditional is true
; If it is, then the 'if' statement is called
; If not, then the 'else' statement is called
; Otherwise the state is returned
(define M_state_if_else
  (lambda (condition if else state break continue throw return parent_name instance_name)
    (cond
      ((M_boolean condition state break continue throw return parent_name instance_name) (M_state if state break continue throw return parent_name instance_name))
      ((not (null? else)) (M_state else state break continue throw return parent_name instance_name))
      (else state))))

; While the conditional is true, the statement is called on a loop
; If break is called, then the loop stops evaulating where is is and exits the loop
(define M_state_while
  (lambda (condition statement state break continue throw return parent_name instance_name)
    (call/cc
     (lambda (break)
       (M_state_while_loop condition statement state break continue throw return parent_name instance_name)))))

; Helper function for while
; If continue is called, the loop jumps to the end, skippin over the rest of the code in the loop
(define M_state_while_loop
  (lambda (condition statement state break continue throw return parent_name instance_name)
    (if (M_boolean condition state break continue throw return parent_name instance_name)
        (M_state_while_loop condition statement (call/cc
                                            (lambda (continue)
                                              (M_state statement state break continue throw return parent_name instance_name)))
                            break continue throw return parent_name instance_name) state)))

; Begins a block of statements to be evaulated
(define M_state_begin
  (lambda (block state break continue throw return parent_name)
    (if (null? block)
        state
        (remove_layer (M_state_begin_block block (cons new_layer state) break continue throw return parent_name instance_name)))))

; Helper function for begin
(define M_state_begin_block
  (lambda (block state break continue throw return parent_name instance_name)
    (if (null? block)
        state 
        (M_state_begin_block (next_statements block) (M_state (current_statement block) state break continue throw return parent_name instance_name) break continue throw return parent_name instance_name))))

; Creates a try-catch-finally statement
; Finally statements are always called no matter what if they exist
; Try statements are regular statements, but if something is thrown, the try exits and goes to the catch if it exists
; The catch statements binds what is thrown to its parameter and then evaulates its statements
(define M_state_try
  (lambda (try catch finally state break continue throw return parent_name instance_name)
       (letrec ((attempt
                 (lambda (try state break continue return)
                   (call/cc
                    (lambda (throw)
                      (M_state_begin_block try (cons new_layer state) break continue throw return parent_name instance_name)))))
                (e (attempt try state break continue return parent_name instance_name)))
         (if (atom? e)
             (cond
               ((and (null? catch) (null? finally)) state)
               ((null? catch) (M_state_begin_block (cadr finally) state break continue throw return parent_name instance_name))
               (else (remove_layer (M_state_begin_block (cadr finally)
                                          (M_state_begin_block (caddr catch)
                                                               (M_state_declare (caadr catch) (list e) (cons new_layer state) break continue throw return parent_name instance_name)
                                                               break continue throw return parent_name)
                                          break continue throw return parent_name instance_name))))
             (if (null? finally)
                 (remove_layer e)
                 (remove_layer (M_state_begin_block (cadr finally) e break continue throw return parent_name instance_name)))))))

; Break continuation
(define M_state_break
  (lambda (state break parent_name instance_name)
    (break (remove_layer state))))

; Continue continuation
(define M_state_continue
  (lambda (state continue parent_name instance_name)
    (continue (remove_layer state))))

; Throw continuation
(define M_state_throw
  (lambda (expression state break continue throw return parent_name instance_name)
    (throw (M_value expression state break continue throw return parent_name instance_name))))

; Return continuation
(define M_state_return
  (lambda (expression state break continue throw return parent_name instance_name)
    (if (and (not (atom? expression)) (is_boolean? (operator expression)))
        (return (convert_value (M_boolean expression state break continue throw return parent_name instance_name)))
        (return (convert_value (M_value expression state break continue throw return parent_name instance_name))))))

; Adds a function to the state
; The variable is the function's name
; The value is the functions closure
(define M_state_function_add
  (lambda (name parameters statement state break continue throw return parent_name instance_name)
    (state_bind name (function_closure parameters statement) state)))

; Calls a function that is not being returned or assigned to a variable
; Only changes global variables
(define M_state_function
  (lambda (name arguments state break continue throw return parent_name instance_name)
    (begin (M_value_function name arguments state break continue throw return parent_name instance_name) state)))

; M_state Abstractions
(define rhs cadr)
(define lhs cddr)
(define else_exists cdddr)
(define conditional cadr)
(define if* caddr)
(define else cadddr)
(define loop caddr)
(define block cdr)
(define name cadr)
(define parameters caddr)
(define body cadddr)
(define arguments cddr)
(define next_variable cadar)
(define next_expression cddar)
(define remove_layer cdr)

; M_value *************************************************************************************************************

; Returns the value of an expression
(define M_value
  (lambda (expression state break continue throw return parent_name instance_name)
    (cond
      ((atom? expression)
       (cond
         ((number? expression) expression)
         ((state_declared? expression state) (state_lookup_variable expression state))
         ((is_boolean? expression) (M_boolean expression state break continue throw return parent_name instance_name))
         ((not (is_operator? expression)) (error "Variable not declared:" expression))))
      ((number? (operator expression)) (operator expression))
      ((state_declared? (operator expression) state) (state_lookup_variable (operator expression) state))
      ((is_boolean? (operator expression)) (M_boolean expression state break continue throw return parent_name instance_name))
      ((not (is_operator? (operator expression))) (error "Variable not declared:" (operator expression)))
      ((eq? (operator expression) 'funcall) (M_value_function (operand_1 expression) (cddr expression) state break continue throw return parent_name instance_name))
      ((eq? (operator expression) 'new) (M_value_class (cadr expression) state break continue throw return parent_name instance_name))
      ((eq? (operator expression) 'dot) (if (eq? (operand_1 expression) 'this)
                                            (M_value_class_this (operand_2 expression) state break continue throw return parent_name instance_name)
                                            (M_value_class_field (operand_1 expression) (operand_2 expression) state break continue throw return parent_name instance_name)))
      ((eq? (operator expression) '+) (round (+ (M_value (operand_1 expression) state break continue throw return parent_name instance_name)
                                                (M_value (operand_2 expression) state break continue throw return parent_name instance_name))))
      ((eq? (operator expression) '-) (if (not (null? (cddr expression)))
                                          (round (- (M_value (operand_1 expression) state break continue throw return parent_name instance_name)
                                                    (M_value (operand_2 expression) state break continue throw return parent_name instance_name)))
                                          (round (* (M_value (operand_1 expression) state break continue throw return parent_name instance_name)
                                                    -1))))
      ((eq? (operator expression) '*) (round (* (M_value (operand_1 expression) state break continue throw return parent_name instance_name)
                                                (M_value (operand_2 expression) state break continue throw return parent_name instance_name))))
      ((eq? (operator expression) '/) (round (/ (M_value (operand_1 expression) state break continue throw return parent_name instance_name)
                                                (M_value (operand_2 expression) state break continue throw return parent_name instance_name))))
      ((eq? (operator expression) '%) (round (modulo (M_value (operand_1 expression) state break continue throw return parent_name instance_name)
                                                     (M_value (operand_2 expression) state break continue throw return parent_name instance_name))))
      (else (error "Illegal operator: " (operator expression))))))

; Returns the value of a function
(define M_value_function
  (lambda (name arguments state break continue throw return parent_name instance_name)
    (cond
      ((and (null? instance_name) (list? (cadr name))) (interpret_function (cadr (state_lookup_variable (caddr name) (state_lookup_variable (cadadr name) state)))
                                                                           (function_environment (caddr name) arguments
                                                                                                 (state_lookup_variable (cadadr name) state)
                                                                                                 break continue throw return)
                                                                           break continue throw parent_name instance_name function_class))
      ((null? instance_name) (interpret_function (cadr (state_lookup_variable (caddr name) (state_lookup_variable (cadr (class_dot_type (cadr name) state)) state)))
                                                 (function_environment (caddr name) arguments
                                                                       (cons (state_get_instance_layer (cadr name) state) (state_lookup_variable (cadr (class_dot_type (cadr name) state)) state))
                                                                       break continue throw return)
                                                 break continue throw
                                                 (cadr (class_dot_type (cadr name) state))
                                                 (car (class_dot_type (cadr name) state))
                                                 (cadr (class_dot_type (cadr name) state))))
      (else (interpret_function (cadr (state_lookup_variable (caddr name) state))
                                (function_environment (caddr name) arguments state break continue throw return)
                                break continue throw parent_name instance_name parent_name))))) ; NOT RIGHT

(define M_value_class
  (lambda (type state break continue throw return parent_name instance_name)
    (if (state_declared? type state)
        (cons type (append (state_instance_fields (car (state_lookup_variable type state)))))
        (error "Class does not exist: " type))))

(define M_value_class_field
  (lambda (instance field state break continue throw return parent_name instance_name)
    (state_lookup_instance instance field state)))

(define M_value_class_this
  (lambda (field state break continue throw return parent_name instance_name)
    (state_lookup_instance instance_name field state)))

; M_value Abstractions
(define operator car)
(define operand_1 cadr)
(define operand_2 caddr)

; M_boolean ************************************************************************************************************

; Returns true or false based on a boolean operation
(define M_boolean
  (lambda (condition state break continue throw return parent_name instance_name)
    (cond
      ((eq? condition 'true) #t)
      ((eq? condition 'false) #f) 
      ((eq? (operator condition) '<=) (<= (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                          (M_value (operand_2 condition) state break continue throw return parent_name instance_name)))
      ((eq? (operator condition) '>=) (>= (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                          (M_value (operand_2 condition) state break continue throw return parent_name instance_name)))
      ((eq? (operator condition) '<) (< (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                        (M_value (operand_2 condition) state break continue throw return parent_name instance_name)))
      ((eq? (operator condition) '>) (> (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                        (M_value (operand_2 condition) state break continue throw return parent_name instance_name)))
      ((eq? (operator condition) '==) (eq? (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                           (M_value (operand_2 condition) state break continue throw return parent_name instance_name)))
      ((eq? (operator condition) '!=) (not (eq? (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                                (M_value (operand_2 condition) state break continue throw return parent_name instance_name))))
      ((eq? (operator condition) '||) (if (or (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                              (M_value (operand_2 condition) state break continue throw return parent_name instance_name))
                                     #t
                                     #f))
      ((eq? (operator condition) '&&) (if (and (M_value (operand_1 condition) state break continue throw return parent_name instance_name)
                                               (M_value (operand_2 condition) state break continue throw return parent_name instance_name))
                                     #t
                                     #f))
      ((eq? (operator condition) '!) (not (M_value (operand_1 condition) state break continue throw return parent_name instance_name)))
      (else (error "Not a boolean operation")))))

; Functions *************************************************************************************************************

; Creates the closure of a function that has a dynamic function that creates the function environment from the current environment
(define function_closure
  (lambda (parameters statement)
    (cons parameters (append (cons statement (list function_environment)) (list function_class)))))

; Creates the function environment based on the current environment
(define function_environment
  (lambda (name arguments state break continue throw return)
    (cons (function_bind_parameters_arguments name arguments state break continue throw return) (cons (car state)  (function_declare name state)))))
  

; Gets the environment with all the variables that are in the scope of the function
(define function_declare
  (lambda (name state)
    (cond
      ((state_layer_declared? name (function_name state)) state)
      (else (function_declare name (next_layer state))))))

; Gets the parameters and arguments that are needed to bind to the environment
(define function_bind_parameters_arguments
  (lambda (name arguments state break continue throw return)
    (function_bind_parameters_arguments_environment
     (parameters* (state_lookup_variable name state))
     arguments state new_layer break continue throw return)))

; Binds the parameters and arguments to the functions environment
(define function_bind_parameters_arguments_environment
  (lambda (parameters arguments state environment break continue throw return)
    (cond
      ((or (and (null? parameters) (not (null? arguments)))
           (and (null? arguments) (not (null? parameters))))
       (error "Mismatched number of parameters and arguments"))
      ((null? parameters) environment)
      (else (function_bind_parameters_arguments_environment
             (next_parameter parameters) (next_argument arguments) state
             (car (state_bind (current_parameter parameters) (M_value (current_argument arguments) state break continue throw return empty empty) (list environment)))
             break continue throw return)))))

(define function_class
  (lambda (name state break continue throw return)
    state))

; Function Abstractions
(define function_name caar)
(define parameters* car)
(define current_parameter car)
(define current_argument car)
(define next_parameter cdr)
(define next_argument cdr)

; Classes *************************************************************************************************************

(define class_dot_type
  (lambda (lefthand state)
    (append (list lefthand) (state_lookup_variable lefthand state))))

; The State *************************************************************************************************************

; Lookups a variable in the state
(define state_lookup_variable
  (lambda (var state)
    (cond
      ((null? state) (error "Variable/function does not exist in this environment:" var))
      ((state_layer_declared? var (current_layer_variables state)) (state_lookup_value var (current_layer state)))
      (else (state_lookup_variable var (next_layer state))))))


; Lookups and returns the value of a variable in the state
(define state_lookup_value
  (lambda (var state)
    (cond
      ((eq? (variable_name state) '?) (error "Variable is unassigned:" var)) 
      ((eq? (variable_name state) var) (unbox (variable_value state)))
      (else (state_lookup_value var (cons (next_variable* state) (list (next_value state))))))))

(define state_lookup_instance
  (lambda (instance var state)
    (cond
      ((null? state) (error "Instance does not exist in this environment:" instance))
      ((state_layer_declared? instance (current_layer_variables state)) (state_lookup_instance_layer instance var (current_layer state)))
      (else (state_lookup_instance instance var (next_layer state))))))

(define state_lookup_instance_layer
  (lambda (instance var state)
    (cond
      ((eq? (caar state) instance) (state_lookup_instance_value var (unbox (caadr state))))
      (else (state_lookup_instance_layer instance var (cons (cdar state) (list (cdadr state))))))))

(define state_lookup_instance_value
  (lambda (var state)
    (cond
      ((atom? (car state)) (state_lookup_instance_value var (cdr state)))
      ((eq? (caar state) var) (unbox (caadr state)))
      (else (state_lookup_instance_value var (cons (cdar state) (list (cdadr state))))))))

; Checks to see if a variable has been declared in the state
(define state_declared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((state_layer_declared? var (current_layer_variables state)))
      (else (state_declared? var (next_layer state))))))

; Checks to see if a variable has been declared in a particular layer of the state
(define state_layer_declared?
  (lambda (var layer)
    (cond
      ((null? layer) #f)
      ((eq? var (variable_name* layer)) #t)
      (else (state_layer_declared? var (next_layer layer))))))

; Binds a variable to the lowest layer of the state
(define state_bind
  (lambda (var val state)
    (cons (cons (cons var (caar state)) (list (cons (box val) (cadar state)))) (cdr state))))

; Updates a variable in the state
(define state_update
  (lambda (var val state)
    (cond
      ((null? state) (error "Variable/function does not exist in this environment:" var))
      ((state_layer_declared? var (current_layer_variables state))
       (cons (state_layer_update var val (current_layer state)) (next_layer state)))
      (else (cons (current_layer state) (state_update var val (next_layer state)))))))

; Updates a variable in a particular layer of the state
(define state_layer_update
  (lambda (var val state)
    (cond
      ((eq? (variable_name state) var)
       (cons (cons var (next_variable* state))
             (list (cons (begin (set-box! (variable_value state) val) (variable_value state)) (next_value state)))))
      (else (car (state_bind (variable_name state) (unbox (variable_value state))
                             (list (state_layer_update var val (cons (next_variable* state) (list (next_value state)))))))))))

(define state_update_instance
  (lambda (var instance val state)
    (cond
      ((null? state) (error "Instance does not exist in this environment:" instance))
      ((state_layer_declared? instance (current_layer_variables state))
       (cons (state_layer_update_instance var instance val (current_layer state)) (next_layer state)))
      (else (cons (current_layer state) (state_update_instance var instance val (next_layer state)))))))

(define state_layer_update_instance
  (lambda (var instance val state)
    (cond
      ((eq? (variable_name state) instance)
       (cons (cons instance (next_variable* state))
             (list (cons (begin (set-box! (variable_value state) (state_layer_update_instance_value var val (unbox (variable_value state)))) (variable_value state)) (next_value state)))))
      (else (car (state_bind (variable_name state) (unbox (variable_value state))
                             (list (state_layer_update_instance var instance val (cons (next_variable* state) (list (next_value state)))))))))))

(define state_layer_update_instance_value
  (lambda (var val state)
    (cond
      ((atom? (car state))
       (cons (car state) (state_layer_update_instance_value var val (cdr state))))
      ((eq? (variable_name state) var)
       (cons (cons var (next_variable* state))
             (list (cons (begin (set-box! (variable_value state) val) (variable_value state)) (next_value state)))))
      (else (car (state_bind (variable_name state) (unbox (variable_value state))
                             (list (state_layer_update_instance_value var val (cons (next_variable* state) (list (next_value state)))))))))))

(define state_instance_fields
  (lambda (state)
    (cond
      ((null? (current_layer state)) state)
      ((list? (unbox (caadr state))) (state_instance_fields (cons (next_variable* state) (list (next_value state)))))
      (else (cons (car state) (list (state_instance_fields_unbox (cadr state))))))))

(define state_instance_fields_unbox
  (lambda (values)
    (cond
      ((null? values) values)
      (else (cons (box (unbox (car values))) (state_instance_fields_unbox (cdr values)))))))

(define state_add_instance_layer
  (lambda (instance state)
    (cons (car state) (cons instance (cdr state)))))

(define state_get_instance_layer
  (lambda (instance state)
    (cond
      ((state_layer_declared? instance (current_layer_variables state)) (current_layer state))
      (else (state_get_instance_layer instance (next_layer state))))))

; State Abstractions
(define current_layer_variables caar)
(define current_layer car)
(define next_layer cdr)
(define variable_name caar)
(define variable_name* car)
(define variable_value caadr)
(define next_variable* cdar)
(define next_value cdadr)
    
; Helper Functions **********************************************************************************************

; Checks to see if the operator is a boolean operator or not
(define (is_boolean? operator)
  (cond
    ((eq? operator 'true) #t)
    ((eq? operator 'false) #t)
    ((eq? operator '<=) #t)
    ((eq? operator '>=) #t)
    ((eq? operator '<) #t)
    ((eq? operator '>) #t)
    ((eq? operator '==) #t)
    ((eq? operator '!=) #t)
    ((eq? operator '||) #t)
    ((eq? operator '&&) #t)
    ((eq? operator '!) #t)
    (else #f)))

; Checks to see if the operator is a mathmatical operator or not
(define (is_operator? operator)
  (cond
    ((eq? operator '+) #t)
    ((eq? operator '-) #t)
    ((eq? operator '*) #t)
    ((eq? operator '/) #t)
    ((eq? operator '%) #t)
    ((eq? operator 'funcall) #t)
    ((eq? operator 'new) #t)
    ((eq? operator 'dot) #t)
    (else #f)))

; Converts #t into true and #f into false
(define (convert_value value)
  (cond
    ((number? value) value)
    ((eq? #t value) 'true)
    ((eq? #f value) 'false)
    (else value)))

; Checks to see if x is an atom or not
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))