; M_state_var: implemented for (var ...) calls; (M_state_var '(var name) state) | (M_state_var '(var name <epxression>) state) -> state

(define M_state_var
  (lambda (declare s)
      (add_to_state s (var_name declare) (M_value (declare_experssion declare)))))

; M_value_var: implemented for (var ...) calls; (M_value_var '(var name)) | (M_value_var '(var name <epxression>)) -> value

(define M_value_var
  (lambda (declare)
    (M_value (declare_expression declare))))

; declare_experssion: returns the expression of a declare if one exists from the format '(var name <experssion>)
;    helper for M_var
(define declare_experssion caddr)
       
; var_name: returns the name of a var from a declare statement        
;    helper for M_var        
(define var_name cadr)

; M_state_assign: implemented for (= ...) calls; (M_state_assign '(= name <expression>) state) -> state

; M_value_assign: implemented for (= ...) calls; (M_value_assign '(= name <expression>) state) -> value

; M_boolean_assign: implemented for (= ...) calls; (M_boolean_assign '(= name <expression>) state) -> bvalue (or error if expression is <numeric>)

; M_value_math: implemented for ({+,-,*,/,%} ...) calls; (M_value_math '(<math_op> <numeric> <numeric>) state) -> nvalue

; M_boolean_logic: implemented for ({&&, ||} ...) calls; (M_value_boolean '(<bool_op> <condition> <condition>) state) -> bvalue

; M_boolean_comparison: implemented for ({<,>,<=,>=,==,!=} ...) calls; (M_boolean_comparison '(<comp_op> <numeric> <numeric>) state) -> bvalue

; M_value_return: implemented for (return ...); note: return does not need an M_state, as the state doesn't matter after return; (M_value_return '(return <expression>) state) -> value

; M_state_if: implemented for (if ...); (M_state_if '(if <condition> <expression>) state) | (M_state_if '(<condition> <expression> <expression>) state) -> state

; M_state_while: implemented for (while ...); (M_state_while '(while <condition> <expression>) state) -> state

; M_state_expression: dispatches M_state to the proper M_state_(expression)

; M_value_expression: dispatches M_value to the proper M_value_(expresion) (error if value is not defined for that expression)

; M_boolean_expressin: dispatches M_boolean to the proper M_boolean_(expression) (error if boolean is not defined for that expression)

; The state: '((var1, var2, ...) (value1, value2, ...))

(define get_empty_state
  (lambda ()
    '(() ())))

(define get_first_var caar)
(define get_first_value caadr)
(define get_vars car)
(define get_values cadr)

; makes a state from a vars list and a values list
(define construct_state
  (lambda (vars values)
    (cons vars (cons values '()))))

; checks if the vars or the values are null
(define null_state?
  (lambda (state)
    (or (null? (get_vars state)) (null? (get_values state)))))

; essentially "cdrstate"
(define remove_first_var
  (lambda (state)
    (if (null_state? state)
        (get_empty_state)
        (construct_state (cdr (get_vars state)) (cdr (get_values state))))))

; (add_to_state state var value) -> state; adds a var with value to the state
(define add_to_state
  (lambda (state var value)
    (construct_state (cons var (get_vars state)) (cons value (get_values state)))))

; (remove_from_state state var) -> state; removes a var from the state, does nothing if var is not in the state
(define remove_from_state
  (lambda (state var)
    (cond
      ((null_state? state) '(() ()))
      ((eq? (get_first_var state) var) (construct_state (cdr (get_vars state)) (cdr (get_values state))))
      (else (add_to_state (remove_from_state (remove_first_var state) var) (get_first_var state) (get_first_value state))))))

; (replace_in_state state var value) -> state; replaces var with value in state
(define replace_in_state
  (lambda (state var value)
    (add_to_state (remove_from_state state var) var value)))

; (get_from_state var) -> value; gets the value of var from the state
(define get_from_state
  (lambda (state var)
    (cond
      ((null_state? state) '())
      ((eq? (get_first_var state) var) (get_first_value state))
      (else (get_from_state (remove_first_var state) var)))))