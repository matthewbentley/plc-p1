; Group Memebers
; - Matthew Bentley
; - David Lance
; - Alex Tryjankowski

(load "simpleParser.scm")

(define interpret
  (lambda (name)
    (call/cc
     (lambda (return)
       (evaluate (parser name) (get_empty_state) (lambda (v) v) (lambda (v) (error "Break outside of loop")) (lambda (v) (error "continue outside of loop")))))))

(define evaluate
  (lambda (program state brace break continue)
    (cond
      ((null? state) '())
      ((eq? (get_return_check state) 'return) (display_val (M_value (get_return_check state) state break continue)))
      ((null? program) (brace (cdr state)))
      (else (evaluate (rest_lines program) (M_state (first_line program) state break continue) brace break continue)))))

; get_return_check: gets the beginning of the state, which is used to check if the state is in return format
(define get_return_check car)
; first_line: gets the first line of the program from the parsed out list
(define first_line car)
; rest_line: gets the lines after the first of the program from the parsed out list
(define rest_lines cdr)

; M_state:
(define M_state
  (lambda (expression s break continue)
    (cond
      ((null? expression) (error 'null "You cannot evaluate null"))
      ((number? expression) s) ; No change in state from a number
      ((bool? expression) s) ; No change in state from a bool
      ((not (list? expression)) s) ; No change in state from accessing a variable
      (else ((state_dispatch (get_op expression)) expression s break continue)))))

; state_dispatch: returns the proper state function given the keyword from M_state
(define state_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'break) M_state_break)
      ((eq? keyword 'continue) M_state_continue)
      ((eq? keyword 'begin) M_state_begin)
      ((eq? keyword 'var) M_state_var)
      ((eq? keyword '=) M_state_assign)
      ((eq? keyword 'return) return)
      ((eq? keyword 'if) M_state_if)
      ((eq? keyword 'while) M_state_while)
      ((or (eq? keyword '+) (eq? keyword '-) (eq? keyword '*) (eq? keyword '/) (eq? keyword '%)
           (eq? keyword '<) (eq? keyword '>) (eq? keyword '<=) (eq? keyword '>=) (eq? keyword '==) (eq? keyword '!=)
           (eq? keyword '||) (eq? keyword '&&) (eq? keyword '!)) M_state_exp)
      (else (error 'keyword "Unknown or unimplemented keyword")))))

; M_value:
(define M_value
  (lambda (expression s break continue)
    (cond
      ((null? expression) (error 'null "You cannot get the value of null"))
      ((eq? (car s) 'return) (cadr s))
      ((number? expression) expression)
      ((bool? expression) expression)
      ((eq? expression 'true) #t)
      ((eq? expression 'false) #f)
      ((not (list? expression)) (get_from_state s expression))
      (else ((value_dispatch (get_op expression)) expression s break continue)))))

; value_dispatch: returns the proper value function given the keyword from M_value
(define value_dispatch
  (lambda (keyword)
    (cond
      ((eq? keyword 'var) M_value_var)
      ((eq? keyword '=) M_value_assign)
      ((eq? keyword 'return) M_value_return)
      ((eq? keyword 'if) (error 'no_value "If cannot be used as a value"))
      ((eq? keyword 'while) (error 'no_value "While cannot be used as a value"))
      ((or (eq? keyword '+) (eq? keyword '-) (eq? keyword '*) (eq? keyword '/) (eq? keyword '%)
           (eq? keyword '<) (eq? keyword '>) (eq? keyword '<=) (eq? keyword '>=) (eq? keyword '==) (eq? keyword '!=)
           (eq? keyword '||) (eq? keyword '&&) (eq? keyword '!)) M_value_exp)
      (else (error keyword "Unknown or unimplemented keyword")))))

; M_state_begin: implemented for (begin ...) calls; (M_state_begin '(begin <expression>) state) -> state
(define M_state_begin
  (lambda (expression s break continue)
    (call/cc
     (lambda (brace)
       (evaluate (cdr expression) (construct_state (get_empty_scope) s) brace break continue)))))

; M_state_var: implemented for (var ...) calls; (M_state_var '(var name) state) | (M_state_var '(var name <epxression>) state) -> state
(define M_state_var
  (lambda (declare s break continue)
    (cond
      ((null? (cddr declare)) (add_to_state s (get_operand1 declare) '()))
      ((in_state? (get_operand1 declare) s) (error 'declare "Cannot declare a var twice"))
      (else (add_to_state (M_state (get_operand2 declare) s break continue) (get_operand1 declare) (M_value (get_operand2 declare) s break continue))))))

; M_value_var: implemented for (var ...) calls; (M_value_var '(var name)) | (M_value_var '(var name <epxression>)) -> value
(define M_value_var
  (lambda (declare s)
    (if (null? (cddr declare))
        '()
        (M_value (get_operand2 declare) s))))

; M_state_assign: implemented for (= ...) calls; (M_state_assign '(= name <expression>) state) -> state
(define M_state_assign
  (lambda (assign s break continue)
    (if (not (in_state? (get_operand1 assign) s))
        (error 'var "Undeclared var")
        (replace_in_state (M_state (get_operand2 assign) s break continue) (get_operand1 assign) (M_value (get_operand2 assign) s break continue)))))

; M_value_assign: implemented for (= ...) calls; (M_value_assign '(= name <expression>) state) -> value
(define M_value_assign
  (lambda (assign s)
    (M_value (get_operand2 assign) s)))

; M_value_exp: 
(define M_value_exp
  (lambda (expression s break continue)
      (cond
        ((and (eq? (get_op expression) '-) (null? (cddr expression))) (* -1 (M_value (get_operand1 expression) s)))
        ((eq? (get_op expression) '!) (error_not (M_value (get_operand1 expression) s break continue)))
        (else ((get_exp_op (get_op expression)) (M_value (get_operand1 expression) s break continue) (M_value (get_operand2 expression) (M_state (get_operand1 expression) s break continue) break continue))))))

; get_exp_op: returns the functions for any experssion
(define get_exp_op
  (lambda (o)
    (cond
      ((eq? '+ o) +)
      ((eq? '- o) -)
      ((eq? '* o) *)
      ((eq? '/ o) quotient)
      ((eq? '% o) remainder)
      ((eq? '< o) <)
      ((eq? '<= o) <=)
      ((eq? '> o) >)
      ((eq? '>= o) >=)
      ((eq? '== o) eq?)
      ((eq? '!= o) not_equal)
      ((eq? '&& o) error_and)
      ((eq? '|| o) error_or)
      ((eq? '! o) error_not)
      (else o))))

(define not_equal
  (lambda (a b)
    (not (eq? a b))))

; M_state_exp: the expression itself doesn't change the state, but the values operated on might
(define M_state_exp
  (lambda (expression s break continue)
    (if (or (and (eq? (get_op expression) '-) (null? (cddr expression))) (eq? (get_op expression) '!))
        (M_state (get_operand1 expression) s break continue)
        (M_state (get_operand2 expression) (M_state (get_operand1 expression) s break continue) break continue))))

; error_and: basic error catching and function in case of nonbooleans

(define error_and
  (lambda (e1 e2)
    (if (and (bool? e1) (bool? e2))
        (and e1 e2)
        (error 'type "Operand 1 and 2 for and were not booleans"))))

; error_or: basic error catching or function in case of nonbooleans

(define error_or
  (lambda (e1 e2)
    (if (and (bool? e1) (bool? e2))
        (or e1 e2)
        (error 'type "Operand 1 and 2 for or were not booleans"))))

; error_not: basic error catching not function in case of nonbooleans
(define error_not
  (lambda (e)
    (if (bool? e)
        (not e)
        (error 'type "Not must take a bool"))))

; M_value_return: implemented for (return ...); note: return does not need an M_state, as the state doesn't matter after return; (M_value_return '(return <expression>) state) -> value

(define M_value_return
  (lambda (expression s)
    ((return (M_value (get_operand1 expression) s)))))

; M_state_if: implemented for (if ...); (M_state_if '(if <condition> <expression>) state) | (M_state_if '(<condition> <expression> <expression>) state) -> state

(define M_state_if
  (lambda (expression s break continue)
    (cond
      ((M_value (get_operand1 expression) s break continue)
       (M_state (get_operand2 expression)
                (M_state (get_operand1 expression) s break continue) break continue))
      ((not (null? (cdddr expression)))
       (M_state (get_operand3 expression)
                (M_state (get_operand1 expression) s break continue) break continue))
      (else s))))

; M_state_while: implemented for (while ...); (M_state_while '(while <condition> <expression>) state) -> state
(define M_state_while_helper
  (lambda (expression s break continue)
    ;(call/cc
     ;(lambda (_break)
       (if (M_value (get_operand1 expression) s break continue)
           (M_state_while_helper expression
                          (call/cc
                           (lambda (_continue)
                             (M_state_while_helper expression (M_state (get_operand2 expression) (M_state (get_operand1 expression) s break _continue) break _continue) break _continue)))
                          break continue)
           (M_state (get_operand1 expression) s break continue))))

(define M_state_while
  (lambda (expression s break continue)
    (call/cc
     (lambda (_break)
       (M_state_while_helper expression s _break continue)))))

(define M_state_continue
  (lambda (expression s break continue)
    (continue (remove_narrow_scope s))))

(define M_state_break
  (lambda (expression s break continue)
    (break (remove_narrow_scope s))))

(define display_val
  (lambda (a)
    (cond
      ((eq? a #t) 'true)
      ((eq? a #f) 'false)
      (else a))))

(define return
  (lambda (expression s break continue)
    (cons 'return (cons (M_value (get_operand1 expression) s break continue) '()))))

(define bool?
  (lambda (b)
    (or (eq? b #t) (eq? b #f))))

(define get_op car)
(define get_operand1 cadr)
(define get_operand2 caddr)
(define get_operand3 cadddr)


; ------------------------ STATE STUFF ------------------------
; - A scope is the current set of {} that the program is in.  -
; - A state is a the cons of all scopes going from narrowest  -
; -   to widest with each car.                                -
; -------------------------------------------------------------

; The state: '(((var3, var4, ...) (value3, value5, ...))
;              ((var1, var2, ...) (value1, value2, ...))...)'
(define get_empty_state
  (lambda ()
    (cons (get_empty_scope) '())))

; The scope: '((var1, var2, ...) (value1, value2, ...))'
(define get_empty_scope
  (lambda ()
    '(() ())))

; remove_narrow_scope: gets all scopes minus most narrow
(define remove_narrow_scope cdr)
(define get_current_scope car)
(define get_first_var caaar)
(define get_first_value caadar)
(define get_vars caar)
(define get_values cadar)
(define rest_vars cdr)
(define rest_values cdr)
(define get_scope_vars car)
(define get_scope_vals cadr)

(define eq_scope?
  (lambda (scope1 scope2)
    (and (eq_list? (get_scope_vars scope1) (get_scope_vars scope2))
         (eq_list? (get_scope_vals scope1) (get_scope_vals scope2)))))

(define eq_list?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1) (null? ls2)) #t)
      ((null? ls1) #f)
      ((null? ls2) #f)
      ((eq? (car ls1) (car ls2)) (eq_list? (cdr ls1) (cdr ls2)))
      (else #f))))

; construct_scope: makes a scope from a vars list and a values list
(define construct_scope
  (lambda (vars values)
    (cons vars (cons values '()))))

; construct_state: makes a state from a new scope and the state with the wider scopes
(define construct_state
  (lambda (scope state)
    (cons scope state)))

; null_current_scope?: checks if the vars or the values are null of current scope
(define null_current_scope?
  (lambda (state)
    (cond
      ((null? state) #t)
      (else (or (null? (get_vars state)) (null? (get_values state)))))))

; null_state?: checks if all the scopes are null
(define null_state?
  (lambda (state)
    (cond
      ((null? state) #t)
      (else (and (null_current_scope? state) (null_state? (remove_narrow_scope state)))))))

; remove_first_var: essentially "cdrscope"
(define remove_first_var
  (lambda (state)
    (if (null_current_scope? state)
        (construct_state (get_empty_scope) (remove_narrow_scope state))
        (construct_state
         (construct_scope (rest_vars (get_vars state)) (rest_values (get_values state)))
         (remove_narrow_scope state)))))

; add_to_state: adds a var with value to the narrowest scope in the state
(define add_to_state
  (lambda (state var value)
    (construct_state (construct_scope (cons var (get_vars state)) (cons value (get_values state)))
                     (remove_narrow_scope state))))

; remove_from_state: removes a var from the state starting with the narrrowest scope, does nothing if var is not in the state
(define remove_from_state
  (lambda (state var)
    (cond
      ((null? state) '())
      ((null_state? state) (get_empty_state))
      ((eq_scope? (get_current_scope state)
                  (get_current_scope (remove_from_scope state var))) (construct_state (get_current_scope state)
                                                                           (remove_from_state (remove_narrow_scope state) var)))
      (else (remove_from_scope state var)))))

; remove_from_scope: removes a var from a given scope if found
(define remove_from_scope
  (lambda (state var)
    (cond
      ((null_current_scope? state) (construct_state (get_empty_scope) (remove_narrow_scope state)))
      ((eq? (get_first_var state) var) (construct_state (construct_scope (rest_vars (get_vars state))
                                                                         (rest_values (get_values state)))
                                                        (remove_narrow_scope state)))
      (else (add_to_state (remove_from_scope (remove_first_var state) var)
                          (get_first_var state) (get_first_value state))))))
      

; (replace_in_state state var value) -> state; replaces var with value in state
(define replace_in_state
  (lambda (state var value)
    (cond
      ((null? state) '())
      ((null_state? state) (get_empty_state))
      ((eq_scope? (get_current_scope state)
                  (get_current_scope (remove_from_scope state var))) (construct_state (get_current_scope state)
                                                                           (replace_in_state (remove_narrow_scope state) var value)))
      (else (add_to_state (remove_from_scope state var) var value)))))

; (get_from_state state var) -> value; gets the value of var from the state
(define get_from_state
  (lambda (state var)
    (cond
      ((null_state? state) (error 'var "Undeclared var"))
      ((null_current_scope? state) (get_from_state (remove_narrow_scope state) var))
      ((and (eq? (get_first_var state) var) (null? (get_first_value state))) (error 'var "Unassigned variable"))
      ((eq? (get_first_var state) var) (get_first_value state))
      (else (get_from_state (remove_first_var state) var)))))

; (in_state? var s) -> bool; checks if var is a declared variable in s
(define in_state?
  (lambda (var s)
    (cond
      ((null_state? s) #f)
      ((null_current_scope? s) (in_state? var (remove_narrow_scope  s)))
      ((eq? (get_first_var s) var) #t)
      (else (in_state? var (remove_first_var s))))))