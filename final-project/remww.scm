(define remww 
    (lambda (input)
        (letrec ((helper 
                        (lambda (input)
                                (if (null? (cdr input)) 
                                input
                                (let (
                                (avilable 
                                        (fold-left (lambda(acc ins) 
                                                    (let ((after (car acc)))
                                                    (cons 
                                                          ((lambda(set1 set2) (filter (lambda(el) (not (member el set2))) set1)) 
                                                          ((lambda(set1 set2) 
                                                                        ((lambda(lst) 
                                                                            (fold-left 
                                                                                (lambda(acc x) (if (member x acc) acc (append acc (list x)) )) 
                                                                                 '() 
                                                                                  lst)) 
                                                            (append set1 set2))) after (caddr ins)) 
                                                            (cadr ins)) acc)))
                                                    '(()) (reverse input)))
                                    )
                                (filter (lambda(x)x) (map (lambda(ins al) (if ((lambda(set1 set2) (andmap (lambda(el) (member el set2)) set1) ) (caddr ins) al) #f ins)) input (cdr avilable)))))))
                )
            (let ((new-input (helper input)))
                (if (equal? input new-input)
                    input
                    (remww new-input)))
        )))
        
        
        
