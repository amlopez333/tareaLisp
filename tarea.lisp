;; Andres Lopez Urena -- b43885
;; Olman Cheng Lam --
;; Tarea 2 -- Lisp
;;------------------------------------------------------------

;; el subarbol de arbol cuya raiz es n, NIL si no existe; la
;; funcion despliega los nodos recorridos en profundidad
;; primero
(defun bpp (nodo arbol)
  (cond ((null arbol) nil)
        ((atom arbol) (cond ((eq nodo arbol) (and (write arbol) arbol))
                            (t (and (write arbol) nil))))
        ((and (write (car arbol)) (eq nodo (car arbol))) arbol)
        (t (bpp* nodo (cdr arbol)))))

;; funcion auxiliar para recorrer la lista de ramas del nodo
(defun bpp* (nodo listaramas)
  (cond ((null listaramas) nil)
        ((bpp nodo (car listaramas)))
        (t (bpp* nodo (cdr listaramas)))))


;; el subarbol de arbol cuya raiz es n, NIL si no existe; la
;; funcion despliega los nodos recorridos en ancho
;; primero
(defun bap (nodo arbol) 
  (cond ((null arbol) nil)
        ((and (write (car arbol)) (eq nodo (car arbol))) arbol)
        (t (bap* nodo (cdr arbol)))))

;; funcion auxiliar para recorrer los niveles del arbol
;; recorrre los niveles y subniveles de las ramas del nodo
(defun bap* (nodo ramas) 
  (cond ((null ramas) nil)
        ((nivelActual nodo ramas))
        (t (bap* nodo (siguientenivel ramas))))) 

;; recorre el nivel actual del nodo que contiene dichas ramas
;; para determinar si esta es ese nivel
(defun nivelActual (nodo ramas) 
  (cond ((null ramas) nil)
        ((and (atom (car ramas)) (write (car ramas)))
          (cond ((eq nodo (car ramas)) (car ramas))
                (t (nivelActual nodo (cdr ramas)))))
        ((and (atom (caar ramas)) (write (caar ramas)))
          (cond ((eq nodo (caar ramas)) (car ramas))
                (t (nivelActual nodo (cdr ramas)))))
        (t (nivelActual nodo (cdr ramas)))))

;; funcion que retorna el siguiente nivel de las ramas
(defun siguienteNivel (ramas) 
  (cond ((null ramas) nil)
        ((atom (car ramas)) (siguienteNivel (cdr ramas)))
        (t (append (cdr (car ramas)) (siguieNtenivel (cdr ramas))))))

;; funcion que calcula el conjunto potencia de c
(defun potencia (c)
   (cond ((or(null a)(atom a)) nil)
            ((or(null b)(atom b)) nil)
         (t (potencia* c))))

;; funcion auxiliar que calcula el conjunto potencia de C
(defun potencia* (c)
  (cond ((null c) (list nil))
        (t (let ((anterior (potencia* (cdr c))))
             (append (mapcar #'(lambda (sub) (cons (car c) sub)) anterior)
                     anterior)))))

(defun cartesiano (a b)
    (cond ((or(null a)(atom a)) nil)
            ((or(null b)(atom b)) nil)
            (t(cons (pares* (car a) b) (pares* a (crd b))))
    )
)

(defun pares* (a b)
    (cond ((null b) nil)
        (t (cons (cons a (car b))(pares* a (cdr b))))
    )
)

(defun appnd (1st tail)
	(cond ((null 1st) tail)
			(t (cons (car 1st) (appnd (cdr 1st) tail)))
	)
)


;; funcion que vuelve la lista l en una circular
(defun circular (l)
        (and (setf (cdr (last l)) l) l))

;; funcion que avanza lista hasta que (car lista) = c       
(defun avanza (c lista)
	(cond ((eq c (car lista)) lista)
		(t (avanza c (cdr lista)))
	)
)



;; funcion que calcula el indice del objeto C en lista Ae
;; Retorna el indice en Cont
;; Mejoras -> Eliminar As de la funcion ya que no tiene proposito alguno
(defun indexOf (C Ae As Cont)
    (cond ((null C) nil)       
            ((equal C (car Ae)) Cont)
            (t (indexOf C (cdr Ae)(cdr As) (+ Cont 1)))
    )
)
;; funcion que rota lista N veces
(defun girarNVeces (lista n)
	(cond ((eq n 1) lista)
		(t (girarNVeces (cdr lista)(- n 1)))
	)
)

;; funcion que encripta la hilera H con los alfabetos de entrada y salida Ae y As respectivamente.
;; Retorna una lista de la forma (h . e) donde h es la hilera encriptada y e es el estado final
;; formado por la cabeza de Ae y As cuando finaliza la encripcion
;; -----Ejemplos: (encripta '(r o t) '(a r o u l t) '(1 3 5 7 ))
;;               ---> (5 3 5 (T . 5))
;;                   
(defun encripta(H Ae As)
	(encriptar H (circular Ae) (circular (reversa As)))
)

;; funcion auxiliar que encripta la hilera H
(defun encriptar (H Ae As)
	(cond ((null H) nil)
		(t (encriptar* H Ae As nil nil))
	)
)
;; function auxiliar que crea la lista de la forma (h . e)
;; Ue y Us corresponden al ultimo valor de la cabeza de Ae y As respectivamente
(defun encriptar* (H Ae As Ue Us)
	(cond ((null H) (cons (cons Ue Us) nil))
		(t (cons (nesimo(indexOf (car H) Ae As 1) As) 
                        (encriptar*(cdr H) 
                                (girarNVeces Ae (indexOf (car H) Ae As 1)) 
                                (girarNVeces As (indexOf (car H) Ae As 1)) 
                                (car (girarNVeces Ae (indexOf (car H) Ae As 1))) 
                                (car (girarNVeces As (indexOf (car H) Ae As 1)))
                        )
                ))
        )
)

;; funcion que decripta la hilera H con los alfabetos de entrada y salida Ae y As respectivamente.
;; Retorna una lista de la forma (h ) donde h es la hilera decriptada 
;; -----Ejemplos: (desencripta '(5 3 5) '(a r o u l t) '(1 3 5 7) '(t 5))
;;               ---> (R O T)
;; Note que Ae y As corresponden a los valores utilizados a la hora de encriptar, respectivamente.
;; Es decir que si Ae para encriptar fue '(a r o u l t) entonces Ae para decriptar es '(a r o u l t). 
;; Lo mismo para As. 
(defun decripta (H Ae As Ef)
	(reversa(desencriptar (reversa H) (avanza (cadr Ef) (circular As)) (avanza (car Ef) (circular (reversa Ae)))))
)

;; funcion auxiliar que crea la lista de la forma (h) con la hilera decriptada
(defun desencriptar (H Ae As)
	(cond ((null H) nil)
			(t (cons (nesimo(indexOf (car H) Ae As 1) As) 
                                (desencriptar(cdr H) 
                                        (girarNVeces Ae (indexOf (car H) Ae As 1)) 
                                        (girarNVeces As (indexOf (car H) Ae As 1)) 
                                )
                        ))
        )
)

;; funcion que retorna el resultado de invertir los elementos de l
(defun reversa (l)
   (cond ((null l) nil)
         (t (append (reversa (cdr l))
                    (cons (car l) nil)))))

;; funcion que retorna el n-esimo elemento de la lista l (objeto)
(defun nesimo (n l)
   (cond ((eq n 1) (car l))
         (t (nesimo (- n 1) (cdr l)))))

