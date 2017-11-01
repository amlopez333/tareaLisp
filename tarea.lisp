(defun bpp (nodo arbol)
  (cond ((null arbol) nil)
        ((atom arbol) (cond ((eq nodo arbol) (and (write arbol) arbol))
                            (t (and (write arbol) nil))))
        ((and (write (car arbol)) (eq nodo (car arbol))) arbol)
        (t (bpp* nodo (cdr arbol)))))

(defun bpp* (nodo listaramas)
  (cond ((null listaramas) nil)
        ((bpp nodo (car listaramas)))
        (t (bpp* nodo (cdr listaramas)))))

(defun bap (objeto arbol) ; procesa la raiz; si no es, busca en la ramas (bap*)
  (cond ((null arbol) nil)
        ((and (write (car arbol)) (eq objeto (car arbol))) arbol)
        (t (bap* objeto (cdr arbol)))))

(defun bap* (objeto ramas) ; si esta en este nivel, fin (exito)
  (cond ((null ramas) nil)
        ((sucesor objeto ramas))
        (t (bap* objeto (siguiente-nivel ramas))))) ; si no, siguiente nivel

(defun sucesor (objeto ramas) ; determina si el objeto esta en este nivel
  (cond ((null ramas) nil)
        ((and (atom (car ramas)) (write (car ramas)))
          (cond ((eq objeto (car ramas)) (car ramas))
                (t (sucesor objeto (cdr ramas)))))
        ((and (atom (caar ramas)) (write (caar ramas)))
          (cond ((eq objeto (caar ramas)) (car ramas))
                (t (sucesor objeto (cdr ramas)))))
        (t (sucesor objeto (cdr ramas)))))

(defun siguiente-nivel (ramas) ; devuelve el siguiente nivel hacia abajo
  (cond ((null ramas) nil)
        ((atom (car ramas)) (siguiente-nivel (cdr ramas)))
        (t (append (cdr (car ramas)) (siguiente-nivel (cdr ramas))))))

(defun potencia (c)
   (cond ((not (conjunto c))
            (and (decir* '(error argumento no es un conjunto)) nil))
         (t (potencia* c))))

(defun potencia* (c)
  (cond ((null c) (list nil))
        (t (let ((anterior (potencia* (cdr c))))
             (append (mapcar #'(lambda (sub) (cons (car c) sub)) anterior)
                     anterior)))))

(defun cartesiano (a b)
    (cond   ((or (null a) (atom a)) nil)
            ((or (null b) (atom b)) nil)
            (t (append (pares* (car a) b) (cartesiano (cdr a) b)))
    )
)

(defun pares* (a b)
    (cond   ((null b) nil)
            (t (cons (cons a (car b)) (pares* a (cdr b))))
    )
)


(setq lista '(a b c))
;(circular lista)
(defun circular (l)
(and (setf (cdr (last l)) l) l))

(defun avanza (c lista)
	(cond ((eq c (car lista)) lista)
		(t (avanza c (cdr lista)))
	)
)


(defun encriptar (H Ae As)
	(cond ((null H) nil)
			(t (encriptar* H Ae As nil nil))
	)
)

(defun encriptar* (H Ae As Ue Us)
	(cond ((null H) (cons (cons Ue Us) nil))
			(t (cons (nesimo(girar (car H) Ae As 1) As) (encriptar*(cdr H) (girarNVeces Ae (girar (car H) Ae As 1)) (girarNVeces As (girar (car H) Ae As 1)) (car (girarNVeces Ae (girar (car H) Ae As 0))) (car (girarNVeces As (girar (car H) Ae As 0))))))
    )
)

(defun girar (C Ae As Cont)
    (cond ((null C) nil)       
            ((equal C (car Ae)) Cont)
            (t (girar C (cdr Ae)(cdr As) (+ cont 1)))
    )
)

(defun girarNVeces (lista n)
	(cond ((eq n 0) lista)
		(t (girarNVeces (cdr lista)(- n 1)))
	)
)

(defun encripta(H Ae As)
	(encriptar H (circular Ae) (circular (reversa As)))
)

(defun desencripta (H Ae As Ef)
	(desencriptar H (avanza (cadr Ef) (circular As)) (avanza (car Ef) (circular (reversa Ae))))
)

(defun desencriptar (H Ae As)
	(cond ((null H) nil)
			(t (cons (nesimo(girar (car H) Ae As 1) As) (encriptar*(cdr H) (girarNVeces Ae (girar (car H) Ae As 1)) (girarNVeces As (girar (car H) Ae As 1)) (car (girarNVeces Ae (girar (car H) Ae As 1))) (car (girarNVeces As (girar (car H) Ae As 1))))))
    )
)

(defun reversa (l)
   (cond ((null l) nil)
         (t (append (reversa (cdr l))
                    (cons (car l) nil)))))

(defun nesimo (n l)
   (cond ((eq n 1) (car l))
         (t (nesimo (- n 1) (cdr l)))))

