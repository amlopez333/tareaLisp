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