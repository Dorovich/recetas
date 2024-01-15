(defparameter *recetas* (make-hash-table :test #'equal))

(defparameter *units-info* '((kg 1000 (kg hg dag g dg cg mg)) ; gramos
                             (hg 100 (kg hg dag g dg cg mg))
                             (dag 10 (kg hg dag g dg cg mg))
                             (g 1 (kg hg dag g dg cg mg))
                             (dg 0.1 (kg hg dag g dg cg mg))
                             (cg 0.01 (kg hg dag g dg cg mg))
                             (mg 0.001 (kg hg dag g dg cg mg))
                             (kl 1000 (kl hl dal l dl cl ml)) ; litros
                             (hl 100 (kl hl dal l dl cl ml))
                             (dal 10 (kl hl dal l dl cl ml))
                             (l 1 (kl hl dal l dl cl ml))
                             (dl 0.1 (kl hl dal l dl cl ml))
                             (cl 0.01 (kl hl dal l dl cl ml))
                             (ml 0.001 (kl hl dal l dl cl ml))
                             (ud 1 (ud)))) ; unidades


;;;; AQUI SE DEFINEN LAS FUNCIONES INTERNAS DEL PROGRAMA

(require :uiop)

(defun guardar-recetas (archivo)
  (with-open-file (out archivo :direction :output :if-exists :supersede)
    (format out "(progn")
    (maphash (lambda (k v)
               (format out " (setf (gethash \"~a\" *recetas*) #(" k)
               (loop for i across v
                     do (format out " #(\"~a\" ~a ~a)" (elt i 0) (elt i 1) (elt i 2)))
               (format out "))"))
             *recetas*)
    (format out ")")))

(defun cargar-recetas (archivo)
  (with-open-file (in archivo :direction :input :if-does-not-exist nil)
    (when in (eval (read in)))))

(defun nueva-receta (nombre ingredientes)
  (setf (gethash nombre *recetas*) ingredientes))

(defun borrar-receta (nombre)
  (remhash nombre *recetas*))

(defun escribir-receta (nombre ingredientes)
  (format t "~a:~%" nombre)
  (loop for i across ingredientes
        do (format t "~3t- ~a (~d ~a)~%" (elt i 0) (elt i 1) (elt i 2)))
  (fresh-line))

(defun ingr (nombre cantidad unidad)
  (declare (string nombre)
           (fixnum cantidad)
           (symbol unidad))
  (let ((nuevo (make-array 3)))
    (setf (elt nuevo 0) nombre)
    (setf (elt nuevo 1) cantidad)
    (setf (elt nuevo 2) unidad)
    nuevo))

(defun substringp (needle haystack &key (test #'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle)
          (string haystack)
          :test test))

(defun buscar-receta (&key
                        (nombre nil nombre-dado-p)
                        (ingredientes nil ingredientes-dado-p)
                        (metodo #'>=))
  (flet ((comprovar-receta (k v)
           (when (and
                  (if nombre-dado-p
                      (substringp nombre k :test #'char-equal)
                      t)
                  (if ingredientes-dado-p
                      (loop for i across ingredientes
                            for match = (find i v :test (lambda (a b)
                                                          (and
                                                           (substringp (elt a 0) (elt b 0)
                                                                       :test #'char-equal)
                                                           (member (elt a 2)
                                                                   (caddr (assoc (elt b 2) *units-info*)))
                                                           (funcall metodo
                                                                    (* (elt a 1) (cadr (assoc (elt a 2) *units-info*)))
                                                                    (* (elt b 1) (cadr (assoc (elt b 2) *units-info*)))))))
                            when (not match)
                              return nil
                            finally (return t))
                      t))
             (escribir-receta k v))))
    (maphash #'comprovar-receta *recetas*)))


;;;; AQUI SE DEFINE LA INTERFAZ PARA EL USUARIO

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun preguntar-ingr ()
  (coerce (loop while (y-or-n-p "¿Añadir ingrediente?")
                collect (let* ((nombre (prompt-read "Nombre del ingrediente:"))
                               (cantidad (uiop:split-string (prompt-read "Cantidad:")))
                               (numero (or (parse-integer (car cantidad) :junk-allowed t) 0))
                               (unidad (read-from-string (cadr cantidad))))
                          (vector nombre numero unidad)))
          'vector))

(defun preguntar-buscar ()
  (let* ((bn (y-or-n-p "¿Filtrar por nombre?"))
         (nombre (when bn (prompt-read "Nombre:")))
         (bi (y-or-n-p "¿Filtrar por ingredientes?"))
         (ingrs (when bi (preguntar-ingr))))
    (cond
      ((and bn (not bi)) (buscar-receta :nombre nombre))
      ((and (not bn) bi) (buscar-receta :ingredientes ingrs))
      ((and bn bi) (buscar-receta :nombre nombre :ingredientes ingrs))
      (t (format t "Búsqueda cancelada.~%")))))

(defun preguntar-nueva ()
  (nueva-receta (prompt-read "Nombre:") (preguntar-ingr)))

(defun preguntar-borrar ()
  (borrar-receta (prompt-read "Nombre:")))

(defun preguntar-guardar ()
  (if (y-or-n-p "¿Guardar en el archivo por defecto (recetas.data)?")
      (guardar-recetas "recetas.data")
      (guardar-recetas (prompt-read "Archivo:")))
  t)

(defun preguntar-cargar ()
  (if (y-or-n-p "¿Cargar del archivo por defecto (recetas.data)?")
      (cargar-recetas "recetas.data")
      (cargar-recetas (prompt-read "Archivo:")))
  t)

(defun bunbun ()
  (format t "~9t/\\ /|~%~9t\\ V/~%~9t| \"\")~%~9t/  |~%~8t/  \\\\\\~%~6t*(__\\_\\~%¡Gloria a Bunnyotzka!~%"))

(defun ayuda-comandos ()
  (format t "Comandos disponibles:~%")
  (loop for c in '("ping" "buscar" "listar" "nueva" "borrar" "guardar" "cargar" "unidades" "salir")
        for d in '("Comprovar que el REPL funciona correctamente."
                   "Buscar entre las recetas disponibles en la lista."
                   "Mostrar toda la lista de recetas."
                   "Añadir una nueva receta a la lista."
                   "Borrar una receta de la lista."
                   "Guardar la lista de recetas en un archivo."
                   "Cargar la lista de recetas de un archivo."
                   "Mostrar las posibles unidades de medida de los ingredientes."
                   "Salir del programa. ¡Adiós!")
        do (format t "~3t'~a': ~a~%" c d)))

(defun ayuda-unidades ()
  (format t "Al especificar una cantidad, escribe un valor entero seguido de un espacio y luego las unidades.~%Unidades disponibles:~%")
  (loop for u in '("g" "l" "ud")
        for d in '("Gramos, unidad de masa. Se admiten múltiplos comunes (kg, hg, dag, g, dg, cg, mg)."
                   "Litros, unidad de volumen. Se admiten múltiplos comunes (kl, hl, dal, l, dl, cl, ml)."
                   "Unidades. Representa una unidad entera e individual de producto.")
        do (format t "~3t'~a': ~a~%" u d)))

(defun error-comandos (cmd)
  (fresh-line)
  (format t "El comando '~a' no existe.~%Escribe 'ayuda' para ver los disponibles.~%" cmd))

(defun test-repl ()
  (let ((cmd (prompt-read ">")))
    (unless (equalp cmd "salir")
      (cond
        ((equalp cmd "ping") (format t "pong!~%"))
        ((equalp cmd "buscar") (preguntar-buscar))
        ((equalp cmd "listar") (buscar-receta)) ; sin palabras clave lo lista todo
        ((equalp cmd "nueva") (preguntar-nueva))
        ((equalp cmd "borrar") (preguntar-borrar))
        ((equalp cmd "guardar") (preguntar-guardar))
        ((equalp cmd "cargar") (preguntar-cargar))
        ((equalp cmd "ayuda") (ayuda-comandos))
        ((equalp cmd "unidades") (ayuda-unidades))
        ((equalp cmd "bunbun") (bunbun))
        (t (error-comandos cmd)))
      (test-repl))))


;;;; ESTE ES EL INICIO DEL PROGRAMA

(defun start-test-repl ()
  (format t "Si es tu primera vez usando el programa, ejecuta los comandos 'ayuda' y 'unidades'.~%")
  (test-repl))

(start-test-repl)
