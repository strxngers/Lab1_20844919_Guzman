#lang racket

; Para la creación de ParadigmaDocs se requiere usar TDA Fecha
(require "TDAFecha.rkt")

; TDAParadigmaDocs

; Se definen las funciones encryptFn y decryptFn.
; Descripción: Ambas funciones realizan lo mismo, reciben un tecto y lo modifican entregando
;              el texto al revés.
; Tipo de recursión: no utiliza recursión.
; Dominio: string.
; Recorrido: string.
(define encrypyFn (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFn (lambda (s) (list->string (reverse (string->list s)))))

; CONSTRUCTOR

; Descripción: constructor constructor que crea un paradigmaDocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: string y entero
; Recorrido: lista que contiene el nombre de la plataforma, la fecha de creación, el texto entregado
; por encryptFn y el texto entregado por decryptFn
(define(createParadigmaDocs nombre fecha encryptFn decryptFn)
  (list nombre fecha encryptFn decryptFn))

; PERTENENCIA

; Descripción: Comprueba si el formato entregado es correcto (el nombre del archivo debe ser un string).
; Tipo de recursión: no utiliza recursión.
; Dominio: documento ParadigmaDocs  
; Recorrido: booleano
(define(isParadigmaDocs? documento)
  (if (and (= (length documento)4)
           (string? (car documento))
           (esFecha (cadr documento))
           )
      #t
      #f))

; SELECTORES

; Desripción: Si corresponde a un nombre (string), retorna el nombre del paradigmadocs, en caso contrario
;             retorna null.
; Tipo de recursión: no utiliza recursión.
; Dominio: string.
; Recorrido: string.
(define(getNombre paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (car paradigmaDocs)
      null))

; Desripción: Retorna la fecha en que fue creado el paradigmaDocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: Entero.
; Recorrido: Entero.
(define(getFecha paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (cadr paradigmaDocs)
      null))

; Desripción: Retorna la función encryptFn o texto.
; Tipo de recursión: no utiliza recursión.
; Dominio: Entero.
; Recorrido: función o texto.
(define(getEncryptFn paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (caddr paradigmaDocs)
      null))

; Desripción: Retorna la función encryptFn o texto.
; Tipo de recursión: no utiliza recursión.
; Dominio: Entero.
; Recorrido: función o texto.
(define(getDecryptFn paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (cadddr paradigmaDocs)
      null))

; MODIFICADORES

; Desripción: Cambia el nombre del paradigmadocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: string.
; Recorrido: string.
(define(setNombre paradigmaDocs nombre)
  (list nombre (getFecha paradigmaDocs) (getEncryptFn paradigmaDocs)(getDecryptFn paradigmaDocs)))

; Desripción: cambia la fecha.
; Tipo de recursión: no utiliza recursión.
; Dominio: entero.
; Recorrido: entero.
(define(setFecha paradigmaDocs fecha)
  (list (getNombre paradigmaDocs) fecha (getEncryptFn paradigmaDocs)(getDecryptFn paradigmaDocs)))

