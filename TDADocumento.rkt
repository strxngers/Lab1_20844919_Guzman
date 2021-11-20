#lang racket

; Para este TDA utilizaremos el TDA Fecha y TDA Access
(require "TDAFecha.rkt")
(require "TDAAccess.rkt")

; TDA Documento

; CONSTRUCTOR
; Descripción: crea un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: fecha tipo entero, nombre del documento tipo string y contenido del documento tipo string.
; Recorrido: lista con enteros y strings.
(define (createDoc fecha nameDoc content)
  (list fecha nameDoc content (list ) 0 (list )))

; PERTENENCIA
; Descripción: nos retorna true si corresponde a un documento y en caso contrario retorna false.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: bolleano.
(define(isDoc doc)
 (if(and (= (length doc) 6)
           (esFecha (car doc))
           (string? (cadr doc))
           (string? (caddr doc))
           (list? (cadddr doc))
           (integer? (car (cdr (cdr (cdr (cdr doc))))))
           (list? (car (cdr(cdr (cdr (cdr (cdr doc)))))))
           )
      #t
      #f))


; al agregar la ID a TDAdocumento, agregar un elemento al final de createDoc (por ejemplo int 0) y
; modificar a la funcion isDoc para el lenght doc = 5 y que el ultimo elemento sea un integer

; Pequeño ejemplo para comprobar si funciona.
(define a(createDoc (fecha 03 11 2021) "a1" "este es mi primer documento"))

; SELECTORES

; Descripción: nos entrega la fecha de creación del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: fecha tipo entero.
(define(getFechaDoc doc)
  (if (isDoc doc)
      (car doc)
      null))

; Descripción: nos entrega el nombre del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: nombre del documento tipo string.
(define(getNameDoc doc)
  (if (isDoc doc)
      (cadr doc)
      null))

; Descripción: nos entrega el contenido del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(getContent doc)
  (if (isDoc doc)
      (caddr doc)
      null))

; Descripción: nos entrega el usuario que creó el documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: usuario que creó el documento tipo string.
(define(getOwner doc)
  (if (isDoc doc)
      (cadddr doc)
      null))

; Descripción: nos entrega la ID del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: Id tipo entero.
(define(getID doc)
  (if (isDoc doc)
      (car(cdr(cdr(cdr(cdr doc)))))
      null))

; Descripción: nos entrega la lista de accesos.
; Tipo de recursión: 
; Dominio: 
; Recorrido: 
(define(getAc doc)
  (if (isDoc doc)
      (car(cdr(cdr(cdr(cdr(cdr doc))))))
      null))

; MODIFICADORES

; Descripción: cambia la fecha de creación del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: fecha tipo entero
(define(setFechaDoc doc fecha)
  (list fecha (getNameDoc doc) (getContent doc) (getOwner doc) (getID doc) (getAc doc)))

; Descripción: cambia el nombre del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: nombre del documento tipo string.
(define(setNameDoc doc nameDoc)
  (list (getFechaDoc doc) nameDoc (getContent doc) (getOwner doc) (getID doc) (getAc doc)))

; Descripción: cambia el contenido del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(setContent doc content)
  (list (getFechaDoc doc)(getNameDoc doc) content (getOwner doc) (getID doc) (getAc doc)))

; Descripción: cambia el usuario que creó el documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: usuario que creó el documento tipo string.
(define(setOwner doc owner)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (list owner) (getID doc) (getAc doc)))


; Descripción: 
; Tipo de recursión: 
; Dominio: 
; Recorrido: 
(define(setID doc iD)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (getOwner doc) iD (getAc doc)))

; Descripción: se guardan los accesos en la lista de accesos.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: lista con enteros y strings.
(define(setAc doc ac)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (getOwner doc) (getID doc) (saveAc (getAc doc) ac)))


; FUNCIONES COMPLEMENTARIAS

; Descripción: guarda los accesos que tienen los usuarios a un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista y string.
; Recorrido: lista con strings.
(define(saveAc listaAc ac)
  (reverse (cons ac (reverse listaAc))))

; Descripción: revisa si un usuario puede dar accesos.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento, usuario y accesos tipo string.
; Recorrido: documento actualizado con los accesos permitidos.
(define(canGiveAc doc user ac)
  (if (equal? (getOwner doc) user)
      (setAc doc ac)
      doc))


(provide (all-defined-out))