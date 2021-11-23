#lang racket

; Para este TDA utilizaremos el TDA Fecha y TDA Access
(require "TDAFecha.rkt")
(require "TDAAccess.rkt")
(require "TDAVersiones.rkt")

; TDA Documento

; CONSTRUCTOR
; Descripción: crea un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: fecha tipo entero, nombre del documento tipo string y contenido del documento tipo string.
; Recorrido: lista con enteros y strings.
(define (createDoc fecha nameDoc content)
  (list fecha nameDoc content (list ) 0 (list ) (list (createVersion 0 content))))


; PERTENENCIA

; Descripción: nos retorna true si corresponde a un documento y en caso contrario retorna false.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: bolleano.
(define(isDoc doc)
 (if(and (= (length doc) 7)
           (esFecha (car doc))
           (string? (cadr doc))
           (string? (caddr doc))
           (list? (cadddr doc))
           (integer? (car (cdr (cdr (cdr (cdr doc))))))
           (list? (car (cdr(cdr (cdr (cdr (cdr doc)))))))
           (list? (car (cdr(cdr (cdr (cdr (cdr (cdr doc))))))))
           )
      #t
      #f))


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

; Descripción: nos entrega la lista de accesos.
; Tipo de recursión: 
; Dominio: 
; Recorrido: 
(define(getVersions doc)
  (if (isDoc doc)
      (car(cdr(cdr(cdr(cdr(cdr(cdr doc)))))))
      null))


; MODIFICADORES

; Descripción: cambia la fecha de creación del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: fecha tipo entero
(define(setFechaDoc doc fecha)
  (list fecha (getNameDoc doc) (getContent doc) (getOwner doc) (getID doc) (getAc doc) (getVersions doc)))

; Descripción: cambia el nombre del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: nombre del documento tipo string.
(define(setNameDoc doc nameDoc)
  (list (getFechaDoc doc) nameDoc (getContent doc) (getOwner doc) (getID doc) (getAc doc) (getVersions doc)))

; Descripción: cambia el contenido del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: contenido del documento tipo string.
(define(setContent doc content)
  (list (getFechaDoc doc)(getNameDoc doc) content (getOwner doc) (getID doc) (getAc doc) (getVersions doc)))

; Descripción: cambia el usuario que creó el documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: usuario que creó el documento tipo string.
(define(setOwner doc owner)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (list owner) (getID doc) (getAc doc) (getVersions doc)))


; Descripción: cambia la iD del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: iD(entero) y documento(lista con enteros y strings). 
; Recorrido: lista con enteros y strings.
(define(setID doc iD)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (getOwner doc) iD (getAc doc) (getVersions doc)))


; Descripción: se guardan los accesos en la lista de accesos.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings.
; Recorrido: lista con enteros y strings.
(define(setAc doc ac)
  (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (getOwner doc) (getID doc) (saveAc (getAc doc) ac) (getVersions doc)))


; FUNCIONES COMPLEMENTARIAS

; Descripción: guarda los accesos que tienen los usuarios a un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: lista y string.
; Recorrido: lista con strings.
(define(saveAc listaAc ac)
  (reverse (cons ac (reverse listaAc))))


; Descripción: revisa si un usuario puede dar accesos y de ser así lo agrega a la lista.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento, usuario y accesos tipo string.
; Recorrido: documento actualizado con los accesos permitidos.
(define(canGiveAc doc user ac)
  (if (equal? (car(getOwner doc)) user)
      (setAc doc ac)
      doc))


; Descripción: revisa si un usuario puede agregar contenido a un documento, ya que es dueño o tiene permiso para hacerlo.
; Tipo de recursión: utiliza recursión de cola.
; Dominio: documento, usuario y accesos tipo string.
; Recorrido: documento actualizado con los accesos permitidos.
(define(wAccess listaAccess user)
  (if(empty? listaAccess)
     #f
     (if (and (equal? (getUserAccess (car(listaAccess))) user) (equal? (getChar (car(listaAccess))) #\w))
         #t
         (wAccess (cdr listaAccess) user))))


; Descripción: función que crea una nueva versión del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: versiones tipo lista y contenido tipo string.
; Recorrido: documento actualizado con los accesos permitidos.
(define(addVersion versions content)
  (cons (createVersion (+ 1 (getIdVersion (car versions))) content) versions))


; Descripción: función que une el contenido existente del documento con el nuevo que se quiere agregar.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento, usuario y accesos tipo string.
; Recorrido: documento actualizado con los accesos permitidos.
(define(aContent doc content)
  (list (getFechaDoc doc) (getNameDoc doc) (string-append (string-append content " ") (getContent doc)) (getOwner doc) (getID doc) (getAc doc)
        (addVersion (getVersions doc) (string-append (string-append content " ") (getContent doc)))))


; Descripción: función que si el usuario que desea agregar contenido al documento tiene los permisos, agrega el nuevo contenido.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento, usuario y accesos tipo string.
; Recorrido: documento actualizado con los accesos permitidos.
(define(editContent doc user content)
  (if (or (equal? (car(getOwner doc)) user) (wAccess (getAc doc) user))
      (aContent doc content)
      doc))


(provide (all-defined-out))