#lang racket
(require "TDAFecha_20844919_GuzmanMadriaga.rkt")

; Implementación del TDAUsuario

; CONSTRUCTOR
; Descripción: Función que permite registrar a un nuevo usuario en la plataforma de documentos.
; Tipo de recursión: no tiene.
; Dominio: string x entero x string x string
; Recorrido: string
(define(createUser fecha username password)
  (list fecha username password))

; PERTENENCIA
; Descripción: función que verifica si el TDA register es correcto.
; Tipo de recursión: no tiene.
; Dominio: lista
; Recorrido: booleano
(define(isUser? user)
 (if(and (= (length user)3)
           (esFecha (car user))
           (string? (cadr user))
           (string? (caddr user))
           )
      #t
      #f))


; SELECTORES

; Descripción: Retorna la fecha.
; Tipo de recursión: no tiene.
; Dominio: entero.
; Recorrido: entero.
(define(getFechaR user)
  (if (isUser? user)
      (cadr user)
      null))

; Descripción: Retorna el nombre de usuario.
; Tipo de recursión: no tiene.
; Dominio: string.
; Recorrido: string.
(define(getUser user)
  (if (isUser? user)
      (caddr user)
      null))

; Descripción: Retorna la contraseña.
; Tipo de recursión: no tiene.
; Dominio: string.
; Recorrido: string.
(define(getPassword user)
  (if (isUser? user)
      (caddr user)
      null))

; MODIFICADORES

; Desripción: cambia la fecha.
; Tipo de recursión: no tiene.
; Dominio: entero.
; Recorrido: entero.
(define(setFechaR user fecha)
  (list fecha (getUser user)(getPassword user)))

; Desripción: cambia el nombre de usuario.
; Tipo de recursión: no tiene.
; Dominio: string.
; Recorrido: string.
(define(setUser user username)
  (list (getFechaR user) username (getPassword user)))

; Desripción: cambia la contraseña.
; Tipo de recursión: no tiene.
; Dominio: string.
; Recorrido: string.
(define(setPassword user password)
  (list (getFechaR user) (getUser user) password ))



; EJEMPLOS
(define ej1(createUser (fecha 25 10 2021) "strxngers" "xdxd"))
(define ej2(createUser (fecha 25 10 2021) "hola" "xdxd"))




(provide (all-defined-out))