#lang racket

; Para la creación de ParadigmaDocs se requiere usar TDA Fecha
(require "TDAFecha.rkt")
(require "TDAUser.rkt")

; TDAParadigmaDocs

; Se definen las funciones encryptFn y decryptFn.
; Descripción: Ambas funciones realizan lo mismo, reciben un tecto y lo modifican entregando
;              el texto al revés.
; Tipo de recursión: no utiliza recursión.
; Dominio: string.
; Recorrido: string.
(define encrypyFunction (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFunction (lambda (s) (list->string (reverse (string->list s)))))

; CONSTRUCTOR

; Descripción: constructor constructor que crea un paradigmaDocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: string y entero
; Recorrido: lista que contiene el nombre de la plataforma, la fecha de creación, el texto entregado
; por encryptFn y el texto entregado por decryptFn
(define(createParadigmaDocs nombre fecha encryptFunction decryptFunction)
  (list nombre fecha encryptFunction decryptFunction (list ) (list )))

; PERTENENCIA

; Descripción: Comprueba si el formato entregado es correcto (el nombre del archivo debe ser un string).
; Tipo de recursión: no utiliza recursión.
; Dominio: documento ParadigmaDocs  
; Recorrido: booleano
(define(isParadigmaDocs? documento)
  (if (and (= (length documento))
           (string? (car documento))
           (esFecha (cadr documento))
           (list? (car(cddddr documento)))
           (list? (car (cdr (cdr (cdr (cdr documento))))))
           (list? (car (cdr (cdr (cdr (cdr (cdr documento)))))))
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

; Desripción: Retorna la función encryptFunction o texto.
; Tipo de recursión: no utiliza recursión.
; Dominio: Entero.
; Recorrido: función o texto.
(define(getEncryptFunction paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (caddr paradigmaDocs)
      null))

; Desripción: Retorna la función encryptFunction o texto.
; Tipo de recursión: no utiliza recursión.
; Dominio: Entero.
; Recorrido: función o texto.
(define(getDecryptFunction paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (cadddr paradigmaDocs)
      null))

; Desripción: Retorna la lista de usuarios registrados.
; Tipo de recursión: no utiliza recursión.
; Dominio: Usuarios (string).
; Recorrido: Lista de strings o vacía.
(define(getListaUsers paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (car(cddddr paradigmaDocs))
      null))

; Desripción: Retorna la lista de usuarios activos
; Tipo de recursión: no utiliza recursión.
; Dominio: usuarios (string).
; Recorrido: lista de strings o vacía.
(define(getListaUserActivo paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (car (cdr (cdr (cdr (cdr (cdr paradigmaDocs))))))
      null))


; MODIFICADORES

; Desripción: Cambia el nombre del paradigmadocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: string.
; Recorrido: string.
(define(setNombre paradigmaDocs nombre)
  (list nombre (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs) (getListaUserActivo paradigmaDocs)))

; Desripción: cambia la fecha.
; Tipo de recursión: no utiliza recursión.
; Dominio: entero.
; Recorrido: entero.
(define(setFecha paradigmaDocs fecha)
  (list (getNombre paradigmaDocs) fecha (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs))(getListaUsers paradigmaDocs) (getListaUserActivo paradigmaDocs))

; Desripción: cambia la listaUsers
; Tipo de recursión: no utiliza recursión como tal, una función que se utiliza dentro de setListaUsers
;                    utiliza recursión.
; Dominio: usuario (string).
; Recorrido: lista de strings.
(define(setListaUsers paradigmaDocs usuario)
  (list (getNombre paradigmaDocs) (getFecha paradigmaDocs)(getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs) (guardarUser (getListaUsers paradigmaDocs) usuario) (getListaUserActivo paradigmaDocs)))

; Desripción: cambia la listaUserActivo, va a servir para la función login.
; Tipo de recursión: no utiliza recursión como tal, una función que se utiliza dentro de setListaUsers
;                    utiliza recursión.
; Dominio: usuario (string).
; Recorrido: lista de strings.
(define(setListaUsersActivo paradigmaDocs usuario)
  (list (getNombre paradigmaDocs)(getFecha paradigmaDocs)(getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs) (guardarUser (getListaUserActivo paradigmaDocs) usuario)))



; FUNCIONES COMPLEMENTARIAS

; Descripción: revisa si el usuario ya está registrado
; Tipo de recursión: de cola / debería ser recursión natural, estoy pensando en como cambiarla 01-11-2021
; Dominio: lista y string.
; Recorrido: booleano
(define(enLista listaUsers usuario)
  (if (not(null? listaUsers))
      (if(eqv? (cadr(car listaUsers)) usuario)
         #t
         (enLista (cdr listaUsers) usuario))
      #f))

; Descripción: Guarda usuarios en paradigmadocs
; Tipo de recursión: no utiliza recursión.
; Dominio: lista y usuario.
; Recorrido: lista con strings.
(define(guardarUser listaUsers usuario)
  (if (not(enLista listaUsers (cadr usuario)))
      (cons usuario listaUsers)
      listaUsers))


(provide (all-defined-out))