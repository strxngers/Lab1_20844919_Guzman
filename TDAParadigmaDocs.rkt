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
(define encryptFunction (lambda (s) (list->string (reverse (string->list s)))))
(define decryptFunction (lambda (s) (list->string (reverse (string->list s)))))

; CONSTRUCTOR

; Descripción: constructor constructor que crea un paradigmaDocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: string y entero
; Recorrido: lista que contiene el nombre de la plataforma, la fecha de creación, el texto entregado
; por encryptFn y el texto entregado por decryptFn
(define(createParadigmaDocs name fecha encryptFunction decryptFunction)
  (list name fecha encryptFunction decryptFunction (list ) (list ) (list )))

; PERTENENCIA

; Descripción: Comprueba si el formato entregado es correcto (el nombre del archivo debe ser un string).
; Tipo de recursión: no utiliza recursión.
; Dominio: documento ParadigmaDocs  
; Recorrido: booleano
(define(isParadigmaDocs? doc)
  (if (and (= (length doc))
           (string? (car doc))
           (esFecha (cadr doc))
           (list? (car(cddddr doc)))
           (list? (car (cdr (cdr (cdr (cdr doc))))))
           (list? (car (cdr (cdr (cdr (cdr (cdr doc)))))))
           (list? (car (cdr (cdr (cdr (cdr (cdr (cdr doc))))))))
           )
      #t
      #f))

; SELECTORES

; Desripción: Si corresponde a un nombre (string), retorna el nombre del paradigmadocs, en caso contrario
;             retorna null.
; Tipo de recursión: no utiliza recursión.
; Dominio: paradigmadocs.
; Recorrido: string.
(define(getName paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (car paradigmaDocs)
      null))

; Desripción: Si corresponde a una fecha retorna la fecha en que fue creado el paradigmaDocs; en caso contrario
;             retorna null.
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

; Desripción: Retorna la lista de usuarios activos.
; Tipo de recursión: no utiliza recursión.
; Dominio: usuarios (string).
; Recorrido: lista de strings o vacía.
(define(getListaUserActivo paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (car (cdr (cdr (cdr (cdr (cdr paradigmaDocs))))))
      null))


; Desripción: retorna la lista de cocumentos.
; Tipo de recursión: no utiliza recursión.
; Dominio: usuarios (string).
; Recorrido: lista de strings o vacía.
(define(getDocs paradigmaDocs)
  (if (isParadigmaDocs? paradigmaDocs)
      (car (cdr (cdr (cdr (cdr (cdr (cdr paradigmaDocs)))))))
      null))


; MODIFICADORES

; Desripción: Cambia el nombre del paradigmadocs.
; Tipo de recursión: no utiliza recursión.
; Dominio: string.
; Recorrido: string.
(define(setNombre paradigmaDocs name)
  (list name (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs) (getListaUserActivo paradigmaDocs) (getDocs paradigmaDocs)))

; Desripción: cambia la fecha.
; Tipo de recursión: no utiliza recursión.
; Dominio: entero.
; Recorrido: entero.
(define(setFecha paradigmaDocs fecha)
  (list (getName paradigmaDocs) fecha (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs))(getListaUsers paradigmaDocs) (getListaUserActivo paradigmaDocs) (getDocs paradigmaDocs))

; Desripción: cambia la listaUsers.
; Tipo de recursión: no utiliza recursión como tal, una función que se utiliza dentro de setListaUsers
;                    utiliza recursión.
; Dominio: paradigmadocs y usuario tipo string.
; Recorrido: lista de strings.
(define(setListaUsers paradigmaDocs usuario)
  (list (getName paradigmaDocs) (getFecha paradigmaDocs)(getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs) (guardarUser (getListaUsers paradigmaDocs) usuario) (getListaUserActivo paradigmaDocs) (getDocs paradigmaDocs)))

; Desripción: cambia la listaUserActivo, va a servir para la función login.
; Tipo de recursión: no utiliza recursión.
; Dominio: paradigmadocs y usuario tipo string.
; Recorrido: lista de strings.
(define(setListaUsersActivo paradigmaDocs usuario)
  (list (getName paradigmaDocs)(getFecha paradigmaDocs)(getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs) (list usuario) (getDocs paradigmaDocs)))

; Desripción: cambia el documento
; Tipo de recursión: no utiliza recursión.
; Dominio: entero.
; Recorrido: entero.
(define(setDoc paradigmaDocs doc)
  (list (getName paradigmaDocs) (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs))(getListaUsers paradigmaDocs) (getListaUserActivo paradigmaDocs) doc)



; FUNCIONES COMPLEMENTARIAS

; Descripción: revisa si el usuario ya está registrado o está activo.
; Tipo de recursión: de cola / debería ser recursión natural, estoy pensando en como cambiarla 01-11-2021
; Dominio: lista y usuario tipo string.
; Recorrido: booleano
(define(enLista listaUsers usuario)
  (if (not(null? listaUsers))
      (if(eqv? (cadr(car listaUsers)) usuario)
         #t
         (enLista (cdr listaUsers) usuario))
      #f))

; Descripción: Guarda usuarios y usuarios activos en paradigmadocs.
; Tipo de recursión: no utiliza recursión como tal, hace llamado a una función que utiliza recursión.
; Dominio: lista y usuario tipo string.
; Recorrido: lista con strings.
(define(guardarUser listaUsers usuario)
  (if (not(enLista listaUsers (cadr usuario)))
      (cons usuario listaUsers)
      listaUsers))


; Desripción: revisa si el usuario y la contraseña están en la lista de usuarios registrados.
; Tipo de recursión: utiliza recursión natural
; Dominio: lista con usuarios, contraseñas (ambos tipo string) y fechas (tipo entero).
; Recorrido: booleano.
(define(userPass listaUsers user password)
  (if (not(null? listaUsers))
      (if(and(eqv? (cadr(car listaUsers)) user)(eqv? (caddr(car listaUsers)) password)) 
         #t
         (userPass (cdr listaUsers) user password))
      #f))

; Exportación de funciones.

(provide (all-defined-out))