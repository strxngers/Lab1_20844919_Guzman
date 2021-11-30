#lang racket

; Para la creación de ParadigmaDocs se requiere usar TDA Fecha
(require "TDAFecha_20844919_GuzmanMadriaga.rkt")
(require "TDAUser_20844919_GuzmanMadriaga.rkt")
(require "TDADocumento_20844919_GuzmanMadriaga.rkt")


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
  (list (getName paradigmaDocs) (getFecha paradigmaDocs)(getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs) (saveUser (getListaUsers paradigmaDocs) usuario) (getListaUserActivo paradigmaDocs) (getDocs paradigmaDocs)))

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
  (list (getName paradigmaDocs) (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs)
  (list ) (appendDoc (setID doc (createID (getDocs paradigmaDocs))) (getDocs paradigmaDocs))))


; Desripción: agrega un nuevo accesso a la lista de accesos.
; Tipo de recursión: no utiliza recursión.
; Dominio: paradigmadocs y iD tipo enter.
; Recorrido: entero.
(define(setAcc paradigmaDocs iD user access)
  (list (getName paradigmaDocs) (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs)
  (list ) (editDocAc iD (getDocs paradigmaDocs) user access)))


; Desripción: entrega una versión actualizada del paradigmadocs con el nuevo contenido añadido al documento.
; Tipo de recursión: una de las funciones utilizadas utiliza recursión de cola.
; Dominio: paradigmadocs y iD tipo entero.
; Recorrido: entero.
(define(addContent paradigmaDocs iD user newContent)
  (list (getName paradigmaDocs) (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs)
  (list ) (editDocCont (getDocs paradigmaDocs) iD user newContent)))


; Desripción: función que entrega una versión actualizada de paradigmadocs con el contenido de la versión restaurada que se desea.
; Tipo de recursión: no utiliza recursión.
; Dominio: paradigmadocs tipo lista con enteros, strings y listas, iDs tipo entero y usuario tipo string.
; Recorrido: paradigmadocs tipo lista con enteros, strings y listas.
(define(rV paradigmaDocs iD idVersion user)
  (list (getName paradigmaDocs) (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs)
  (list ) (editDocVer (getDocs paradigmaDocs) iD user idVersion)))


; Desripción: función que entrega una versión actualizada de paradigmadocs sin los accesos que se habían entregado anteriormente.
; Tipo de recursión: no utiliza recursión.
; Dominio: paradigmadocs tipo lista con enteros, strings y listas, usuario tipo string.
; Recorrido: paradigmadocs tipo lista con enteros, strings y listas.
(define(revok paradigmaDocs user)
  (list (getName paradigmaDocs) (getFecha paradigmaDocs) (getEncryptFunction paradigmaDocs)(getDecryptFunction paradigmaDocs)(getListaUsers paradigmaDocs)
  (list ) (resetAccesses (getDocs paradigmaDocs) user)))




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
(define(saveUser listaUsers usuario)
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

; Desripción: agrega un documento a la lista de documentos.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo string y lista. 
; Recorrido: lista con strings.
(define (appendDoc doc listaDocs)
  (reverse (cons doc (reverse listaDocs))))

; Desripción: crea una ID diferente a la inicial (0).
; Tipo de recursión: recursión de cola.
; Dominio: lista.
; Recorrido: lista con enteros.
(define(createID listaDocs)
  (if (not (empty? listaDocs))
      (if (= 1 (length listaDocs))
          (+ 1 (getID(car listaDocs)))
          (createID (cdr listaDocs)))
      0))

; Desripción: agregar los accesos de un documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: ID tipo enterto, lista con strings (o en su defecto vacía), usuario tipo string y access tipo string.
; Recorrido: lista con enteros y strings.
(define(editDocAc iD listaDocs user access)
  (map(lambda (doc)
        (if (equal? (getID doc) iD)
            (canGiveAc doc user access)
            doc))listaDocs))


; Desripción: función que revisa que las iD de los documentos en lista corresponda con la iD de los documentos existentes, de ser así, agrega el contenido.
; Tipo de recursión: recursión de cola.
; Dominio: ID tipo enterto, lista con strings (o en su defecto vacía), usuario tipo string y access tipo string.
; Recorrido: lista con enteros y strings.
(define(editDocCont listaDocs iD user content)
  (map(lambda (doc)
        (if (equal? (getID doc) iD)
            (editContent doc user content)
            doc))listaDocs))


; Desripción: función que revisa que las iD de los documentos en lista corresponda con la iD de los documentos existentes, de ser así, agrega el contenido.
; Tipo de recursión: recursión de cola.
; Dominio: ID tipo enterto, lista con strings (o en su defecto vacía), usuario tipo string y access tipo string.
; Recorrido: lista con enteros y strings.
(define(editDocVer listaDocs iD user idVersion)
  (map(lambda (doc)
        (if (equal? (getID doc) iD)
            (restore doc idVersion user)
            doc))listaDocs))


; Desripción: función que entrega una versión del documento en la que se quitaron los accesos a el/los documentos si el usuario es el dueño del documento.
; Tipo de recursión: no utiliza recursión.
; Dominio: documento tipo lista con enteros y strings, lista de documentos y usuario tipo string.
; Recorrido: documento actualizado sin los accesos en documentos.
(define(resetAccesses listaDocs user)
  (map(lambda (doc)
        (if (isOwner? doc user)
            (list (getFechaDoc doc)(getNameDoc doc) (getContent doc) (getOwner doc) (getID doc) (list ) (getVersions doc))
            doc))listaDocs))







; Exportación de funciones.

(provide (all-defined-out))