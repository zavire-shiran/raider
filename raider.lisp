(defpackage #:raider
    (:use #:cl))

(in-package #:raider)

(defclass entity ()
  ((name :accessor name :initarg :name)
   (display-char :accessor dchar :initarg :display-char :initform nil)
   (stats :accessor stats :initarg :stats)
   (team :accessor team :initarg :team)
   (position :accessor pos :initarg :position)
   (abilities :accessor abilities :initarg :abilities)))

(defclass terrain ()
  ((passable :accessor passable :initarg :passable :initform t)))

(defclass arena ()
  ((terrain :accessor terrain :initarg :terrain)
   (entities :accessor entities :initarg :entities :initform nil)
   (behaviors :accessor behaviors :initarg :behaviors :initform nil)))

(defclass ability ()
  ((name :accessor name :initarg :name)
   (cost :accessor cost :initarg :cost)
   (range :accessor range :initarg :range)
   (effect :accessor effect :initarg :effect)
   (cooldown :accessor cooldown :initform 0)))

(defun make-arena (size)
  (let ((array (make-array size)))
    (loop for x from 0 to (- (first size) 1)
         do (loop for y from 0 to (- (second size) 1)
                 do (setf (aref array x y) (make-instance 'terrain))))
    (make-instance 'arena :terrain array)))

(defun add-behavior (arena team behavior)
  (let ((item (assoc team (behaviors arena))))
    (if item
        (setf (cdr item) behavior)
        (setf (behavior arena) (acons team behavior (behavior arena))))))

(defun add-entity (arena entity)
  (setf (entities arena) (cons entity (entities arena))))

(defun find-entity (entities x y)
  (let ((entity-here nil))
    (dolist (entity entities entity-here)
      (let ((p (pos entity)))
        (if (and (= (first p) x) (= (second p) y))
            (setf entity-here entity))))))

(defun display-arena (arena)
  (with-slots (terrain entities) arena
    (destructuring-bind (xsize ysize) (array-dimensions terrain)
      (loop for x from 0 to (- xsize 1)
           do (loop for y from 0 to (- ysize 1)
                   do (let ((e (find-entity entities x y)))
                        (cond (e (if (dchar e)
                                     (format t "~a" (dchar e))
                                     (format t "A")))
                              ((passable (aref terrain x y)) (format t "."))
                              (t (format t "O")))))
              (format t "~%")))))

(defun make-ability ()
  (make-instance 'ability :name "attack" :range 1 :effect '((hp-damage . 1))))

(defun make-test-arena ()
  (let ((arena (make-arena '(10 10))))
    (add-entity arena (make-instance 'entity
                                     :name "hero1"
                                     :team 'hero
                                     :position (list 0 0)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero2"
                                     :team 'hero
                                     :position (list 1 0)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero3"
                                     :team 'hero
                                     :position (list 0 1)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero4"
                                     :team 'hero
                                     :position (list 1 1)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "monster1"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (list 9 9)))
    (add-entity arena (make-instance 'entity
                                     :name "monster2"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (list 8 9)))
    (add-entity arena (make-instance 'entity
                                     :name "monster3"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (list 9 8)))
    (add-entity arena (make-instance 'entity
                                     :name "monster4"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (list 8 8)))
    arena))
