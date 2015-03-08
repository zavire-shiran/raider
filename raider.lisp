(defpackage #:raider
    (:use #:cl))

(in-package #:raider)

(defclass entity ()
  ((name :accessor name :initarg :name)
   (stats :accessor stats :initarg :stats)
   (team :accessor team :initarg :team)
   (position :accessor pos :initarg :position)
   (abilities :accessor abilities :initarg :abilities)
   (behavior :accessor behavior :initarg :behavior)))

(defclass terrain ()
  ((passable :accessor passable :initarg :passable :initform t)))

(defclass dungeon ()
  ((terrain :accessor terrain :initarg :terrain)
   (entities :accessor entities :initarg :entities :initform nil)))

(defun make-dungeon (size)
  (let ((array (make-array size)))
    (loop for x from 0 to (- (first size) 1)
         do (loop for y from 0 to (- (second size) 1)
                 do (setf (aref array x y) (make-instance 'terrain))))
    (make-instance 'dungeon :terrain array)))

(defun find-entity (entities x y)
  (let ((entity-here nil))
    (dolist (entity entities entity-here)
      (let ((p (pos entity)))
	(if (and (= (first p) x) (= (second p) y))
	    (setf entity-here entity))))))

(defun display-dungeon (dungeon)
  (with-slots (terrain entities) dungeon
    (destructuring-bind (xsize ysize) (array-dimensions terrain)
      (loop for x from 0 to (- xsize 1)
           do (loop for y from 0 to (- ysize 1)
                   do (let ((e (find-entity entities x y)))
			(cond (e (format t "A"))
			      ((passable (aref terrain x y)) (format t "."))
			      (t (format t "O")))))
              (format t "~%")))))
