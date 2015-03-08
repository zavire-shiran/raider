(defpackage #:raider
    (:use #:cl))

(in-package #:raider)

(defstruct (pos (:conc-name pos-)) x y)

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
   (behaviors :accessor behaviors :initarg :behaviors :initform nil)
   (turn-order :accessor turn-order :initarg :turn-order :initform nil)))

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
        (setf (behaviors arena) (acons team behavior (behaviors arena))))))

(defun add-entity (arena entity)
  (setf (entities arena) (cons entity (entities arena))))

(defun find-entity (entities x y)
  (let ((entity-here nil))
    (dolist (entity entities entity-here)
      (let ((p (pos entity)))
        (if (and (= (pos-x p) x) (= (pos-y p) y))
            (setf entity-here entity))))))

(defun display-arena (arena)
  (with-slots (terrain entities) arena
    (destructuring-bind (xsize ysize) (array-dimensions terrain)
      (loop for y from (- ysize 1) downto 0
           do (loop for x from 0 to (- xsize 1)
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
                                     :position (make-pos :x 0 :y 0)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero2"
                                     :team 'hero
                                     :position (make-pos :x 1 :y 0)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero3"
                                     :team 'hero
                                     :position (make-pos :x 0 :y 1)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero4"
                                     :team 'hero
                                     :position (make-pos :x 1 :y 1)
                                     :abilities (list (make-ability))
                                     :display-char "H"))

    (add-entity arena (make-instance 'entity
                                     :name "monster1"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 9 :y 9)))
    (add-entity arena (make-instance 'entity
                                     :name "monster2"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 9 :y 8)))
    (add-entity arena (make-instance 'entity
                                     :name "monster3"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 8 :y 9)))
    (add-entity arena (make-instance 'entity
                                     :name "monster4"
                                     :team 'monster
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 8 :y 8)))

    (setf (turn-order arena) '(hero monster))
    (add-behavior arena 'hero #'null-behavior)
    (add-behavior arena 'monster #'move-down-behavior)
    arena))

(defun get-team-entities (arena team)
 (mapcan #'(lambda (ent) (if (eq team (team ent)) (list ent))) (entities arena)))

(defun move-down-behavior (arena team)
  (let ((teammates (get-team-entities arena team))
        (result-list nil))
    (dolist (ent teammates result-list)
      (setf result-list (acons (name ent) (list 'move 'down) result-list)))))

(defun null-behavior (arena team)
  nil)

(defun get-entity-by-name (arena name)
  (loop for ent in (entities arena)
       if (equal (name ent) name) return ent))

(defun resolve-action (arena team action)
  (format t "resolving action ~a~%" action)
  (let ((entity (get-entity-by-name arena (car action)))
        (a (cdr action)))
    (cond ((eq (first a) 'move)
           (case (second a) ; this eventually needs bounds checking
             (up (incf (pos-y (pos entity))))
             (down (decf (pos-y (pos entity))))
             (left (decf (pos-x (pos entity))))
             (right (incf (pos-x (pos entity))))))
          (t (format t "Team ~a Entity ~a: unknown action ~a~%"
                     team (name entity) a)))))

(defun resolve-turn (arena)
  (dolist (team (turn-order arena) arena)
    (format t "resolving turn for ~a~%" team)
    (let ((behavior-cons (assoc team (behaviors arena))))
      (if (not behavior-cons)
          (format t "Team ~a does not have a behavior. Skipping...~%" team)
          (dolist (action (funcall (cdr behavior-cons) arena team))
            (resolve-action arena team action))))))
