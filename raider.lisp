(defpackage #:raider
    (:use #:cl))

(in-package #:raider)

(defmacro array-iterate (array xname yname &body body)
  (let ((arr (gensym)) (xsize (gensym)) (ysize (gensym)))
    `(let ((,arr ,array))
       (destructuring-bind (,xsize ,ysize) (array-dimensions ,arr)
         (loop for ,xname from 0 to (1- ,xsize)
               do (loop for ,yname from 0 to (1- ,ysize)
                        do (progn ,@body)))))))

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
  ((current-turn :accessor current-turn :initform 0)
   (terrain :accessor terrain :initarg :terrain)
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
    (array-iterate array x y (setf (aref array x y) (make-instance 'terrain)))
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
    (loop for entity in entities do
         (let ((p (pos entity)))
           (if (and (= (pos-x p) x) (= (pos-y p) y))
               (setf entity-here entity))))
    entity-here))

(defun display-arena (arena)
  (with-slots (terrain entities current-turn) arena
    (format t "Turn ~a~%" current-turn)
    (destructuring-bind (xsize ysize) (array-dimensions terrain)
      (loop for y from (- ysize 1) downto 0
           do (loop for x from 0 to (- xsize 1)
                   do (let ((e (find-entity entities x y)))
                        (cond (e (if (dchar e)
                                     (format t "~a" (dchar e))
                                     (format t "A")))
                              ((passable (aref terrain x y)) (format t "."))
                              (t (format t "O")))))
              (format t "~%")))
    (loop for entity in entities do
         (format t "(~a ~a) ~a ~a ~a~%" (pos-x (pos entity)) (pos-y (pos entity))
                 (name entity) (team entity) (stats entity)))))

(defun make-ability ()
  (make-instance 'ability :name "attack" :range 'touch :effect '((hp-damage . 1))))

(defun make-move-test-arena ()
  (let ((arena (make-arena '(10 10))))
    (add-entity arena (make-instance 'entity
                                     :name "hero1"
                                     :team 'hero
                                     :stats (make-stats)
                                     :position (make-pos :x 0 :y 0)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero2"
                                     :team 'hero
                                     :stats (make-stats)
                                     :position (make-pos :x 1 :y 0)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero3"
                                     :team 'hero
                                     :stats (make-stats)
                                     :position (make-pos :x 0 :y 1)
                                     :abilities (list (make-ability))
                                     :display-char "H"))
    (add-entity arena (make-instance 'entity
                                     :name "hero4"
                                     :team 'hero
                                     :stats (make-stats)
                                     :position (make-pos :x 1 :y 1)
                                     :abilities (list (make-ability))
                                     :display-char "H"))

    (add-entity arena (make-instance 'entity
                                     :name "monster1"
                                     :team 'monster
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 9 :y 9)))
    (add-entity arena (make-instance 'entity
                                     :name "monster2"
                                     :team 'monster
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 9 :y 8)))
    (add-entity arena (make-instance 'entity
                                     :name "monster3"
                                     :team 'monster
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 8 :y 9)))
    (add-entity arena (make-instance 'entity
                                     :name "monster4"
                                     :team 'monster
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 8 :y 8)))

    (setf (turn-order arena) '(hero monster))
    (add-behavior arena 'hero #'null-behavior)
    (add-behavior arena 'monster #'move-down-behavior)
    arena))

(defun make-stats ()
  (list (cons "hp" 20) (cons "move" 5)))

(defun get-stat (entity stat)
  (let ((stat-entry (assoc stat (stats entity) :test #'equal)))
    (if stat-entry
        (cdr stat-entry)
        nil)))

(defun make-attack-test-arena ()
  (let ((arena (make-arena (list 10 10))))
    (add-entity arena (make-instance 'entity
                                     :name "hero1"
                                     :team 'hero
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "H"
                                     :position (make-pos :x 4 :y 4)))
    (add-entity arena (make-instance 'entity
                                     :name "hero2"
                                     :team 'hero
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "H"
                                     :position (make-pos :x 5 :y 5)))
    (add-entity arena (make-instance 'entity
                                     :name "monster1"
                                     :team 'monster
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 5 :y 4)))
    (add-entity arena (make-instance 'entity
                                     :name "monster2"
                                     :team 'monster
                                     :stats (make-stats)
                                     :abilities (list (make-ability))
                                     :display-char "M"
                                     :position (make-pos :x 4 :y 5)))
    (setf (turn-order arena) '(hero monster))
    (add-behavior arena 'hero #'hero-attack-test-behavior)
    (add-behavior arena 'monster #'monster-attack-test-behavior)
    arena))

(defun get-team-entities (arena team)
 (mapcan #'(lambda (ent) (if (eq team (team ent)) (list ent))) (entities arena)))

(defun move-down-behavior (arena team)
  (let ((teammates (get-team-entities arena team))
        (result-list nil))
    (loop for ent in teammates do
       (setf result-list (acons (name ent) (list 'move 'down) result-list)))
    result-list))

(defun hero-attack-test-behavior (arena team)
  (list (list "hero1" 'use-ability "attack" 'up)
        (list "hero2" 'use-ability "attack" 'down)))

(defun monster-attack-test-behavior (arena team)
  (list (list "monster1" 'use-ability "attack" 'right)
        (list "monster2" 'use-ability "attack" 'left)))

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
           (case (second a) ; this eventually needs bounds and terrain checking
             (up (incf (pos-y (pos entity))))
             (down (decf (pos-y (pos entity))))
             (left (decf (pos-x (pos entity))))
             (right (incf (pos-x (pos entity))))))
          (t (format t "Team ~a Entity ~a: unknown action ~a~%"
                     team (name entity) a)))))

(defun resolve-turn (arena)
  (loop for team in (turn-order arena) do
       (format t "resolving turn for ~a~%" team)
       (let ((behavior-cons (assoc team (behaviors arena))))
         (if (not behavior-cons)
             (format t "Team ~a does not have a behavior. Skipping...~%" team)
             (loop for action in (funcall (cdr behavior-cons) arena team) do
                  (resolve-action arena team action)))))
  (incf (current-turn arena)))

(defun generate-neighbors (pos)
  (list (make-pos :x (1+ (pos-x pos)) :y (pos-y pos))
        (make-pos :x (1- (pos-x pos)) :y (pos-y pos))
        (make-pos :x (pos-x pos) :y (1+ (pos-y pos)))
        (make-pos :x (pos-x pos) :y (1- (pos-y pos)))))

(defun bounds-check (arena pos)
  (destructuring-bind (maxx maxy) (array-dimensions (terrain arena))
    (let ((x (pos-x pos))
          (y (pos-y pos)))
      (and (>= x 0) (>= y 0) (< x maxx) (< y maxy)))))

(defun pos-equal (pos1 pos2)
  (and (= (pos-x pos1) (pos-x pos2))
       (= (pos-y pos1) (pos-y pos2))))

(defun find-possible-moves (arena entity)
  (let* ((entitypos (pos entity))
         (move (get-stat entity "move"))
         (possible-moves nil)
         (move-queue (list (cons entitypos 0))))
    (do ((pos (car (first move-queue)) (car (first move-queue)))
         (current-moves (cdr (first move-queue)) (cdr (first move-queue))))
        ((not move-queue) possible-moves)
      (pop move-queue)
      (when (and (bounds-check arena pos)
                 (passable (aref (terrain arena) (pos-x pos) (pos-y pos)))
                 (not (find pos possible-moves :test #'pos-equal)))
        (push pos possible-moves)
        (when (< current-moves move)
          (setf move-queue (nconc move-queue (mapcar #'cons
                                                     (generate-neighbors pos)
                                                     (make-list 8 :initial-element (1+ current-moves))))))))))
