;;; COPYRIGHT 2006 Sergio Garcia <sergio.garcia@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

;;; Used structures:
;;; a tree is a list composed of a pair (name . value) followed
;;; by many trees

(in-package #:torta)

(defparameter font-id 64000)
(defparameter radiansgrades 0.0174532)
(defparameter min-angle (* 2 radiansgrades)) ;; 2 degrees, in radians
(defparameter steps-per-angle 0.1)


(defun load-directory ()
    (let ((here #.(or *compile-file-truename* *load-truename*)))
          (make-pathname :directory (pathname-directory here))))

(defparameter torta-binary-font
  (load-time-value
   (make-pathname :name "freeserif" :type "fo" :version nil
                                          :defaults (load-time-value (load-directory)))))


(defparameter *color-scale*
 (mapcar
  (lambda (vals)
    (apply (lambda (r g b a) (make-instance 'rgba :r r :g g :b b :a a))
           (mapcar (lambda (val) (* 255 val)) vals)))
  '((1 0 0 1) (1 1 0 1) (0 1 0 1) (0 1 1 1) (0 0 1 1) (1 0 1 1) (1 0 0 1))))


(defparameter label-box-width 3400)
(defparameter label-box-height 320)

(defstruct torta-piece
  (slice nil) ; the little arch that graphically symbolizes the size of a file
  (line nil) ; the line that connects a slice and its label
  (level nil) ; the level of the arch
  (angle nil) ; the position of the slice in its circle, in radians
  (label-tag nil)                    ; the label describing this piece
  (add-label-tag nil)
  (slice-id nil) ; id and depth of the slice
  (line-id nil) ; id and depth of the line
  (label-id nil) ; the ID of the label
  ) ; the level of the piece


(defun get-counter (init-value delta)
  (let ((counter init-value))
    (lambda ()
      (incf counter delta))))

;;; The tree

(defun get-name-tree (tr) (caar tr))
(defun get-value-tree (tr) (cdar tr))
(defun get-children-tree (tr) (cdr tr))

;; calculate the number of steps we want to use for a certain angle
(defun number-of-steps (angle)
  (+ 1 (floor (* steps-per-angle (/ angle radiansgrades)))))

(defun size-to-english (size)
  (cond ((< size 1000)
         (format nil "~d KB~p" size size))
        ((< size 1000000)
         (format nil "~,2f MB~p" (/ size 1000) (/ size 1000)))
        (t (format nil "~,2f GB~p" (/ size 1000000) (/ size 1000000)))
        ))


(defun make-message (file-name size &optional (max-size 32))
  (let* ((size-in-words (size-to-english size)))
    (format nil "~A (~A)"
            (cond
              ((not file-name) "/")
              ((> (+ (length file-name) (length size-in-words) )  max-size)
               (format nil "~A..." (subseq file-name 0 (- max-size (length size-in-words) 3))))
              (t file-name))
            size-in-words
            )))


(defun make-label-tag (message id &optional (alignment :RIGHT) (width label-box-width) (height label-box-height))
  (make-instance 'tag-define-edit-text
   :border nil
   :bounds (make-instance 'rect :xmin 0 :ymin 0 :xmax width :ymax height)
   :align alignment
   :max-length 100
   :left-margin 0
   :right-margin 0
   :indent 5
   :leading 5
   :id id
   :font-id font-id
   :autosize nil
   :use-outlines t
   :font-height 200
   :html nil
   :variable-name (format nil "_root.sergio~A" id)
   :editable nil
   :initial-text message
   :selectable nil
   :color *a-black*)
  )



(defun paint-tree (tree old-value old-angle init-angle width radio level num-layers initial-width max-depth labels-ids-dispenser slices-ids-dispenser movie x-canvas-size y-canvas-size parent-color)
  (let ((current-angle (* old-angle (/ (get-value-tree tree) old-value))))
    (when
        (> current-angle min-angle)
                                        ; this is confusing as hell due to the fact that archs in my API
                                        ; start at angle pi/2 and go on the sense of clock hands...
                                        ; (fix this, please its ugly)
      (let* (
             (x-half-canvas (floor ( / x-canvas-size 2)))
             (y-half-canvas (floor ( / y-canvas-size 2)))
             (slice-middle-angle-c (+ (/ pi 2) (* -1 (+ init-angle (/ current-angle 2))))) ; only calcule it
             (slice-middle-angle (if (< slice-middle-angle-c 0) (+ (* pi 2) slice-middle-angle-c) slice-middle-angle-c))
             (slice-middle-radio (- radio (/ width 2)))
             (slice-distance-from-border (* width (- max-depth level 1)))
             (label-tag-id (funcall labels-ids-dispenser))
             (line-tag-id (funcall labels-ids-dispenser))
             (slice-tag-id (funcall slices-ids-dispenser))
             (button-tag-id (funcall slices-ids-dispenser))
             (color-for-slice (color-for-slice (+ init-angle (/ current-angle 2)) parent-color))
             (slice (draw-shape-with-style `(,(make-bridge-contour (number-of-steps current-angle) radio current-angle width :initial-angle init-angle)) :fill-color color-for-slice :stroke-width 20))
             ;; Im not using this at the moment:
             (line
              (draw-shape-with-style
               `((
                  ;; start in the center of the slice
                  (,(floor (* slice-middle-radio (cos slice-middle-angle)))
                   ,(* -1 (floor (* slice-middle-radio (sin slice-middle-angle)))))
                  ;; go to the end
                  (,(floor (* slice-distance-from-border (cos slice-middle-angle)))
                   ,(* -1 (- (floor (+
                                     (* slice-distance-from-border (sin slice-middle-angle)))) (/ label-box-height 2))))
                  ;; and return to the center of the slice (or flash player will go bananas)
                  (,(floor (* -1 slice-distance-from-border (cos slice-middle-angle)))
                   ,(floor (- (* slice-distance-from-border (sin slice-middle-angle))
                              (/ label-box-height 2)
                              )))

                  ))
               :stroke-color *a-black*
               ))
             (label-tag (make-label-tag (make-message (car (get-name-tree tree))
                                                      (get-value-tree tree) )
                                        label-tag-id
                                        (if (and (> slice-middle-angle (/ pi 2))
                                                 (< slice-middle-angle (* pi 1.5)))
                                            :RIGHT :LEFT))))

                                        ;   (add-shape movie slice-tag-id slice-tag-id slice half-canvas half-canvas)


        (add-to-movie movie (make-instance 'tag-define-shape :id slice-tag-id :shape slice))
        ;(add-to-movie movie (make-tag-define-shape :id line-tag-id :shape line))


        (add-to-movie movie label-tag)
        (add-to-movie movie
                 (make-instance 'tag-define-button-2
                  :id button-tag-id
                  :characters (list


                               (make-instance 'button-record
                                :state-over t
                                :state-down t
                                :character-id slice-tag-id
                                :depth 1
                                :place-matrix (make-instance 'matrix)
                                :color-transform (make-instance 'cxform-wa
                                                  :amult 1.0
                                                  :radd 100
                                                  :rmult 2.0
                                                  :gmult 2.0
                                                  :bmult 2.0))
                               (make-instance 'button-record
                                :state-up t
                                :test-hit t
                                :character-id slice-tag-id
                                :depth 3
                                :place-matrix (make-instance 'matrix)
                                :color-transform (make-instance 'cxform-wa))

                               (let ((offset
                                      (if (or (< slice-middle-angle (/ pi 2))
                                              (> slice-middle-angle (* pi 1.5)))
                                          0
                                          (* -1 label-box-width))))
                                 (make-instance 'button-record
                                  :depth 2
                                  :state-over t
                                  :state-down t
                                  :character-id label-tag-id
                                  :place-matrix (translation-matrix
                                                 (+ offset
                                                    (floor (*
                                                            (+ slice-middle-radio slice-distance-from-border)
                                                            (cos slice-middle-angle))))
                                                 (+
                                                  (floor

                                                   (* -1 (* (+ slice-middle-radio slice-distance-from-border) (sin slice-middle-angle))))))
                                  :color-transform (make-instance 'cxform-wa)))
                               )))

        (add-to-movie movie (make-instance 'tag-place-object2 :depth button-tag-id :id button-tag-id :matrix (make-instance 'matrix :translate-x x-half-canvas :translate-y y-half-canvas)))

        (paint-trees (get-children-tree tree) current-angle (get-value-tree tree) init-angle width radio (1+ level) num-layers initial-width max-depth labels-ids-dispenser slices-ids-dispenser movie x-canvas-size y-canvas-size color-for-slice)))))



(defun paint-trees (trees total-angle total-value init-angle width radio level num-layers initial-width max-depth labels-ids-dispenser slices-ids-dispenser movie x-canvas-size y-canvas-size color)
  (loop
   for tree in trees
   do (paint-tree tree total-value total-angle (+ sub-total-angle init-angle) width (+ radio width) level num-layers initial-width max-depth labels-ids-dispenser slices-ids-dispenser movie x-canvas-size y-canvas-size color)
   summing (let ((current-angle (* total-angle (/ (get-value-tree tree) total-value))))
             (if (> current-angle min-angle)
                 current-angle
                 0)) into sub-total-angle))


(defun color-for-slice (angle parent-color)
  (assert (< angle (* 2 pi)))
  (let* ((pos-in-scale (* (/ angle (* 2 pi)) (- (length *color-scale*) 1)))
         (pos-in-list (nthcdr (floor pos-in-scale) *color-scale*))
         (sub-pos (mod pos-in-scale 1)))
    (flet ((get-color (f)
             (round (funcall
                     (if parent-color
                         (lambda (v) (/ (+ (funcall f parent-color) v) 2))
                         #'identity)
                     (+ (* (- 1 sub-pos) (funcall f (car pos-in-list)))
                          (* sub-pos (funcall f (cadr pos-in-list))))))))
      (make-instance 'rgba
                     :r (get-color #'rgb-r)
                     :g (get-color #'rgb-g)
                     :b (get-color #'rgb-b)
                     :a (get-color #'rgba-a)))))


(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
   (if p1
       (let ((p2 (position-if
                  #'(lambda (c)
                      (not (funcall test c)))
                  str :start p1)))
         (cons (subseq str p1 p2)
               (if p2
                   (tokens str test p2)
                   nil)))
       nil)))

(defun parse-path (line)
  (reverse (tokens line (lambda (c) (not (char= c #\/))) 0)))





(defun read-file-size (str eof-symbol)
  (let ((size (read str nil eof-symbol)))
    (if (not (eq size eof-symbol))
        (let ((line (read-line str nil eof-symbol)))
          (cons
           (parse-path line)
           size))
        eof-symbol)))


(defun is-in-dir-p (path1 path2)
  (equal (cdr path1) path2))




(defun read-sizes (dirname max-depth)
  (let ((root-depth (length (parse-path dirname))))
    (with-open-pipe (str (pipe-input "/usr/bin/du" "-ak" dirname))
      (car (loop
            with tree = '()
            for file-size = (read-file-size str 'eof) then (read-file-size str 'eof)
            do (if (eq file-size 'eof)
                   (return tree)
                   (when (or (not max-depth) (<= (- (length (car file-size)) root-depth) max-depth))
                     (setf tree
                           (loop
                            with offspring = '()
                            with brothers = ()
                            for item in tree
                            do (if (is-in-dir-p (get-name-tree item) (car file-size))
                                   (setf offspring (cons item offspring))
                                   (setf brothers (cons item brothers))
                                   )
                            finally (return (cons (cons file-size offspring) brothers)))))))))))

(defun nodes-in-tree (tree)
  (+ 1
     (reduce #'+ (mapcar #'nodes-in-tree (get-children-tree tree)) :initial-value 0)))


(defun max-tree-depth (tree)
  (let ((children (get-children-tree tree)))
    (if (null children)
        1
        ; Im looping here instead of calling max because clisp's max
        ; seems to die with a big number of arguments.
        ;
        (+ 1 (loop for item in (mapcar (lambda (subtree) (max-tree-depth subtree)) children) maximizing item)))))

(defun max-tree-depth-visible (tree)
  (labels ((max-tdv-helper (tree big-total)
             (let ((children (get-children-tree tree)))
               (if (> (* 2 pi (/  (get-value-tree tree) big-total)) min-angle)
                   (if (null children)
                       1
                       (+ 1 (loop for item in (mapcar (lambda (subtree) (max-tdv-helper subtree big-total)) children) maximizing item)))
                   0))))
        (max-tdv-helper tree (get-value-tree tree))))



(defun torta (fileroot &key (output "torta.swf") (radius 1000) (slice-width 400) (max-depth nil))
  (with-open-file (fontstream torta-binary-font :direction :input :element-type '(unsigned-byte 8))
    (let ((tree (read-sizes fileroot max-depth)))
      (when (zerop (get-value-tree tree)) (error "I cant draw 0 sized trees"))
      (let* (
             (max-visible-depth (max-tree-depth-visible tree))
             (max-slices (* 2 (nodes-in-tree tree)) )
             (total-radius (+ radius (* slice-width (- max-visible-depth 1))))
             (labels-ids-dispenser (get-counter max-slices 1))
             (slices-ids-dispenser (get-counter 1 1))
             (x-canvas-size (+
                             (* 2 radius)
                             (* 2 max-visible-depth slice-width)
                             (* 2 label-box-width)))

             (y-canvas-size (+

                             (* 2 radius)
                             (* 2 max-visible-depth slice-width)
                             (* 2 label-box-height))))


        (with-movie (m output
                       :width x-canvas-size
                       :height y-canvas-size
                       :frame-rate 12
                       :bgcolor *white*)
          (add-to-movie m fontstream)
          (paint-trees (get-children-tree tree)
                       (* 2 pi)
                       (get-value-tree tree)
                       0 slice-width radius 0 max-visible-depth radius max-visible-depth labels-ids-dispenser slices-ids-dispenser m x-canvas-size y-canvas-size nil)
          (add-to-movie m
                   (make-label-tag (make-message (car (get-name-tree tree))
                                                 (get-value-tree tree))
                                   63000
                                   :CENTER
                                   (* 2 radius) label-box-height))
          (add-to-movie m (make-instance 'tag-place-object2 :depth 63000 :id 63000 :matrix (make-instance 'matrix :translate-x (- (floor (/ x-canvas-size 2)) radius) :translate-y (floor (/ (- y-canvas-size label-box-height) 2)))))
          (add-to-movie m (make-instance 'tag-showframe))
          (add-to-movie m (make-instance 'tag-end)))))))

