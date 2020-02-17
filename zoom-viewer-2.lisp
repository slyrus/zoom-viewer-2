
(defpackage #:zoom-viewer-2
  (:use #:clim #:clim-lisp #:clim-extensions #:mcclim-bezier))

(in-package #:zoom-viewer-2)

(defclass zoom-pane (application-pane clime:always-repaint-background-mixin)
  ((zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)))

(defmethod handle-repaint ((pane zoom-pane) region)
  (with-sheet-medium (medium pane)
    (with-bounding-rectangle* (x1 y1 x2 y2) (medium-sheet medium)
      #+nil
      (medium-clear-area medium x1 y1 x2 y2)
      (let ((x-grid-width 2)
            (y-grid-width 2)
            (x-grid-color +red+)
            (y-grid-color +blue+))
        (let ;; take width away from both x2 and y2 so that we draw inside the
            ;; bounding rectangle
            ((grid-x2 (- x2 x-grid-width))
             (grid-y2 (- y2 y-grid-width)))
          (let ((x-divisions 20)
                (y-divisions 20)
                (medium-width (- grid-x2 x1))
                (medium-height (- grid-y2 y1)))
            (loop for i from x1 to grid-x2 by (/ medium-width x-divisions)
               do
                 (draw-line* medium i y1 i grid-y2 :line-thickness y-grid-width :ink y-grid-color))
            (loop for i from y1 to grid-y2 by (/ medium-height y-divisions)
               do
                 (draw-line* medium x1 i grid-x2 i :line-thickness x-grid-width :ink x-grid-color))))))))

(defun zoom-x-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'app)))
      (setf (zoom-x-level pane) scale)
      (repaint-sheet pane +everywhere+))))

(defun zoom-y-callback (gadget scale)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'app)))
      (setf (zoom-y-level pane) scale)
      (repaint-sheet pane +everywhere+))))

(defclass redisplay-event (clim:window-event) ())

(defmethod queue-redisplay ((pane zoom-pane))
  (queue-event pane (make-instance 'redisplay-event :sheet pane)))

(defmethod handle-event ((client zoom-pane)
                         (event  redisplay-event))
  (redisplay-frame-pane (pane-frame client) client :force-p t))

(define-application-frame zoom-viewer-2-app ()
  ()
  (:panes
   (app zoom-pane
        :height 512 :width 1024
        :display-time t
        :display-function 'display-zoom-viewer-2)
   (int :interactor :height 200 :width 600)
   (zoom-x :slider
           :min-value 0.1
           :max-value 10
           :decimal-places 2
           :value 1.0d0
           :show-value-p t
           :orientation :horizontal
           :drag-callback 'zoom-x-callback
           :value-changed-callback 'zoom-x-callback)
   (zoom-y :slider
           :min-value 0.1
           :max-value 10
           :decimal-places 2
           :value 1.0d0
           :show-value-p t
           :orientation :horizontal
           :drag-callback 'zoom-y-callback
           :value-changed-callback 'zoom-y-callback))
  (:layouts
   (default (vertically ()
              app
              (labelling (:label "Zoom X")
                zoom-x)
              (labelling (:label "Zoom Y")
                zoom-y)
              int))))

(define-zoom-viewer-2-app-command (resize-window :name t) ()
  (let* ((frame *application-frame*)
         (pane (find-pane-named frame 'app)))
    (setf (pane-needs-redisplay pane) :t)
    (redisplay-frame-pane frame pane :force-p t )
    #+nil
    (progn
      (with-bounding-rectangle* (left top right bottom) pane
        (when (sheet-viewable-p pane)
          (medium-clear-area (sheet-medium pane) left top right bottom)))
      (climi::invoke-display-function frame pane))))

(defmethod note-space-requirements-changed (sheet (pane zoom-pane))
    )

(defclass grid () ())

(define-presentation-method present (grid (type grid) pane view &key)
  )

(defparameter *grid* (make-instance 'grid))

(defun display-zoom-viewer-2 (frame pane)
  (declare (ignore frame pane))
  (present *grid* (class-of *grid*) :single-box t))

(defun zoom-viewer-2-main ()
  (let ((frame (make-application-frame 'zoom-viewer-2-app)))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

#+nil
(defmethod allocate-space :after ((pane zoom-pane) width height)
  (execute-frame-command (pane-frame pane) '(resize-window)))

(defmethod note-sheet-region-changed :after ((pane zoom-pane))
  (queue-redisplay pane)
  #+nil (redisplay-frame-pane (pane-frame pane) pane)
  #+nil (setf (pane-needs-redisplay pane) :t)
  )

;;
;; This sort of works but doesn't seem right
#+nil
(defmethod handle-event :after ((sheet climi::top-level-sheet-pane)
                                (event window-configuration-event))
  (let* ((frame (pane-frame sheet))
         (pane (find-pane-named frame 'app)))
    #+nil
    (execute-frame-command (pane-frame pane) '(resize-window))
    #+nil
    (redisplay-frame-pane frame pane)
    #+nil
    (progn
      (with-bounding-rectangle* (left top right bottom) pane
        (when (sheet-viewable-p pane)
          (medium-clear-area (sheet-medium pane) left top right bottom)))
      (climi::invoke-display-function frame pane))))


#+nil
(compute-applicable-methods #'allocate-space (list (find-pane-named
                                                    (find-application-frame 'zoom-viewer-2-app)
                                                    'app)
                                                   t t))


;; (remove-method #'allocate-space (find-method #'allocate-space '(:around) (list (find-class 'zoom-pane) t t)))

(defun simple-remove-method (method args &optional qualifiers)
  (remove-method (symbol-function method) (find-method (symbol-function method)
                                                       qualifiers
                                                       (mapcar (lambda (x)
                                                                 (if (equal x t)
                                                                     t
                                                                     (find-class x)))
                                                               args))))

;; (simple-remove-method 'handle-event '(climi::top-level-sheet-pane window-configuration-event))

;; (remove-method #'handle-event (find-method #'handle-event '(:after) (list (find-class 'zoom-pane) t t)))

