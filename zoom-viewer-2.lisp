
(defpackage #:zoom-viewer-2
  (:use #:clim #:clim-lisp #:clim-extensions #:mcclim-bezier))

(in-package #:zoom-viewer-2)

(defclass zoom-pane (application-pane clime:always-repaint-background-mixin)
  ((zoom-record :initform (make-instance 'standard-tree-output-history)
                :accessor zoom-record)
   (zoom-x-level :initform 1.0d0 :accessor zoom-x-level)
   (zoom-y-level :initform 1.0d0 :accessor zoom-y-level)
   (scroll-x-position :initform 1.0d0 :accessor scroll-x-position)
   (scroll-y-position :initform 1.0d0 :accessor scroll-y-position)))

(defmethod handle-repaint ((pane zoom-pane) region)
  (with-sheet-medium (medium pane)
    (climi::letf (((medium-clipping-region medium) region))
      (with-bounding-rectangle* (x1 y1 x2 y2) (medium-sheet medium)
        (medium-clear-area medium x1 y1 x2 y2)
        (with-drawing-options (medium :transformation (make-translation-transformation
                                                       (- (scroll-x-position pane))
                                                       (- (scroll-y-position pane))))
          (draw-rectangle* pane
                           (* 100 (zoom-x-level pane)) (* 80 (zoom-y-level pane))
                           (* 150 (zoom-x-level pane)) (* 120 (zoom-y-level pane))
                           :filled t :ink +orange+)
          (draw-text* pane "This text moves when you zoom!" (* 100 (zoom-x-level pane)) (* 100 (zoom-y-level pane)))
          (let ((x-grid-width 2)
                (y-grid-width 2)
                (x-grid-color +red+)
                (y-grid-color +blue+))
            (let ;; take width away from both x2 and y2 so that we draw inside the
                ;; bounding rectangle
                ((grid-x2 (- x2 x-grid-width))
                 (grid-y2 (- y2 y-grid-width)))
              (let ((x-divisions (floor (/ 20 (zoom-x-level pane) )))
                    (y-divisions (floor (/ 20 (zoom-y-level pane) )))
                    (medium-width (- grid-x2 x1))
                    (medium-height (- grid-y2 y1)))
                (when (and (> grid-x2 x1)
                           (> grid-y2 y1))
                  (loop for i from x1 to grid-x2 by (/ medium-width x-divisions)
                     do
                       (draw-line* medium i y1 i grid-y2 :line-thickness y-grid-width :ink y-grid-color))
                  (loop for i from y1 to grid-y2 by (/ medium-height y-divisions)
                     do
                       (draw-line* medium x1 i grid-x2 i :line-thickness x-grid-width :ink x-grid-color))
                  (draw-rectangle* pane
                                   (* 2 (/ medium-width x-divisions)) (* 2 (/ medium-height y-divisions))
                                   (* 4 (/ medium-width x-divisions)) (* 4 (/ medium-height y-divisions))
                                   :filled t :ink +blue+)
                  (draw-text* pane "This text is in box 3, 3!"
                              (* 3 (/ medium-width x-divisions))
                              (* 3 (/ medium-height y-divisions))))))))))
    (call-next-method)))

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

(defun scroll-x-callback (gadget pos)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'app)))
      (setf (scroll-x-position pane) pos)
      (repaint-sheet pane +everywhere+))))

(defun scroll-y-callback (gadget pos)
  (let ((frame (pane-frame gadget)))
    (let ((pane (find-pane-named frame 'app)))
      (setf (scroll-y-position pane) pos)
      (repaint-sheet pane +everywhere+))))

(defclass redisplay-event (clim:window-event) ())

(defmethod queue-redisplay ((pane zoom-pane))
  (queue-event pane (make-instance 'redisplay-event :sheet pane)))

(defmethod handle-event ((pane zoom-pane)
                         (event redisplay-event))
  (handle-repaint pane +everywhere+))

(define-application-frame zoom-viewer-2-app ()
  ()
  (:panes
   (app zoom-pane
        :height 512 :width 1024
        :display-time t
        :display-function 'display-zoom-viewer-2)
   (scroll-y :scroll-bar
             :width 16
             :min-width 16
             :max-width 16
             :min-value -500
             :max-value 500
             :value 0
             :orientation :vertical
             :drag-callback 'scroll-y-callback
             :value-changed-callback 'scroll-y-callback)
   (scroll-x :scroll-bar
             :height 16
             :min-height 16
             :max-height 16
             :min-value -500
             :max-value 500
             :value 0
             :orientation :horizontal
             :drag-callback 'scroll-x-callback
             :value-changed-callback 'scroll-x-callback)
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
              (vertically ()
                (horizontally ()
                  app
                  scroll-y)
                scroll-x)
              (labelling (:label "Zoom X")
                zoom-x)
              (labelling (:label "Zoom Y")
                zoom-y)
              int))))

(defclass some-text () ())

(define-presentation-method present (some-text (type some-text) pane view &key)
  (draw-rectangle* pane 20 30 300 80 :filled t :ink +green+)
  (draw-text* pane "This is some nice text, drawn normally!" 20 50))

(defparameter *some-text* (make-instance 'some-text))
(defun display-zoom-viewer-2 (frame pane)
  (declare (ignore frame pane))
  (present *some-text* (class-of *some-text*) :single-box t))

(defun zoom-viewer-2-main ()
  (let ((frame (make-application-frame 'zoom-viewer-2-app)))
    (values frame
            (bt:make-thread
             (lambda ()
               (run-frame-top-level frame))))))

(defclass zoom-viewer-2-top-level-sheet-pane (climi::top-level-sheet-pane) ())

(defmethod find-pane-for-frame ((fm frame-manager) (frame zoom-viewer-2-app))
  (make-pane-1 fm frame 'zoom-viewer-2-top-level-sheet-pane
               :name (frame-name frame)
               :pretty-name (frame-pretty-name frame)
               ;; sheet is enabled from enable-frame
               :enabled-p nil))

(defmethod handle-event :after ((top-level-sheet-pane zoom-viewer-2-top-level-sheet-pane)
                                (event window-configuration-event))
  (let* ((frame (pane-frame top-level-sheet-pane))
         (pane (find-pane-named frame 'app)))
    (when pane
      (queue-redisplay pane))))


