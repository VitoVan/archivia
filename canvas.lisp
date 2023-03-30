(in-package #:calm)
(u:load-from-app "src/config.lisp")
(u:load-from-app "src/utils.lisp")
(u:load-from-app "src/file-renderer.lisp")
(u:load-from-app "src/events.lisp")

(defun draw-list (dir-list x width &key selected)
  (let* ((current-position (* *item-height* (1+ (or (position selected dir-list) 0))))
         (current-margin-bottom (- *calm-window-height* current-position)))
      (if (< (- *cursor-margin-bottom* current-margin-bottom) 0)
          (c:move-to x *margin-top*)
          (c:move-to x (- *margin-top* (- *cursor-margin-bottom* current-margin-bottom)))))

  (u:with-cairo-state
    (c:rel-move-to *item-margin-left* *item-margin-top*)
    (loop for dir in dir-list
          for y = (nth-value 1 (c:get-current-point))
          for highlight = (get-pathname-str selected)
          do
             (when
                 (and (>= y *item-margin-top*)
                      (<= y (- *calm-window-height* *margin-top* *margin-bottom* *item-margin-top* *item-margin-bottom*)))
               (u:show-markup
                (if (string= highlight (get-pathname-str dir))
                    (str:concat
                     "<span fgcolor='#2052BB' weight='SemiBold'>"
                     (get-pathname-str dir)
                     "</span>")
                    (get-pathname-str dir))
                :width (- width *item-margin-right*)
                :height -1)
               )
          (c:rel-move-to 0 *item-height*))))



(defun draw-column-bg (x width &key bg-color)
  (u:with-cairo-state
    (apply #'c:set-source-rgb (or bg-color *column-bg-color*))
    (u:rrectangle x *margin-top* width (- *calm-window-height* *margin-bottom* *margin-top*))
    (c:fill-path)))



(defun draw-file (filename x width)
  ;; draw filename
  (let* ((filename-str (get-pathname-str filename)))
    (c:set-source-rgb 1 1 1)
    (multiple-value-bind (layout w h)
        (u:create-markup-layout
         (format nil "<span bgcolor='~A'> ~A </span>" *filename-bg-color* filename-str)
         :width -1 :height -1 :align :center)
      (c:move-to
       (- *calm-window-width* *margin-right* w)
       (+ *margin-top*))
      (u:show-layout layout)



      ;; draw png
      (when (string= (pathname-type filename) "png")
        (draw-png filename
                  (+ x *item-margin-left*)
                  (+ *margin-top* *item-margin-top* h)
                  (- width *item-margin-right* *margin-right*)
                  (- *calm-window-height* *margin-top* *item-margin-top* *margin-bottom* *item-margin-bottom* h 10))
        (return-from draw-file))

      ;; draw file content
      (apply #'c:set-source-rgb *fg-color*)
      (c:move-to (+ x *item-margin-left*) (+ *margin-top* *item-margin-top* h))
      (u:show-markup
       (get-markup filename)
       :width (- width *item-margin-right* *margin-right*)
       :height
       (- *calm-window-height*
          *margin-top* *margin-bottom* *item-margin-top* *item-margin-bottom* *calm-default-font-size*)))))

(defun draw-searching ()
  (u:with-cairo-state
    (apply #'c:set-source-rgb *search-bg-color*)
    (u:rrectangle
     *margin-left*
     *search-y*
     (- *calm-window-width* *margin-left* *margin-right*)
     *search-height*
     :radius 10)
    (c:fill-path)
    (c:move-to *margin-left* *search-y*)
    (u:show-markup (format nil "<span fgcolor='#eafdff' weight='bold'> Search: ~A</span> " (or *search-string* ""))))
  )

(defun draw ()
  (unless *current-selected*
    (setf *current-selected* (uiop:getcwd)))

  (when (and *searching* *siblings* (not (str:emptyp *search-string*)))
    (let ((matched
            (find *search-string* *siblings* :test #'(lambda (x y) (str:containsp (str:downcase x) (str:downcase (get-pathname-str y)))))))
      (when matched
        (setf *current-selected* matched))))

  (apply #'c:set-source-rgb *bg-color*)
  (c:paint)
  (apply #'c:set-source-rgb *fg-color*)

  (let* ((parent
           (if (uiop:directory-pathname-p *current-selected*)
               (uiop:pathname-parent-directory-pathname *current-selected*)
               (uiop:pathname-directory-pathname *current-selected*)))
         (grand-parent (uiop:pathname-parent-directory-pathname parent))
         (siblings (or *siblings*
                       (append
                        (uiop:directory-files parent)
                        (uiop:subdirectories parent))))
         (children
           (or *children*
               (when (uiop:directory-pathname-p *current-selected*)
                 (append
                  (uiop:directory-files *current-selected*)
                  (uiop:subdirectories *current-selected*)))))
         (elders (or *elders*
                     (append
                      (uiop:directory-files grand-parent)
                      (uiop:subdirectories grand-parent)))))

    (setf *elders* elders
          *children* children
          *siblings* siblings)

    (draw-column-bg *elders-x* *elders-width*)

    (draw-column-bg *siblings-x* *siblings-width*)

    (draw-column-bg *children-x* *children-width*)

    (apply #'c:set-source-rgb *fg-color*)

    (draw-list elders *elders-x* *elders-width* :selected (get-parent *current-selected*))
    (draw-list siblings *siblings-x* *siblings-width*  :selected *current-selected*)

    (if children
        (draw-list children *children-x* *children-width*)
        (when (not (uiop:directory-pathname-p *current-selected*))
          (draw-file *current-selected* *children-x* *children-width*))))

  (when *searching* (draw-searching)))