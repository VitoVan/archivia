(in-package #:calm)

(defun on-windowresized (width height)
  (setf *calm-window-width* width
        *calm-window-height* height)
  (config))

(defun on-textinput (text)
  (setf *minibar-string* (str:concat (or *minibar-string* "") text)))

(defun on-keyup (key)
  (cond
    ;;
    ;; search
    ;;
    ;; start search
    ((and (eq key :SCANCODE-SLASH) (not *minibar-typing*))
     (setf *minibar-typing* t
           *searching* t
           *minibar-string* nil)
     (sdl2:start-text-input))
    ;;
    ;; goto a place
    ;;
    ((and (eq key :SCANCODE-G) (not *minibar-typing*))
     (setf *minibar-typing* t
           *going* t
           *minibar-string* nil)
     (sdl2:start-text-input))
    ;;
    ;; execute a command
    ;;
    ((and (eq key :SCANCODE-X) (not *minibar-typing*))
     (setf *minibar-typing* t
           *executing* t
           *minibar-string* nil)
     (sdl2:start-text-input))
    ;;
    ;; pre-load files
    ;;
    ((and (c:keq key
                 :SCANCODE-J
                 :SCANCODE-K
                 :SCANCODE-H
                 :SCANCODE-L
                 :SCANCODE-LEFT
                 :SCANCODE-RIGHT
                 :SCANCODE-RETURN)
          (not *minibar-typing*))
     (pre-load-markup *current-selected*))
    ;;
    ;; quit
    ;;
    ((and (eq key :SCANCODE-Q) (not *minibar-typing*))
     (uiop:quit))))

(defun on-keydown (key)
  (cond

    (*minibar-typing*
     (case key
       ;; delete minibar-string
       ((:scancode-backspace)
        (setf *minibar-string* (str:substring 0 -1 *minibar-string*)))

       ;; end minibar input
       ((:scancode-return :scancode-escape)
        (when (and *executing* (c:keq key :scancode-return))
          ;; if it's executing, if pressed return, execute it.
          (format t "EXEC: ~A ~A~%" *minibar-string* *current-selected*)
          (os-exec (format nil "~A ~A" *minibar-string* *current-selected*)))
       ;; reset `*going*', `*searching*', `*executing*'
        (setf *minibar-typing* nil
              *going* nil
              *searching* nil
              *executing* nil
              *minibar-string* nil)
        (sdl2:stop-text-input))))

    ((not *minibar-typing*)
     (case key
       ;;
       ;; file viewer
       ;;
       ((:scancode-v)
        (cond
          ((sdl2:mod-key-state-p :ctrl) (incf *file-viewer-scroll* 10))
          ((sdl2:mod-key-state-p :alt) (decf *file-viewer-scroll* 10)))
        (when (< *file-viewer-scroll* 0)
          (setf *file-viewer-scroll* 0)))
       ((:scancode-n)
        (when (sdl2:mod-key-state-p :ctrl)
          (incf *file-viewer-scroll* 1)))
       ((:scancode-p)
        (when (sdl2:mod-key-state-p :ctrl)
          (decf *file-viewer-scroll* 1)))
       ;;
       ;; movement
       ;;
       ((:scancode-j)
        (setf *file-viewer-scroll* 0)
        (let ((dist-dir (nth (1+ (or (position *current-selected* *siblings*) 0)) *siblings*)))
          (when dist-dir (setf *current-selected* dist-dir))))
       ((:scancode-k)
        (setf *file-viewer-scroll* 0)
        (let ((dist-dir (nth (max (1- (or (position *current-selected* *siblings*) 0)) 0) *siblings*)))
          (when dist-dir (setf *current-selected* dist-dir))))
       ((:scancode-h)
        (setf *current-selected* (get-parent *current-selected*)))
       ((:scancode-l)
        ;; cd to first child dir
        (when (and (uiop:directory-pathname-p *current-selected*) (first *children*))
          (setf *current-selected* (first *children*))))
       ;;
       ;; open file
       ;;
       ((:scancode-o)
        (os-open *current-selected*))
       ;;
       ;; clear cache
       ;;
       ((:scancode-c)
        (setf *file-markup-cache* nil))
       ;;
       ;; open file manager and switch to current dir
       ;;
       ((:scancode-f)
        (uiop:run-program (str:concat "open -a ForkLift " (uiop:native-namestring *current-selected*))))
       ;;
       ;; switch file preview encoding
       ;;
       ((:scancode-space)
        (incf *file-encoding-index*)
        (when (>= *file-encoding-index* (length *file-encoding-list*))
          (setf *file-encoding-index* 0))
        (format t "Using preview encoding: ~A~%" (nth *file-encoding-index* *file-encoding-list*))))))

  (setf *elders* nil
          *children* nil
          *siblings* nil)

  (when (> (length *file-markup-cache*) *file-markup-max-cache*)
      (nbutlast *file-markup-cache* 1))

  (format t "~A~%" key))
