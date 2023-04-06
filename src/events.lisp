(in-package #:calm)

(defun on-windowresized (width height)
  (setf *calm-window-width* width
        *calm-window-height* height)
  (config))

(defun on-textinput (text)
  (setf *search-string* (str:concat (or *search-string* "") text)))

(defun on-keyup (key)
  (cond
    ;;
    ;; search
    ;;
    ;; start search
    ((and (eq key :SCANCODE-SLASH) (not *searching*))
     (setf *searching* t
           *search-string* nil)
     (sdl2:start-text-input))
    ;;
    ;; pre-load files
    ;;
    ((and (member key
                  '(:SCANCODE-J
                   :SCANCODE-K
                   :SCANCODE-H
                   :SCANCODE-L
                   :SCANCODE-LEFT
                   :SCANCODE-RIGHT
                   :SCANCODE-RETURN))
          (not *searching*))
     (pre-load-markup *current-selected*))
    ;;
    ;; quit
    ;;
    ((and (eq key :SCANCODE-Q) (not *searching*))
     (uiop:quit))))

(defun on-keydown (key)
  (cond
    ;;
    ;; movement
    ;;
    ((and (or (eq key :SCANCODE-J) (eq key :SCANCODE-N)) (not *searching*))
     (let ((dist-dir (nth (1+ (or (position *current-selected* *siblings*) 0)) *siblings*)))
       (when dist-dir (setf *current-selected* dist-dir))))
    ((and (or (eq key :SCANCODE-K) (eq key :SCANCODE-P)) (not *searching*))
     (let ((dist-dir (nth (max (1- (or (position *current-selected* *siblings*) 0)) 0) *siblings*)))
       (when dist-dir (setf *current-selected* dist-dir))))
    ((and (eq key :SCANCODE-H) (not *searching*))
     (setf *current-selected* (get-parent *current-selected*)))
    ((and (eq key :SCANCODE-L) (not *searching*))
     ;; cd to first child dir
     (when (and (uiop:directory-pathname-p *current-selected*) (first *children*))
       (setf *current-selected* (first *children*))))
    ((and (eq key :SCANCODE-O) (not *searching*))
     (os-open *current-selected*))
    ((and (eq key :SCANCODE-F) (not *searching*))
     (uiop:run-program (str:concat "open -a ForkLift " (uiop:native-namestring *current-selected*))))
    ;;
    ;; search
    ;;
    ;; delete search string
    ((and (eq key :SCANCODE-BACKSPACE) *searching*)
     (setf *search-string* (str:substring 0 -1 *search-string*)))
    ;; end search
    ((or (eq key :SCANCODE-RETURN) (eq key :SCANCODE-ESCAPE))
     (setf *searching* nil
           *search-string* nil)
     (sdl2:stop-text-input)))

  (setf *elders* nil
          *children* nil
          *siblings* nil)

  (when (> (length *file-markup-cache*) *file-markup-max-cache*)
      (nbutlast *file-markup-cache* 1))

  (format t "~A~%" key))
