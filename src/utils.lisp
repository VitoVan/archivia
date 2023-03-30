(in-package #:calm)

(defun get-pathname-str (pathname)
  (when pathname
    (if (uiop:directory-pathname-p pathname)
        (first (last (pathname-directory pathname)))
        (file-namestring pathname))))

(defun get-parent (pathname)
  (when pathname
    (if (uiop:directory-pathname-p pathname)
        (uiop:pathname-parent-directory-pathname pathname)
        (uiop:pathname-directory-pathname pathname))))

(defun os-open (pathname)
  (ignore-errors
   (uiop:run-program
    #+darwin
    (str:concat "open " (uiop:native-namestring pathname))
    #+linux
    (str:concat "xdg-open " (uiop:native-namestring pathname))
    #+win32
    (str:concat "explorer " (uiop:native-namestring pathname)))))

(defun get-file (filename)
  (or
   (ignore-errors
    (str:unlines
     (with-open-file (stream filename)
       (loop for line = (read-line stream nil)
             for i = 1 then (incf i)
             while (and line (< i *file-preview-lines*))
             collect line))))
   "Empty or invalid text file."))

(defun get-markup (filename)
  (let* ((markup-cache
           (when *file-markup-cache* (cdr (assoc filename *file-markup-cache*))))
        (markup
          (or
           markup-cache
           (with-input-from-string (s (get-file filename))
             (uiop:run-program
              (format nil "pygmentize -O style=xcode -f pango -l ~A"
                      (uiop:run-program (str:concat "pygmentize -N \"" (str:replace-all "#" "" (get-pathname-str filename)) "\"")
                                        :output 'string))
              :input s :output 'string)))))
    (unless markup-cache
      (setf *file-markup-cache* (acons filename markup *file-markup-cache*)))
    markup))

(defun pre-load-child-markup (current)
  (let ((children
          (when (uiop:directory-pathname-p current)
            (append
             (uiop:directory-files current)
             (uiop:subdirectories current)))))
    (when (and children (not (uiop:directory-pathname-p (first children))))
      (bt:make-thread (lambda () (get-markup (first children)))))))

(defun pre-load-siblings-markup (current)
  (let* ((parent (get-parent current))
         (siblings (uiop:directory-files parent))
         (i (position current siblings))
         (elder-sibling-1 (nth (1+ (or i 0)) siblings))
         (elder-sibling-2 (nth (+ 2 (or i 0)) siblings))
         (younger-sibling (nth (max (1- (or i 0)) 0) siblings)))
    (labels ((pre-markup (x)
               (when (and x (not (uiop:directory-pathname-p x)))
                 (bt:make-thread (lambda () (get-markup x))))))
      (pre-markup elder-sibling-1)
      (pre-markup elder-sibling-2)
      (pre-markup younger-sibling))))

(defun pre-load-markup (current)
  (pre-load-child-markup current)
  (pre-load-siblings-markup current))
