(in-package #:calm)
(u:load-from-app "src/config.lisp")

(defun get-pathname-str (pathname)
  (if (uiop:directory-pathname-p pathname)
      (first (last (pathname-directory pathname)))
      (pathname-name pathname)))

(defparameter *cur-selected-index* nil)
(defparameter *cwd* nil)
(defparameter *cur-list* nil)
(defparameter *pre-list* nil)
(defparameter *nxt-list* nil)
(defparameter *cur-pathname-list* nil)
(defparameter *pre-pathname-list* nil)
(defparameter *nxt-pathname-list* nil)

(defun show-list (dir-list &optional cur-selected)
  (loop for dir in dir-list
        do
           (u:show-markup
            (if (string= cur-selected (get-pathname-str dir))
                (str:concat "<span foreground='red'>" (get-pathname-str dir) "</span>")
                (get-pathname-str dir)))
           (c:rel-move-to 0 14)
        ))

(defun on-keyup (key)
  (cond
    ((eq key :SCANCODE-J)
     (incf *cur-selected-index*)
     (let ((selected-pathname (nth *cur-selected-index* *cur-pathname-list*)))
       (when (uiop:directory-pathname-p selected-pathname)
         (setf *cwd* selected-pathname)))     
     )
    ((eq key :SCANCODE-K) (decf *cur-selected-index*)
     (let ((selected-pathname (nth *cur-selected-index* *cur-pathname-list*)))
       (when (uiop:directory-pathname-p selected-pathname)
         (setf *cwd* selected-pathname))))

    ((eq key :SCANCODE-H)
     (setf *cwd* (uiop:pathname-parent-directory-pathname *cwd*))
     )

    ((eq key :SCANCODE-L)
     (let* ((children-dirs (uiop:subdirectories *cwd*))
            (dist-dir (first children-dirs)))
       (if dist-dir
           (setf *cwd* dist-dir)
           (setf *cwd* (merge-pathnames "no-such-folder/" *cwd*))))
     ))
  (format t "~A~%" key))

(defun draw ()
  (unless *cwd*
    (setf *cwd* (uiop:getcwd)))
  (c:set-source-rgb (/ 12 255) (/ 55 255) (/ 132 255))
  (c:paint)
  (c:set-source-rgb 1 1 1)
  (c:move-to 50 10)
  (let* ((cwd-parent (uiop:pathname-parent-directory-pathname *cwd*))
         (cwd-grand-parent (uiop:pathname-parent-directory-pathname cwd-parent))
         (dirs (pathname-directory *cwd*))
         (cur-dir (first (last dirs)))
         (pre-dir (first (last dirs 2)))
         )

    (setf
     *cur-pathname-list*
     (append
      (uiop:directory-files cwd-parent)
      (uiop:subdirectories cwd-parent))
     *cur-list*
     (mapcar #'get-pathname-str *cur-pathname-list*)

     *pre-pathname-list*
     (append
      (uiop:directory-files cwd-grand-parent)
      (uiop:subdirectories cwd-grand-parent))
     *pre-list*
     (mapcar #'get-pathname-str *pre-pathname-list*)

     *nxt-pathname-list*
     (append
      (uiop:directory-files *cwd*)
      (uiop:subdirectories *cwd*))
     *nxt-list*
     (mapcar #'get-pathname-str *nxt-pathname-list*)

     *cur-selected-index*
     (or *cur-selected-index*
         (position cur-dir *cur-list* :test #'string=)))


    (u:show-markup
     (str:concat pre-dir " /  " cur-dir)
     :font-size 20)

    (c:move-to 40 40)
    (show-list *pre-list*)

    (c:move-to 240 40)
    (show-list *cur-list* (nth *cur-selected-index* *cur-list*))

    (c:move-to 400 40)
    (show-list *nxt-list*)

    )

  )
