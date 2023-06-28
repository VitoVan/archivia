(in-package #:calm)
;;
;; CALM version check
;;

(let ((required-version "0.0.40")
      (calm-version (slot-value (asdf:find-system 'calm) 'asdf:version)))
  (when (uiop:version< calm-version required-version)
    (format t "Sorry, this is built on CALM ~A, older version (current: ~A) of CALM won't work.~%" required-version calm-version)
    (uiop:quit 42)))

;;
;; the swank server is for debugging, for usage please check
;; Emacs:
;;        https://slime.common-lisp.dev/
;; Visual Studio Code
;;        https://lispcookbook.github.io/cl-cookbook/vscode-alive.html
;;
;; uncomment the following line to enable SWANK Server
(unless (str:starts-with? "dist" (uiop:getenv "CALM_CMD")) (swank:create-server))

;;
;; by default, the screensaver is disabled,
;; if you want to enable screensaver,
;; please uncomment the following line
;;
(setf (uiop:getenv "SDL_VIDEO_ALLOW_SCREENSAVER") "1")

;;
;; setting window properties, for more of this, please check
;;      https://github.com/VitoVan/calm/blob/main/src/config.lisp
;;
(setf *calm-window-width* 1024)
(setf *calm-window-height* 768)
(setf *calm-window-title* "Archivia")
(setf *calm-default-font-size* 14)
(setf *calm-default-font-family* "Open Sans")
(defparameter *calm-window-flags* '(:shown :allow-highdpi :resizable))


(defparameter *current-selected* nil)
(defparameter *file-encoding-list* '(:utf-8 :eucjp :gbk))
(defparameter *file-encoding-index* 0)
(defparameter *file-viewer-scroll* 0)
(defparameter *file-markup-cache* nil)
(defparameter *file-markup-max-cache* 42)
(defparameter *cursor-margin-bottom* 100)
(defparameter *margin-left* 10)
(defparameter *margin-right* 10)
(defparameter *margin-bottom* 10)
(defparameter *margin-top* 10)
(defparameter *column-gap* 10)
(defparameter *item-height* 20)
(defparameter *item-margin-left* 16)
(defparameter *item-margin-right* 20)
(defparameter *item-margin-top* 10)
(defparameter *item-margin-bottom* 4)

;; (defparameter *bg-color* (list (/ 45 255) (/ 62 255) (/ 80 255)))
;; (defparameter *bg-color* (list (/ 45 255) (/ 62 255) (/ 80 255)))
;; (defparameter *bg-color* (list (/ 35 255) (/ 52 255) (/ 70 255)))
;; (defparameter *bg-color* (list (/ 248 255) (/ 248 255) (/ 248 255)))
(defparameter *bg-color* '(0 0.35 0.59))
;; (defparameter *bg-color* (list (/ 5 255) (/ 34 255) (/ 62 255)))
;; (defparameter *column-bg-color* (list (/ 77 255) (/ 113 255) (/ 148 255)))
;; (defparameter *column-bg-color* (list (/ 52 255) (/ 101 255) (/ 164 255)))
;; (defparameter *column-bg-color* '(0 0.35 0.59))
(defparameter *column-bg-color* (list (/ 248 255) (/ 248 255) (/ 248 255)))
;; (defparameter *column-bg-color* '(0.94 0.87 0.47))
;; (defparameter *file-bg-color* (list (/ 212 255) (/ 209 255) (/ 215 255)))
;; (defparameter *file-bg-color* (list (/ 248 255) (/ 248 255) (/ 248 255)))
;; (defparameter *file-fg-color* (list 0 0 0))
(defparameter *filename-bg-color* "#2052BB")
(defparameter *file-preview-lines* 100)
(defparameter *fg-color* (list (/ 83 255) (/ 83 255) (/ 83 255)))
;; (defparameter *fg-color* '(0 0 0))

(defparameter *supported-file-type* '("lisp" "sh" "c" "md" "asd" "bat" "yml" "html" "conf"))
(defparameter *supported-file-name* '("LICENSE" ".gitignore" "Dockerfile"))

(defparameter *elders* nil)
(defparameter *siblings* nil)
(defparameter *children* nil)

(defparameter *elders-x* nil)
(defparameter *elders-width* nil)
(defparameter *siblings-x* nil)
(defparameter *siblings-width* nil)
(defparameter *children-x* nil)
(defparameter *children-width* nil)

(setf *calm-default-font-family* "Open Sans")
;; (setf *calm-default-font-family* "Monaco")

(defparameter *searching* nil "if the user has pressed `/' to search in current list")
(defparameter *going* nil "if the user has pressed `g' to go to a PATH")
(defparameter *executing* nil "if the use has pressed `x' to execute a command on the current file or dir")
(defparameter *minibar-typing* nil)
(defparameter *minibar-string* nil)
(defparameter *search-bg-color* (list (/ 5 255) (/ 10 255) (/ 17 255)))
(defparameter *search-height* 20)
(defparameter *search-y* nil)

;; recaculate positions and sizes
(defun config ()
  (setf *elders-x* *margin-left*
        *elders-width* (/ *calm-window-width* 6)
        *siblings-x* (+ *elders-x* *elders-width* *column-gap*)
        *siblings-width* (/ *calm-window-width* 3)
        *children-x* (+ *siblings-x* *siblings-width* *column-gap*)
        *children-width* (- *calm-window-width* *siblings-width* *elders-width* (* 2 *column-gap*) *margin-left* *margin-right*)
        *search-y* (- *calm-window-height* *search-height* 4)))

(config)
