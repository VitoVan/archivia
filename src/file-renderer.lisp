(in-package #:calm)

(defun draw-png (filename x y width height)
  (c:with-state
    (let* ((surface (c:image-surface-create-from-png filename))
           (img-width (c:image-surface-get-width surface))
           (z (/ width img-width)))
      (c:move-to x y)
      (c:rectangle x y width height)
      (when (<= z 1)
        ;; (format t "~A~%" z)
        (c:scale z z)
        (c:translate (* (/ (- 1 z) z) x) (* (/ (- 1 z) z) y)))
      (c:set-source-surface surface x y)
      
      (c:fill-path))))
