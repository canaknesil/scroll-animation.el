;;
;; Global variables
;;

(setq animation-refresh-period 0.01)
(setq scroll-animation-screen-ratio 0.75)
(setq scroll-animation-sec 0.2)

;;
;; Utility
;;

(defmacro measure-time (&rest body)
  `(let ((past (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since past)))))

;;
;; Animation
;;

;; Constant speed
;; (defun location (time)
;;   "location: [0,1] -> [0,1]"
;;   (float time))

;; Continuous speed function
(defun location (time)
  (let ((x (float time)))
    (if (< x 0.5)
	(* 2.0 x x)
      (- (* 4.0 x) (* 2.0 x x) 1))))

(defun animate (func n sec)
  "Call func n times, animate through sec seconds."
  (let* ((N (floor (/ (float sec) animation-refresh-period)))
	 (P (/ (float sec) N))
	 (curr-loc 0)
	 (val))
    (dotimes (i N val)
      (if (not (= i 0))
	  (sit-for P))
      (let* ((new-loc (* (location (/ (float (+ i 1)) N)) n))
	     (diff (floor (- new-loc curr-loc))))
	(dotimes (j diff val)
	  (funcall func))
	(setq curr-loc (+ curr-loc diff))))))
	
;;
;; Scroll
;;

(defun scroll-animation-get-step ()
  (* (window-body-height) scroll-animation-screen-ratio))

(defun current-line ()
  (+ 1 (count-lines 1 (window-start))))

(defun end-line ()
  (count-lines 1 (buffer-end 1)))

(defun scroll-animation-up ()
  (interactive)
  (let* ((step (scroll-animation-get-step))
	 (line (current-line))
	 (end-line (end-line))
	 (rest (- end-line line))
	 (animation-time scroll-animation-sec))
    (if (= rest 0)
	(message "End of the buffer.") 
      (progn
	(when (< rest step)
	  (setq animation-time (* animation-time (/ (float rest) step)))
	  (setq step rest))
	(animate 'scroll-up-line step animation-time)))))

(defun scroll-animation-down ()
  (interactive)
  (let* ((step (scroll-animation-get-step))
	 (line (current-line))
	 (animation-time scroll-animation-sec))
    (if (= line 1)
	(message "Beginning of the buffer.") 
      (progn
	(when (< (- line 1) step)
	  (setq animation-time (* animation-time (/ (float (- line 1)) step)))
	  (setq step (- line 1)))
	(animate 'scroll-down-line step animation-time)))))

(global-set-key (kbd "C-v") 'scroll-animation-up)
(global-set-key (kbd "M-v") 'scroll-animation-down)
  
