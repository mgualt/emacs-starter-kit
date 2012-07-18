;; outline-mode-easy-bindings.el
;;
;; You can control outline entirely with Meta+<cursor> keys
;;
;; Store this file as outline-mode-easy-bindings.el somewhere in your
;; load-path and add the following lines to your init file:
;;
;;      (add-hook 'outline-mode-hook
;;                '(lambda ()
;;                   (require 'outline-mode-easy-bindings)))
;;
;;      (add-hook 'outline-minor-mode-hook
;;                '(lambda ()
;;                   (require 'outline-mode-easy-bindings)))

(defun outline-body-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (and (not (eobp))
         (progn (forward-char 1)
                (not (outline-on-heading-p))))))

(defun outline-body-visible-p ()
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (not (outline-invisible-p))))

(defun outline-subheadings-p ()
  (save-excursion
    (outline-back-to-heading)
    (let ((level (funcall outline-level)))
      (outline-next-heading)
      (and (not (eobp))
           (< level (funcall outline-level))))))

(defun outline-subheadings-visible-p ()
  (interactive)
  (save-excursion
    (outline-next-heading)
    (not (outline-invisible-p))))

(defun outline-do-close ()
  (interactive)
  (if (outline-on-heading-p)
      (cond ((and (outline-body-p)
                  (outline-body-visible-p))
             (hide-entry)
             (hide-leaves))
            (t
             (hide-subtree)))))

(defun outline-do-open ()
  (interactive)
  (if (outline-on-heading-p)
      (cond ((and (outline-subheadings-p)
                  (not (outline-subheadings-visible-p)))
             (show-children))
            ((and (not (outline-subheadings-p))
                  (not (outline-body-visible-p)))
             (show-subtree))
            ((and (outline-body-p)
                  (not (outline-body-visible-p)))
             (show-entry))
            (t
             (show-subtree)))))

(define-key outline-mode-map (kbd "M-<left>") 'outline-do-close)
(define-key outline-mode-map (kbd "M-<right>") 'outline-do-open)
(define-key outline-mode-map (kbd "M-<up>") 'outline-previous-visible-heading)
(define-key outline-mode-map (kbd "M-<down>") 'outline-next-visible-heading)

(define-key outline-minor-mode-map (kbd "M-<left>") 'outline-do-close)
(define-key outline-minor-mode-map (kbd "M-<right>") 'outline-do-open)
(define-key outline-minor-mode-map (kbd "M-<up>") 'outline-previous-visible-heading)
(define-key outline-minor-mode-map (kbd "M-<down>") 'outline-next-visible-heading)

(provide 'outline-mode-easy-bindings)

