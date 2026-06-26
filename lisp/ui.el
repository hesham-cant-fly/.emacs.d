;;; -*- lexical-binding: t -*-
(setq lexical-binding t)

;; (use-package vui
;;   :ensure t)

;; (require 'vui)

(defun curry (fun &rest args)
  "Return a function that partially or fully applies FUN to ARGS."
  (lambda (&rest more-args)
    (apply fun (append args more-args))))

;; (vui-defcomponent todo-item (content)
;;   :render
;;   (vui-fragment
;;    (vui-newline)
;;    (vui-text (format " * %s" content))))

;; (vui-defcomponent todo-list (title)
;;   :state ((expanded  nil)
;;           (the-list  (list)))
;;   :render
;;   (vui-fragment
;;    (vui-button (if expanded "*" ">")
;;      :on-click (lambda () (vui-set-state :expanded #'not)))
;;    (vui-text (format " %s" title))
;;    (when expanded
;;      (vui-fragment
;;       (vui-newline)
;;       (vui-text "New Todo: ")
;;       (vui-field :size 25
;;                  :on-submit (lambda (value)
;;                               (vui-set-state :the-list
;;                                              (curry #'cons (s-trim value)))))
;;      (apply #'vui-fragment
;;             (mapcar (curry #'vui-component 'todo-item :content) the-list))))))

;; (vui-defcomponent app ()
;;   :render
;;   (vui-vstack
;;    (vui-component 'todo-list
;;      :title "Todo List")))

;; (vui-mount (vui-component 'app) "*app*")
