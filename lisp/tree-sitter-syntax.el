;; -*- lexical-binding: t -*-
(require 'ht)

(defconst ts-face-by-node-type
  (ht<-alist '(("import" . font-lock-keyword-face)
               ("with" . font-lock-keyword-face)
               ("as" . font-lock-keyword-face)
               ("class" . font-lock-keyword-face)
               ("def" . font-lock-keyword-face)
               ("pass" . font-lock-keyword-face)
               ("comment" . font-lock-comment-face)
               ("string" . font-lock-string-face)
               ("call" . hl-call)
               ("none" . font-lock-constant-face)
               ("identifier" . ts-hl-identifier)
               ("true" . font-lock-constant-face)
               ("false" . font-lock-constant-face)
               ("integer" . highlight-numbers-number)
               ("function_definition" . hl-fn-def)
               ("class_definition" . hl-class-def))))

(defconst ts-builtins
  (ht<-alist '(("open" . nil)
               ("getattr" . nil)
               ("print" . nil))))

(defun ts-hl-identifier (node)
  (ts-add-face-weighted node 'font-lock-variable-name-face 11))



(defun ts-add-face-weighted (node face weight)
  (let* ((start (tree-sitter-node-start-byte node))
         (end (tree-sitter-node-end-byte node))
         (current-weight (get-text-property start 'face-weight)))
    (unless (and current-weight (> current-weight weight))
       (add-face-text-property start end face)
       (put-text-property start end 'face-weight weight))))

(defun ts-get-node-text (node)
  (let ((start (tree-sitter-node-start-byte node))
        (end (tree-sitter-node-end-byte node)))
    (buffer-substring-no-properties start end)))

(defun hl-call (node)
  (let* ((child (tree-sitter-node-child node 0))
         (child-type (tree-sitter-node-type child)))
    (when (and (string= "identifier" child-type)
               (ht-contains-p ts-builtins (ts-get-node-text child)))
      (let ((start (tree-sitter-node-start-byte child))
            (end (tree-sitter-node-end-byte child)))
          (add-face-text-property start end 'font-lock-builtin-face)))))

(defun hl-fn-def (node)
  ;;; TODO: Add children lenth checks
  (let* ((child (tree-sitter-node-child node 1))
         (child-type (tree-sitter-node-type child)))
    (when (string= "identifier" child-type)
      (let ((start (tree-sitter-node-start-byte child))
            (end (tree-sitter-node-end-byte child)))
        (add-face-text-property start end 'font-lock-function-name-face)))))

(defun hl-class-def (node)
  ;;; TODO: Add children lenth checks
  (let* ((child (tree-sitter-node-child node 1))
         (child-type (tree-sitter-node-type child)))
    (when (string= "identifier" child-type)
        (ts-add-face-weighted child 'font-lock-type-face 10))))

(defun print-node (node)
  (message "%s %s %d %d"
           (tree-sitter-node-type node)
           (tree-sitter-node-named-p node)
           (tree-sitter-node-start-byte node)
           (tree-sitter-node-end-byte node)))


(defun walk-tree (node cb)
  (funcall cb node)
  (when (/= 0 (tree-sitter-node-child-count node))
    (dotimes (idx (tree-sitter-node-child-count node))
      (walk-tree (tree-sitter-node-child node idx) cb))))

(defun tree-sitter-syntax-is-keyword-p (node)
  (null (tree-sitter-node-named-p node)))

(defun hl-node (node)
  (let ((face-or-fn (ht-get ts-face-by-node-type (tree-sitter-node-type node))))
    (when face-or-fn
      (let ((start (tree-sitter-node-start-byte node))
            (end (tree-sitter-node-end-byte node)))
        (message "Node type %s" (tree-sitter-node-type node))
        ;(remove-text-properties start end '(face nil))
        (if (functionp face-or-fn)
          (funcall face-or-fn node)
          (ts-add-face-weighted node face-or-fn 10))))))

(defun tree-sitter-syntax-hl-buffer ()
  (interactive)
  (let ((parser (tree-sitter-parser-new))
        (lang (tree-sitter-lang-python)))
    (tree-sitter-parser-set-language parser lang)
    (let ((tree (tree-sitter-parser-parse-buffer parser (current-buffer))))
      (let ((root (tree-sitter-tree-root-node tree)))
        (walk-tree root 'hl-node)))))

(defun ts-get-face-weight ()
  (get-text-property (point) 'face-weight))
(defun ts-inspect-face ()
   (interactive)
   (message "Face: %s Weight: %s" (get-text-property (point) 'face) (ts-get-face-weight)))
(provide 'tree-sitter-syntax)

(defun insert-file-name ()
  "Insert the full path file name into the current buffer."
  (interactive))

;; (file-notify-add-watch "/home/novikov/projects/tree-sitter-syntax.el/tree-sitter-syntax.el" '(change)
;;                        (lambda (event)
;;                          (load "/home/novikov/projects/tree-sitter-syntax.el/tree-sitter-syntax.el")
;;                          (with-current-buffer "test.py"
;;                            (tree-sitter-syntax-hl-buffer))
;;                          (message "Event %S" event)))

