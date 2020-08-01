;; -*- lexical-binding: t;-*-

(defun bno/write-down ()
  "Open processing to write down a thought, task, or whatever else."
  (interactive)
  (find-file "/home/bruno/Notebooks/Nonzim/management/processing.org"))

(global-set-key (kbd "C-c b w") 'bno/write-down)

(defun bno/paste-link ()
  "Given a link that is pasted in the clipboard, paste it in the proper format at the end of the line."
  (interactive)
  (end-of-line)
  (insert "{{\s")
  (yank)
  (insert"\s}}"))

(defun bno/copy-link ()
  "Copy the next link following point.

Link must be in proper format. That is, enclosed in {{ }}"
  (interactive)
  (let (link-start link-end original)
    (setq original (point))
    (search-forward "{{" nil t)
    (forward-char)
    (setq link-start (point))
    (search-forward "}}" nil t)
    (setq link-end (- (point) 3))
    (copy-region-as-kill link-start link-end)
    (goto-char original))
  )


(defun bno/tab-indent ()
  "Insert two spaces as indentation."
  (interactive)
  (insert "  "))

(defun bno/new-block ()
  "Create a new block."
  (interactive)
  (end-of-line)
  (newline-and-indent)
  (bno/tab-indent)
  (insert "{")
  (newline-and-indent)
  (bno/tab-indent)
  (insert "-\s")
  (newline-and-indent)
  (delete-char -2)
  (insert "}")
  (forward-line -1)
  (end-of-line))

(defun bno/moveto (dest)
  "Move whatever is in the current region into DEST which can have one of the values specified below.

DEST are places in management where things frequently get moved.
Valid values are: trash.md, next.md, maybe.md, waiting.md, completed.org"
  (interactive "sMove to(default: completed.org):")
  (when (equal dest 'nil)
    (setq dest "completed.org"))
  (call-process-region
   (condition-case nil
       (region-beginning)
     (error (message "An error occurred regarding regions.")))
   (condition-case nil
       (region-end)
     (error (message "An error occurred regarding regions.")))
   "moveto"
   t nil nil dest)
  (delete-blank-lines))

(defvar keybindings '(
                      ("C-c b m" . bno/moveto)
                      ("C-c b n" . bno/new-block)
                      ("C-c b c" . bno/copy-link)
                      ("C-c b p" . bno/paste-link)
                      ))

(defvar bno/keymap (make-sparse-keymap))

(defun bind-keys-to-keymap ()
  (dolist (binding keybindings nil)
    (define-key bno/keymap (kbd (car binding)) (cdr binding)))
  )


(bind-keys-to-keymap)

(defgroup bno-notes nil
  "Customizing Bruno Notes.")

(define-minor-mode bno-notes-mode
  "Mode for structured note taking."
  :group 'bno-notes
  :keymap bno/keymap)

(defun bno/search-notes (regex)
  "Search all notes for a specified REGEX. REGEX is internally passed to grep. Matching files are returned."
  (interactive "sRegular expression: ")
  (let (search-buffer-name buffer-exists)
    (setq search-buffer-name "*bno-search-buffer*")
    (if (get-buffer search-buffer-name) (setq buffer-exists t)
      (setq buffer-exists nil)
      )
    (switch-to-buffer search-buffer-name)
    (when buffer-exists
      (erase-buffer))
    (call-process "node" nil t nil "/home/bruno/emacs-external/bin/bno-search" regex)))

(defun bno/book-index ()
  "Generate an index of all books that are mentioned."
  (interactive)
  (let (contents-buffer-name buffer-exists)
    (setq contents-buffer-name "*bno-book-index-buffer*")
    (if (get-buffer contents-buffer-name) (setq buffer-exists t)
      (setq buffer-exists nil)
      )
    (switch-to-buffer contents-buffer-name)
    (when buffer-exists
      (erase-buffer)
      )
    (call-process "node" nil t nil "/home/bruno/emacs-external/bin/bno-book-index"))
  )

(defun bno/review (date)
  "Generate a buffer with the concatenated contents of all notes from DATE, or yesterday by default."
  (interactive "sDate in the format Y/m/d (yesterday is default):")
  (let (contents-buffer-name buffer-exists)
    (setq contents-buffer-name "*bno-review-buffer*")
    (if (get-buffer contents-buffer-name) (setq buffer-exists t)
      (setq buffer-exists nil)
      )
    (switch-to-buffer contents-buffer-name)
    (when buffer-exists
      (erase-buffer)
      )
    (call-process "node" nil t nil "/home/bruno/emacs-external/bin/bno-review" date))
  )

(defun bno/search-goto ()
  "Go to file specified by the current line of the point."
  (interactive)
  (let (line-contents line-contents-cleared)
    (setq line-contents (thing-at-point 'line))
    (setq line-contents-cleared (seq-take line-contents (1- (length line-contents))))
    (find-file line-contents-cleared))
  )

(provide 'bno/notes)
