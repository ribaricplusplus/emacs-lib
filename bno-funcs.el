(defun bno/questions ()
    "Insert questions for measuring performance.
Questions are defined in an array within this function."
  (interactive)
  (let (questions)
    (setq questions '("What is the desired length of the next cycle?"
                      "What do you want to be doing in the next cycle?"
                      "What did you do in the previous cycle?"
                      "What percentage of activities in the previous cycle aligned with the plan?"
                      "Describe your bodily sensations."
                      )
          )
    (seq-do (lambda (x)
              (insert x)
              (insert "\n\n")) questions)
    (insert "==================================================\n"))
  )

(provide 'bno-funcs)
