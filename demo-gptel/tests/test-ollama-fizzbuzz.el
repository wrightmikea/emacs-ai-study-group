;;; test-ollama-fizzbuzz.el --- Tests for AI-generated code corrections -*- lexical-binding: t; -*-

;; This file tests the corrections documented in the "Common AI Mistakes" section
;; of demo-gptel/docs/demo.org

;;; Commentary:
;;
;; Run these tests with:
;;   emacs -Q -batch -L . -l test-ollama-fizzbuzz.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET test-ollama-fizzbuzz.el RET
;;   M-x ert RET t RET

;;; Code:

(require 'ert)

;;;; Correct Implementations (From Documentation)

(defun fizzbuzz-correct (n)
  "Print FizzBuzz sequence up to N.
This is the CORRECT Emacs Lisp version."
  (let ((output '()))
    (dotimes (i n)
      (let ((num (1+ i)))
        (push (cond
               ((zerop (mod num 15)) "FizzBuzz")
               ((zerop (mod num 3))  "Fizz")
               ((zerop (mod num 5))  "Buzz")
               (t (number-to-string num)))
              output)))
    (nreverse output)))

(defun greet-names-correct ()
  "Print greetings for a list of names.
This is the CORRECT Emacs Lisp version using dolist."
  (let ((output '()))
    (dolist (name '("Alice" "Bob" "Charlie"))
      (push (format "Hello, %s!" name) output))
    (nreverse output)))

;;;; Wrong Implementations (Common AI Mistakes)

;; These demonstrate what AI often generates incorrectly

(defun fizzbuzz-wrong-format-t (n)
  "WRONG: Uses Common Lisp 'format t' which doesn't work in Emacs.
This is what AI mistakenly generates."
  ;; This would use (format t ...) which fails in Emacs
  ;; We can't even define it properly because it would error
  (error "This is Common Lisp syntax - format t doesn't exist in Emacs Lisp"))

(defun greet-names-wrong-dotimes ()
  "WRONG: Uses dotimes with a list instead of dolist.
This is what AI mistakenly generates."
  ;; This demonstrates the type error
  (condition-case err
      (let ((names '("Alice" "Bob" "Charlie")))
        ;; dotimes expects a NUMBER, not a list
        (dotimes (name names)
          (message "Hello, %s!" name)))
    (wrong-type-argument
     (format "Expected error: %s" err))))

;;;; Tests for Correct Implementations

(ert-deftest test-fizzbuzz-output-15 ()
  "Test FizzBuzz output for numbers 1-15."
  (let ((expected '("1" "2" "Fizz" "4" "Buzz" "Fizz" "7" "8" "Fizz" "Buzz"
                    "11" "Fizz" "13" "14" "FizzBuzz")))
    (should (equal (fizzbuzz-correct 15) expected))))

(ert-deftest test-fizzbuzz-divisible-by-3 ()
  "Test that numbers divisible by 3 return 'Fizz'."
  (let ((result (fizzbuzz-correct 9)))
    (should (equal (nth 2 result) "Fizz"))   ; 3
    (should (equal (nth 5 result) "Fizz"))   ; 6
    (should (equal (nth 8 result) "Fizz")))) ; 9

(ert-deftest test-fizzbuzz-divisible-by-5 ()
  "Test that numbers divisible by 5 return 'Buzz'."
  (let ((result (fizzbuzz-correct 10)))
    (should (equal (nth 4 result) "Buzz"))   ; 5
    (should (equal (nth 9 result) "Buzz")))) ; 10

(ert-deftest test-fizzbuzz-divisible-by-15 ()
  "Test that numbers divisible by both 3 and 5 return 'FizzBuzz'."
  (let ((result (fizzbuzz-correct 30)))
    (should (equal (nth 14 result) "FizzBuzz"))  ; 15
    (should (equal (nth 29 result) "FizzBuzz")))) ; 30

(ert-deftest test-fizzbuzz-regular-numbers ()
  "Test that non-divisible numbers return as strings."
  (let ((result (fizzbuzz-correct 8)))
    (should (equal (nth 0 result) "1"))
    (should (equal (nth 1 result) "2"))
    (should (equal (nth 3 result) "4"))
    (should (equal (nth 6 result) "7"))))

(ert-deftest test-fizzbuzz-empty ()
  "Test FizzBuzz with n=0 returns empty list."
  (should (equal (fizzbuzz-correct 0) '())))

(ert-deftest test-fizzbuzz-one ()
  "Test FizzBuzz with n=1 returns just '1'."
  (should (equal (fizzbuzz-correct 1) '("1"))))

(ert-deftest test-greet-names-output ()
  "Test that greet-names produces correct output."
  (let ((expected '("Hello, Alice!" "Hello, Bob!" "Hello, Charlie!")))
    (should (equal (greet-names-correct) expected))))

(ert-deftest test-greet-names-count ()
  "Test that greet-names produces correct number of greetings."
  (should (= (length (greet-names-correct)) 3)))

(ert-deftest test-greet-names-format ()
  "Test that greet-names uses correct format string."
  (let ((result (greet-names-correct)))
    (should (string-match-p "^Hello, .+!$" (car result)))))

;;;; Tests for Wrong Implementations (Should Fail)

(ert-deftest test-fizzbuzz-wrong-format-t-fails ()
  "Test that Common Lisp format t version throws an error.
This demonstrates that format t doesn't exist in Emacs Lisp."
  (should-error (fizzbuzz-wrong-format-t 5)
                :type 'error))

(ert-deftest test-greet-names-wrong-dotimes-fails ()
  "Test that dotimes with list fails with type error.
This demonstrates the common mistake AI makes."
  ;; This test verifies that the wrong approach fails
  (should (stringp (greet-names-wrong-dotimes)))
  (should (string-match-p "Expected error" (greet-names-wrong-dotimes))))

;;;; Tests for Iteration Functions (dotimes vs dolist)

(ert-deftest test-dotimes-with-number ()
  "Test that dotimes works correctly with a number."
  (let ((result '()))
    (dotimes (i 5)
      (push i result))
    (should (equal (nreverse result) '(0 1 2 3 4)))))

(ert-deftest test-dolist-with-list ()
  "Test that dolist works correctly with a list."
  (let ((result '()))
    (dolist (item '("a" "b" "c"))
      (push item result))
    (should (equal (nreverse result) '("a" "b" "c")))))

(ert-deftest test-dotimes-starts-at-zero ()
  "Test that dotimes starts counting at 0, not 1."
  (let ((first-value nil))
    (dotimes (i 3)
      (when (null first-value)
        (setq first-value i)))
    (should (= first-value 0))))

(ert-deftest test-dolist-gets-values-not-indices ()
  "Test that dolist gives values, not indices."
  (let ((first-value nil))
    (dolist (item '("x" "y" "z"))
      (when (null first-value)
        (setq first-value item)))
    (should (equal first-value "x"))
    (should-not (equal first-value 0))))

;;;; Integration Tests

(ert-deftest test-fizzbuzz-100-length ()
  "Test that FizzBuzz(100) produces 100 results."
  (should (= (length (fizzbuzz-correct 100)) 100)))

(ert-deftest test-fizzbuzz-100-fizzbuzz-count ()
  "Test that FizzBuzz(100) has correct number of FizzBuzz entries.
Numbers divisible by 15 in 1-100: 15, 30, 45, 60, 75, 90 = 6 entries."
  (let ((fizzbuzzes (cl-count "FizzBuzz" (fizzbuzz-correct 100) :test #'equal)))
    (should (= fizzbuzzes 6))))

(ert-deftest test-fizzbuzz-100-fizz-count ()
  "Test that FizzBuzz(100) has correct number of Fizz entries.
Divisible by 3 but not 15: 3,6,9,12,18,21,24,27,33... = 27 entries."
  (let ((fizzes (cl-count "Fizz" (fizzbuzz-correct 100) :test #'equal)))
    (should (= fizzes 27))))

(ert-deftest test-fizzbuzz-100-buzz-count ()
  "Test that FizzBuzz(100) has correct number of Buzz entries.
Divisible by 5 but not 15: 5,10,20,25,35,40,50,55,65,70,80,85,95,100 = 14 entries."
  (let ((buzzes (cl-count "Buzz" (fizzbuzz-correct 100) :test #'equal)))
    (should (= buzzes 14))))

;;;; Documentation Tests

(ert-deftest test-fizzbuzz-has-docstring ()
  "Test that fizzbuzz function has documentation."
  (should (documentation 'fizzbuzz-correct)))

(ert-deftest test-greet-names-has-docstring ()
  "Test that greet-names function has documentation."
  (should (documentation 'greet-names-correct)))

;;;; Performance Tests (Optional)

(ert-deftest test-fizzbuzz-performance ()
  "Test that FizzBuzz completes in reasonable time."
  :tags '(performance)
  (let ((start-time (current-time)))
    (fizzbuzz-correct 10000)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      ;; Should complete in less than 1 second
      (should (< elapsed 1.0)))))

;;;; Test Summary

(defun run-all-tests ()
  "Run all tests and display results.
Use this interactively: M-x run-all-tests"
  (interactive)
  (ert-run-tests-interactively "^test-"))

(defun run-correct-implementation-tests ()
  "Run only tests for correct implementations."
  (interactive)
  (ert-run-tests-interactively "^test-\\(fizzbuzz\\|greet-names\\)-\\(output\\|count\\|format\\|divisible\\)"))

(defun run-wrong-implementation-tests ()
  "Run only tests that demonstrate wrong implementations."
  (interactive)
  (ert-run-tests-interactively "^test-.*-wrong-"))

(defun run-iteration-tests ()
  "Run only tests for dotimes vs dolist."
  (interactive)
  (ert-run-tests-interactively "^test-\\(dotimes\\|dolist\\)"))

(provide 'test-ollama-fizzbuzz)

;;; test-ollama-fizzbuzz.el ends here
