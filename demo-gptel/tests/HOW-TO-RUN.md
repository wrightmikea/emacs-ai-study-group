# How to Run the AI Tests

## The Proper Test: test-ai-iterative.el ✅

This is the **valid test** that demonstrates whether better prompts lead to better AI-generated code.

### What It Tests

- ✅ Sends 5 progressively better prompts to Ollama
- ✅ Documents **exactly** what the AI generates (NO corrections)
- ✅ Tests each response: Does it evaluate? Run? Produce correct output?
- ✅ Verifies FizzBuzz output is actually correct
- ✅ Shows whether AI improves with better prompts

### How to Run It

**In Emacs:**

```elisp
;; 1. Setup
(load-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/quick-setup.el")

;; 2. Load test
(load-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/test-ai-iterative.el")

;; 3. Run all 5 attempts
(test-ai-iterative-all)

;; This will take ~5 minutes (5 prompts × ~60 seconds each)
;; Results will be saved to results/ directory
```

**From Command Line:**

```bash
cd /Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests

emacs --batch --eval "(progn
  (load-file \"quick-setup.el\")
  (load-file \"test-ai-iterative.el\")
  (test-ai-iterative-all)
  (sleep-for 300))"  # Wait 5 minutes for all attempts

# Then view results
ls -lt results/ai-attempt-*.txt
```

### What You'll Get

Five result files showing AI's progressive improvement (or lack thereof):

```
results/
├── ai-attempt-1-TIMESTAMP.txt  (vague prompt → probably broken)
├── ai-attempt-2-TIMESTAMP.txt  (better prompt → maybe better?)
├── ai-attempt-3-TIMESTAMP.txt  (explicit prompt → hopefully better)
├── ai-attempt-4-TIMESTAMP.txt  (very explicit → should be close)
└── ai-attempt-5-TIMESTAMP.txt  (ultra-explicit → hopefully works!)
```

Each file contains:

```
===================================================================
AI ITERATIVE TEST - Attempt N of 5
===================================================================

-------------------------------------------------------------------
PROMPT SENT TO AI
-------------------------------------------------------------------
[The exact prompt sent]

-------------------------------------------------------------------
AI RESPONSE (EXACTLY AS RECEIVED)
-------------------------------------------------------------------
[Exactly what AI generated - NO modifications]

-------------------------------------------------------------------
EXTRACTED CODE (NO MODIFICATIONS)
-------------------------------------------------------------------
[The code extracted from response]

-------------------------------------------------------------------
MISTAKE ANALYSIS
-------------------------------------------------------------------
  ✗ Uses 'format t' (Common Lisp - will fail in Emacs)
  ✗ Uses infix % operator (C/Python syntax - not valid in Lisp)

-------------------------------------------------------------------
TEST 1: EVALUATION (Can the code be loaded?)
-------------------------------------------------------------------
✗ FAILED - Code failed to evaluate
   Error: void-variable %

-------------------------------------------------------------------
TEST 2: EXECUTION (Can the function run?)
-------------------------------------------------------------------
⊘ SKIPPED - Code did not evaluate

-------------------------------------------------------------------
TEST 3: OUTPUT VERIFICATION (Is the output correct?)
-------------------------------------------------------------------
⊘ SKIPPED - Function did not run successfully

-------------------------------------------------------------------
SUMMARY
-------------------------------------------------------------------
Prompt Quality: Attempt 1 (vague → explicit)
Evaluation: FAIL
Execution: N/A
Output Correct: N/A
Mistakes Detected: 2
Overall: ✗ COMPLETELY BROKEN

===================================================================
NOTE: This shows ONLY what the AI generated.
NO automatic corrections were applied.
===================================================================
```

## The Learning Tool: test-automated-ollama.el ⚠️

**WARNING:** This test includes auto-correction by my regex code, NOT by the AI.

### What It's Good For

- Shows common AI mistake patterns
- Demonstrates what corrections are needed
- Educational tool for learning Emacs Lisp vs Common Lisp differences

### What It's NOT Good For

- ❌ Does NOT prove AI can improve
- ❌ Does NOT show AI learning
- ❌ Corrections are from MY code, not the AI

### How to Run It (If You Want to See Common Mistakes)

```elisp
(load-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/quick-setup.el")
(load-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/test-automated-ollama.el")
(test-automated-ollama-single)
```

Results will clearly mark corrections as "NOT FROM AI".

## Comparison Table

| Feature | test-ai-iterative.el | test-automated-ollama.el |
|---------|---------------------|--------------------------|
| Tests actual AI output | ✅ Yes | ✅ Yes |
| Multiple prompts | ✅ Yes (5 prompts) | ❌ No (single) |
| Auto-correction | ❌ No (proper) | ⚠️ Yes (misleading) |
| Output verification | ✅ Yes (checks correctness) | ❌ No (only runs) |
| Valid demonstration | ✅ Yes | ❌ No |
| Educational value | ✅ Shows AI improvement | ⚠️ Shows mistakes only |

## Quick Start (The Right Way)

**Just copy-paste this into Emacs scratch buffer:**

```elisp
(progn
  ;; Load everything
  (load-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/quick-setup.el")
  (load-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/test-ai-iterative.el")

  ;; Run the test
  (test-ai-iterative-all)

  (message "Test running... This will take ~5 minutes for 5 attempts")
  (message "Results will be saved to results/ directory")

  ;; Auto-open results after 5 minutes
  (run-with-timer
   300 nil
   (lambda ()
     (find-file "/Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests/results/")
     (message "All attempts complete! Review the ai-attempt-*.txt files"))))
```

Press `C-x C-e` at the end and wait ~5 minutes.

## What This Proves

After running `test-ai-iterative.el`, you can compare results to see:

1. **Does better prompting help?**
   - Compare attempt 1 (vague) vs attempt 5 (explicit)
   - Did the AI generate working code with better prompts?

2. **What mistakes does AI make?**
   - Common Lisp vs Emacs Lisp confusion
   - Infix operators (C/Python syntax)
   - Wrong logic (checking divisibility incorrectly)

3. **When does AI succeed?**
   - Which prompt quality level produces working code?
   - Does the output actually match expected FizzBuzz sequence?

**This is a valid demonstration because it tests ONLY what the AI generates.**
