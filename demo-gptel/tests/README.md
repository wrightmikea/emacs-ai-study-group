# AI-Generated Code Tests

This directory contains tests for validating AI-generated Emacs Lisp code using gptel + Ollama.

## Test Files

### 1. ✅ **test-automated-ollama.el** - FULLY AUTOMATED (WORKING)

**What it does:**
- Automatically calls Ollama via gptel
- Captures AI responses
- Saves results to timestamped files
- **NO manual steps required!**

**Prerequisites:**
```bash
# Start Ollama
ollama serve

# Pull a model
ollama pull llama3.2
```

**Run in Emacs:**
```elisp
;; 1. Load setup
(load-file "demo-gptel/tests/quick-setup.el")

;; 2. Load test
(load-file "demo-gptel/tests/test-automated-ollama.el")

;; 3. Run it
(test-automated-ollama-single)

;; Wait ~30-60 seconds for Ollama to respond
;; Result will be saved to results/ directory
```

**Run from command line:**
```bash
cd demo-gptel/tests
emacs --batch --eval "(progn
  (load-file \"quick-setup.el\")
  (load-file \"test-automated-ollama.el\")
  (test-automated-ollama-single)
  (sleep-for 60))"
```

**Example Output:**
```
Sending prompt to Ollama...
Backend: Ollama, Model: llama3.2:latest

=== Response received ===
Response type: string

=== AI RESPONSE ===
(defun fizzbuzz (n)
  (dotimes (i n)
    (message "%d" (if (< i %3) "Fizz"
                     (if (= i %5) "Buzz"
                         (format "%d" i))))))

Saved to: results/test-result-20251103-125655.txt
```

**What Gets Saved:**
Each test creates a timestamped file in `results/` containing:
- Timestamp
- Backend and model used
- Full AI response
- Extracted code

See `results/test-result-20251103-125655.txt` for a real example!

### 2. ✅ **test-ollama-fizzbuzz.el** - Static Unit Tests

**What it does:**
- Tests pre-written correct vs incorrect implementations
- 23 ERT tests validating documented corrections
- Does NOT call AI - tests static code only

**Run:**
```bash
cd demo-gptel/tests
emacs -Q -batch -l test-ollama-fizzbuzz.el -f ert-run-tests-batch-and-exit
```

**Purpose:** Validates that the documented corrections in `../docs/demo.org` are actually correct.

### 3. ⚠️ **test-iterative-prompt-improvement.el** - Manual Interactive

**What it does:**
- Tests 5 progressively better prompts
- Requires manual copy-paste of AI responses
- Analyzes each attempt for mistakes

**Status:** Manual interaction required - use `test-automated-ollama.el` instead for automation.

### 4. 📁 **results/** Directory

Contains all captured AI test results with timestamps.

**Example file:** `test-result-20251103-125655.txt`
```
=== AUTOMATED TEST RESULT ===

Timestamp: Mon Nov  3 12:56:55 2025
Backend: Ollama
Model: llama3.2:latest

=== AI RESPONSE ===
(defun fizzbuzz (n)
  (dotimes (i n)
    (message "%d" (if (< i %3) "Fizz"
                     (if (= i %5) "Buzz"
                         (format "%d" i))))))

=== END ===
```

## Quick Start

**To actually test AI code generation:**

```elisp
;; In Emacs:
(load-file "demo-gptel/tests/quick-setup.el")
(load-file "demo-gptel/tests/test-automated-ollama.el")
(test-automated-ollama-single)

;; Wait 30-60 seconds, then check results:
(dired "demo-gptel/tests/results")
```

**From command line:**
```bash
cd /Users/mike/github/emacs-ai-study-group/emacs-ai-study-group/demo-gptel/tests
emacs --batch --eval "(progn
  (load-file \"quick-setup.el\")
  (load-file \"test-automated-ollama.el\")
  (test-automated-ollama-single)
  (sleep-for 60))"

# Check results
ls -lt results/
cat results/test-result-*.txt
```

## What This Proves

✅ **Automation works** - No manual copy-paste needed
✅ **gptel + Ollama integration works** - Real AI responses captured
✅ **Results are saved** - Timestamped files for review
✅ **AI makes mistakes** - Example shows `%3` instead of `(mod i 3)`

## Common AI Mistakes Demonstrated

From actual captured responses:

❌ **Wrong:** `(if (< i %3) "Fizz"` - `%3` is invalid syntax
✅ **Right:** `(if (zerop (mod i 3)) "Fizz"` - Proper modulo check

❌ **Wrong:** `(= i %5)` - `%5` is invalid
✅ **Right:** `(zerop (mod i 5))` - Correct Emacs Lisp

This demonstrates why testing AI-generated code is essential!

## Files Summary

| File | Type | Uses AI? | Automated? | Purpose |
|------|------|----------|------------|---------|
| `test-automated-ollama.el` | Integration | ✅ Yes | ✅ Yes | **Actually call Ollama and capture results** |
| `test-ollama-fizzbuzz.el` | Unit | ❌ No | ✅ Yes | Validate documented corrections |
| `test-iterative-prompt-improvement.el` | Manual | ✅ Yes | ❌ No | Interactive prompt testing |
| `quick-setup.el` | Config | N/A | N/A | Setup gptel + Ollama |
| `results/` | Output | N/A | N/A | Captured AI responses |

## Next Steps

1. **Run the automated test** to see it work
2. **Review results/** to see what AI generated
3. **Analyze mistakes** in the generated code
4. **Document patterns** you observe
5. **Share with study group** to compare results across models

## Contributing

When you find interesting AI mistakes:
1. Run `test-automated-ollama-single`
2. Save the result file
3. Document the mistake pattern
4. Add to `../docs/demo.org` "Common AI Mistakes" section

This helps everyone learn from real examples!
