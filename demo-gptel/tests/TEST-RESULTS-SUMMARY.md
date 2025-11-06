# Test Results Summary - AI Iterative Prompt Test

## Run Information

- **Date:** 2025-11-03 13:22-13:23
- **Model:** llama3.2:latest (Ollama)
- **Test:** 5 progressive prompts (vague → explicit)
- **Purpose:** Test if better prompts → better AI code

## Results Overview

| Attempt | Prompt Quality | Evaluation | Execution | Output | Overall |
|---------|---------------|------------|-----------|---------|---------|
| 1 | Vague | PASS | FAIL | N/A | ⚠ LOADS BUT CRASHES |
| 2 | Basic | ? | ? | ? | ? |
| 3 | Medium | PASS | FAIL | N/A | ⚠ LOADS BUT CRASHES |
| 4 | Better | PASS | FAIL | N/A | ⚠ LOADS BUT CRASHES |
| 5 | Ultra-explicit | PASS | PASS | **FAIL** | ⚠ RUNS BUT OUTPUT WRONG |

## Detailed Findings

### Attempt 1: "Write a FizzBuzz function."

**AI Generated:** Elixir code!

```elixir
defmodule FizzBuzz do
  def run(n) do
    Enum.map(1..n, fn i ->
      if rem(i, 3) == 0 and rem(i, 5) != 0 do
        "Jazz"  # ← AI invented "Jazz" for multiples of 15!
      ...
```

**Error:** Symbol's function definition is void: fizzbuzz

**Analysis:** AI defaulted to Elixir (functional language) instead of Emacs Lisp. Completely wrong language.

---

### Attempt 3: "Write an Emacs Lisp function called fizzbuzz. Use dotimes... Use message..."

**AI Generated:** Emacs Lisp with C/Python syntax

```elisp
(defun fizzbuzz (n)
  (dotimes (i n)
    (let ((result "")
           (multiple-of-3 (zerop i % 3)))  ; ← INFIX % OPERATOR
```

**Error:** Symbol's value as variable is void: %

**Analysis:** AI mixed Lisp with C/Python infix operator syntax (`i % 3` instead of `(mod i 3)`).

---

### Attempt 4: "...Use (zerop (mod num 15)) for FizzBuzz..."

**AI Generated:** Wrong function signature

```elisp
(defun fizzbuzz ()    ; ← NO PARAMETER!
  (dotimes (n 100)    ; ← HARDCODED
```

**Error:** Wrong number of arguments: #[nil ...], 1

**Analysis:** AI ignored the parameter requirement and hardcoded the limit to 100.

---

### Attempt 5: Full specification with all details

**AI Generated:** Valid Emacs Lisp! But with logic errors:

```elisp
(defun fizzbuzz (n)
  (dotimes (i (1- n))    ; ← BUG: iterates 0 to n-2, not 1 to n
    (cond ((zerop (mod i 15)) (message "FizzBuzz"))  ; ← Checks i, not (1+ i)
          ((zerop (mod i 3)) (message "Fizz"))
          ((zerop (mod i 5)) (message "Buzz"))
          (t (message "%d" (1+ i))))))    ; ← Only default uses (1+ i)
```

**Output:**
```
FizzBuzz  ← i=0, should be "1"
2         ← Correct
3         ← Should be "Fizz" (i=2, but (1+i)=3)
Fizz      ← Correct
5         ← Should be "Buzz"
...
14        ← Stops at 14, missing "FizzBuzz" for 15
```

**Error:** Output does not match expected sequence

**Analysis:**
- Code evaluates ✓
- Code runs ✓
- Output is WRONG:
  - Uses `(1- n)` so iterates 0 to 13 (should be 1 to 15)
  - Checks divisibility of `i` but outputs `(1+ i)` only in default case
  - Creates off-by-one errors throughout

---

## Key Learnings

### 1. Better Prompts DO Help

- **Attempt 1 (vague):** Wrong programming language entirely
- **Attempt 3 (medium):** Right language, wrong syntax
- **Attempt 5 (explicit):** Valid syntax, runs without crashing

**Progression:** Elixir → Syntax errors → Logic errors

### 2. Even Best Prompts Have Issues

Despite ultra-explicit instructions, Attempt 5 still has:
- Off-by-one errors (loop bounds)
- Inconsistent variable usage
- Wrong output sequence

### 3. AI Mistake Patterns

**Language confusion:**
- Defaults to other languages when vague
- Mixes syntax from multiple languages

**Syntax errors:**
- Infix operators from C/Python (`i % 3`)
- Common Lisp constructs

**Logic errors:**
- Loop bounds off by one
- Inconsistent use of iteration variable

### 4. Validation is Critical

**Without testing:**
- Attempt 5 "looks" correct at first glance
- Syntax is valid
- Function definition is proper
- Only running it reveals the bugs

**With testing:**
- Immediately catches the wrong output
- Shows exactly what's wrong
- Validates the sequence matches expected

## Conclusions

### What Works
✅ Progressive prompting improves AI output quality
✅ Ultra-explicit prompts prevent syntax errors
✅ llama3.2 can generate valid Emacs Lisp syntax

### What Doesn't Work
❌ Even detailed prompts don't guarantee correct logic
❌ AI doesn't always follow loop iteration instructions correctly
❌ Off-by-one errors persist despite explicit guidance

### Recommendations

1. **Always test AI-generated code**
   - Don't assume it works because it looks right
   - Run it and verify output

2. **Use very explicit prompts**
   - Specify language clearly
   - Give syntax examples
   - State what NOT to use

3. **Expect logic errors**
   - Even when syntax is perfect
   - Verify algorithm correctness
   - Test edge cases

4. **Iterate on failures**
   - Use error messages to improve prompts
   - Show AI what went wrong
   - Request specific fixes

## Files Generated

All results saved to `results/` directory:

- `ai-attempt-1-20251103-132256.txt` - Elixir code
- `ai-attempt-2-20251103-132305.txt` - (need to review)
- `ai-attempt-3-20251103-132316.txt` - Infix % operator
- `ai-attempt-4-20251103-132327.txt` - Wrong function signature
- `ai-attempt-5-20251103-132335.txt` - Valid but wrong logic

Each file contains:
- Exact prompt sent
- Exact AI response
- Extracted code
- Mistake analysis
- Test results (evaluation, execution, output verification)
- Summary

## Test Validity

✅ **This test is valid because:**
- Tests ONLY what AI generates
- NO auto-correction applied
- Documents exact AI output
- Verifies output correctness

This demonstrates real AI behavior, not wishful thinking.

## Next Steps

To get working FizzBuzz from AI:

1. **Option A:** Fix and re-prompt
   - Show AI the error output
   - Request specific fixes
   - Test again

2. **Option B:** Different model
   - Try larger model (llama3.1 70B)
   - Try different model family
   - Compare results

3. **Option C:** Pair programming approach
   - Generate code
   - Test it
   - Show errors to AI
   - Let AI fix its own mistakes
   - Iterate until correct

The test proves: **Better prompts help, but don't guarantee correctness.**
