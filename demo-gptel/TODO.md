# TODO - gptel Demo

## Current Status

**What exists:**
- README with overview
- docs/demo.org with 1,660 lines of content
- elisp/setup.el with comprehensive configuration (125 lines)
- **tests/ directory with working automated tests** ✅
- Captured test results showing real AI responses

**What's working:**
✅ Automated integration tests with Ollama
✅ Test results captured and saved
✅ Static unit tests for code validation
✅ Documented common AI mistakes

**Status:** Most mature demo in the repository

## Next Steps

### High Priority - Verification

- [ ] **Test with multiple backends**
  - Current: Ollama tested and working ✅
  - Test with OpenAI API (requires API key)
  - Test with Anthropic Claude (requires API key)
  - Test with Google Gemini (requires API key)
  - Document differences in response quality

- [ ] **Run docs/demo.org examples**
  - Execute each code block
  - Verify all examples work
  - Update any outdated code
  - Add results/screenshots

- [ ] **Verify setup.el on clean Emacs**
  - Test installation from scratch
  - Confirm use-package configuration works
  - Test with emacs -Q + minimal config
  - Document any missing dependencies

### Medium Priority - Expand Testing

- [ ] **Add more test scenarios**
  - Test different types of code generation (not just fizzbuzz)
  - Test documentation generation
  - Test code explanation
  - Test refactoring suggestions
  - Test bug fixing

- [ ] **Cross-model comparison**
  - Run same prompt across multiple models
  - Compare quality of responses
  - Document which models are best for what
  - Create comparison matrix

- [ ] **Test error handling**
  - What happens when Ollama is down?
  - What happens with invalid API keys?
  - What happens with network errors?
  - Document error messages and solutions

- [ ] **Improve test automation**
  - Add CI/GitHub Actions
  - Run tests on multiple Emacs versions
  - Test on different OS (macOS, Linux, Windows)
  - Automated quality checks on responses

### Medium Priority - Content

- [ ] **Add real-world examples**
  - Show gptel helping with actual Elisp development
  - Demonstrate multi-turn conversations
  - Show context management (editing prompts)
  - Include examples of fixing AI mistakes

- [ ] **Create workflow guides**
  - Code review workflow
  - Documentation generation workflow
  - Test writing workflow
  - Learning new code workflow

- [ ] **Document AI mistake patterns**
  - Currently have one example (fizzbuzz %3 vs mod)
  - Collect more examples from test results
  - Categorize common mistakes
  - Create guide: "What to watch for in AI-generated code"

- [ ] **Comparison with other tools**
  - gptel vs. GitHub Copilot
  - gptel vs. aider
  - gptel vs. ChatGPT web interface
  - When to use each

### Low Priority - Enhancement

- [ ] **Advanced prompt engineering**
  - Collection of effective prompts for Elisp
  - System prompts that improve code quality
  - Few-shot examples that work well
  - Prompt templates

- [ ] **Integration examples**
  - gptel + projectile
  - gptel + magit (for commit messages)
  - gptel + org-mode (for literate programming)
  - gptel + company (for completions?)

- [ ] **Performance optimization**
  - Streaming vs. non-streaming comparison
  - Timeout configuration
  - Caching strategies
  - Async operation best practices

## Test Infrastructure Status

### What's Working ✅

| Test File | Purpose | Status |
|-----------|---------|--------|
| `test-automated-ollama.el` | Integration test with Ollama | ✅ Working |
| `test-ollama-fizzbuzz.el` | Static unit tests | ✅ 23 tests passing |
| `quick-setup.el` | Test configuration | ✅ Working |
| `results/` directory | Captured outputs | ✅ Has examples |

### What Could Be Added

- [ ] Tests for other backends (OpenAI, Claude, Gemini)
- [ ] Tests for streaming responses
- [ ] Tests for conversation context
- [ ] Tests for error handling
- [ ] Performance benchmarks
- [ ] Quality metrics (how to measure if response is "good"?)

## Known Issues

- No verification of cloud API backends (OpenAI, Claude, Gemini)
- Test results directory could grow large over time (need cleanup strategy?)
- No automated quality assessment of AI responses
- docs/demo.org examples not verified to run

## Questions to Answer

1. **Quality comparison**
   - Which backend gives best code for Elisp?
   - How much better are paid APIs vs. free local models?
   - Is the quality difference worth the cost?

2. **Best practices**
   - How much context should you include?
   - When to start new conversation vs. continue?
   - How to structure prompts for code generation?

3. **Integration**
   - Can you use gptel + ellama together?
   - Best workflow for daily use?
   - How to incorporate into existing development process?

4. **Testing methodology**
   - How to automatically evaluate code quality?
   - How to detect if AI-generated code is correct?
   - What metrics matter?

## Success Criteria

This demo will be complete when:
- [x] Automated tests working (✅ Done with Ollama)
- [ ] Tests working with at least 2 different backends
- [ ] docs/demo.org examples all verified
- [ ] At least 5 different test scenarios (currently have 1)
- [ ] Real-world workflow examples documented
- [ ] Comparison with other tools provided
- [ ] Best practices guide written

## Contributing

The test infrastructure makes this demo easy to contribute to:

1. **Add new test scenarios**: Follow `test-automated-ollama.el` pattern
2. **Test different models**: Modify model name in test
3. **Analyze results**: Look at files in `results/` directory
4. **Document patterns**: Add findings to docs/demo.org

---

**Status**: Most complete demo - automated tests working, needs expansion to other backends
**Last Updated**: 2025-11-06
