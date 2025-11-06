# TODO - aider.el Demo

## Current Status

**What exists:**
- Basic README with overview and resources
- docs/demo.org with 492 lines of content
- elisp/setup.el with installation instructions

**What's uncertain:**
- Whether aider.el is available on MELPA (README notes it may need manual installation)
- If the demo.org examples have been tested end-to-end
- Whether setup.el configuration actually works

## Next Steps

### High Priority - Verification

- [ ] **Test installation on clean Emacs**
  - Try installation from MELPA
  - If not on MELPA, document straight.el or manual installation process
  - Verify all package dependencies are available

- [ ] **Run docs/demo.org examples**
  - Execute each code block with C-c C-c
  - Document which examples work vs. which fail
  - Note any API key requirements
  - Fix any broken examples

- [ ] **Verify setup.el**
  - Test use-package configuration
  - Confirm keybindings work (M-x aider)
  - Test with at least one LLM provider

- [ ] **Document prerequisites**
  - Python version required for aider-chat
  - Which API keys are needed
  - Minimum Emacs version

### Medium Priority - Content

- [ ] **Add real examples**
  - Show actual aider session transcript
  - Demonstrate git integration features
  - Show multi-file editing example
  - Compare with vanilla Aider CLI

- [ ] **Expand docs/demo.org**
  - Add troubleshooting section
  - Document common errors and solutions
  - Add example prompts that work well
  - Show before/after code examples

- [ ] **Create comparison**
  - When to use aider.el vs. gptel vs. other tools?
  - Strengths and weaknesses
  - Integration with Magit workflows

### Low Priority - Enhancement

- [ ] **Add automated tests**
  - Follow demo-gptel test pattern
  - Create simple integration test
  - Test that aider process starts/stops correctly

- [ ] **Screenshots/GIFs**
  - Show aider UI in Emacs
  - Demonstrate key workflows visually

- [ ] **Advanced examples**
  - Custom aider commands
  - Project-specific configurations
  - Integration with projectile/project.el

## Known Issues

- aider.el installation method unclear (not confirmed on MELPA)
- No verification that examples work
- Missing error handling documentation

## Questions to Answer

1. Is aider.el actively maintained? Last commit date?
2. What's the best way to install it in 2025?
3. How does it compare to using aider directly in terminal?
4. Can you use multiple LLM providers simultaneously?
5. Does it work with local models via Ollama?

## Success Criteria

This demo will be complete when:
- [ ] Installation instructions are verified and work
- [ ] All code blocks in demo.org execute successfully
- [ ] At least 3 real examples showing actual use cases
- [ ] Prerequisites clearly documented
- [ ] Comparison with alternatives provided

---

**Status**: Documented but not verified
**Last Updated**: 2025-11-06
