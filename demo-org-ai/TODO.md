# TODO - org-ai Demo

## Current Status

**What exists:**
- README with overview
- docs/demo.org with 1,715 lines (most content!)
- elisp/setup.el with 117 lines of configuration

**What's uncertain:**
- Whether org-ai has been installed and tested
- If the demo.org examples work with current org-ai version
- Which LLM backends have been tested
- Whether special features (DALL-E, speech) have been verified

## Next Steps

### High Priority - Verification

- [ ] **Install and test org-ai**
  - Install from MELPA
  - Verify installation success
  - Check current version and compatibility
  - Test basic functionality

- [ ] **Test API configurations**
  - OpenAI API (required for base functionality)
  - Test with GPT-4
  - Test with GPT-3.5
  - Document API key setup

- [ ] **Run docs/demo.org examples**
  - Execute #+begin_ai blocks
  - Test text generation
  - Test code generation
  - Test inline completion
  - Document which features work

- [ ] **Test special features**
  - Image generation (DALL-E) - requires OpenAI API
  - Speech input (if implemented)
  - Speech output (if implemented)
  - Document prerequisites for each

### Medium Priority - Content

- [ ] **Add real examples with results**
  - Show actual AI responses inline in org files
  - Include successful code generation examples
  - Show documentation generation use cases
  - Demonstrate refactoring examples

- [ ] **Create workflow examples**
  - Using org-ai for blog writing
  - Using org-ai for technical documentation
  - Using org-ai for literate programming
  - Using org-ai for learning/notes

- [ ] **Document integration with org-mode**
  - Using AI with org-babel blocks
  - Generating org content
  - AI-assisted TODO management
  - Exporting AI-enhanced documents

- [ ] **Comparison guide**
  - org-ai vs. gptel in org-mode
  - When to use org-ai vs. other tools
  - Advantages of inline AI in org documents
  - Trade-offs (locked to OpenAI API?)

### Medium Priority - Advanced Usage

- [ ] **Test alternative backends**
  - Current org-ai primarily uses OpenAI
  - Check if other backends are supported
  - Document limitations vs. gptel's multi-backend support
  - Workarounds if locked to OpenAI

- [ ] **Prompt engineering for org-mode**
  - Best prompts for documentation generation
  - Prompts for code explanation in literate programming
  - System prompts for technical writing
  - Few-shot examples

- [ ] **Org-mode specific features**
  - AI-generated org structure (headlines, lists)
  - AI for org-agenda items
  - AI for org-roam notes
  - Integration with org-export

### Low Priority - Enhancement

- [ ] **Create template collection**
  - org-ai blocks for common tasks
  - Reusable prompt templates
  - Project-specific configurations
  - Yasnippet integration

- [ ] **Automated tests**
  - Follow demo-gptel test pattern
  - Test #+begin_ai block execution
  - Capture and verify responses
  - Test different prompt types

- [ ] **Advanced org integrations**
  - org-ai + org-roam (AI-enhanced note-taking)
  - org-ai + org-publish (AI-enhanced publishing)
  - org-ai + org-babel (AI code generation)
  - org-ai + org-agenda (AI task suggestions)

## Known Issues

- No verification that org-ai is currently working
- May be locked to OpenAI API (limits model choice)
- docs/demo.org examples not tested
- Special features (images, speech) not verified
- Unclear if maintained/compatible with latest org-mode

## Questions to Answer

1. **Backend support**
   - Is org-ai limited to OpenAI?
   - Can it use Claude, Gemini, or local models?
   - How does this limit compare to gptel?

2. **Maintenance status**
   - Is org-ai actively maintained?
   - Last update date?
   - Compatible with latest Emacs/org-mode?

3. **Unique capabilities**
   - What can org-ai do that gptel can't?
   - Why use org-ai vs. gptel in org-mode?
   - What are the killer features?

4. **Integration depth**
   - How well does it integrate with org-mode features?
   - Can AI generate proper org structure?
   - Does it understand org syntax?

5. **Performance**
   - How fast are responses?
   - Any caching?
   - Impact on org-mode performance?

## Special Features to Test

- [ ] **#+begin_ai blocks**
  - Basic text generation
  - Code generation
  - Multi-turn conversations
  - Context from surrounding org content

- [ ] **Inline completion**
  - Does it work like Copilot?
  - Quality of suggestions
  - Performance impact

- [ ] **Image generation** (if supported)
  - DALL-E integration
  - Image insertion in org documents
  - Use cases

- [ ] **Speech features** (if supported)
  - Voice input
  - Voice output
  - Use cases

## Prerequisites to Document

- [ ] OpenAI API key (required)
- [ ] Org-mode version
- [ ] Emacs version
- [ ] Additional API keys (DALL-E, etc.)
- [ ] External dependencies for speech features

## Success Criteria

This demo will be complete when:
- [ ] Installation verified on clean system
- [ ] All docs/demo.org examples work
- [ ] At least 3 real use cases demonstrated with results
- [ ] Special features tested (or documented as unavailable)
- [ ] Comparison with gptel approach provided
- [ ] Best practices for org-ai usage documented
- [ ] Clear explanation of when to use org-ai vs. alternatives

---

**Status**: Well-documented but not verified - needs testing
**Last Updated**: 2025-11-06
