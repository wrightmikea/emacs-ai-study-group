# TODO - elisp-dev-mcp Demo

## Current Status

**What exists:**
- README with overview of MCP server concept
- docs/demo.org with 814 lines of content
- elisp/setup.el with configuration examples

**What's uncertain:**
- Whether the MCP server has been installed and tested
- If the demo.org examples actually connect to a running MCP server
- Whether this demonstrates the Emacs side, AI client side, or both

## Next Steps

### High Priority - Verification

- [ ] **Install and run elisp-dev-mcp server**
  - Follow installation instructions from https://github.com/ppbb/elisp-dev-mcp
  - Document installation steps (npm/pip/binary?)
  - Get server running and verify it responds

- [ ] **Configure MCP client**
  - Document which client is being used (Claude Desktop? Custom?)
  - Show configuration file setup
  - Test connection to server
  - Verify MCP tools/resources are accessible

- [ ] **Test docs/demo.org examples**
  - Execute each code block
  - Verify MCP interactions work
  - Document which require running server vs. which are standalone
  - Fix any broken examples

- [ ] **Clarify the demo scope**
  - Is this demonstrating how to USE the MCP server from Emacs?
  - Or how to INTERACT with Emacs via an AI + MCP?
  - Or how to BUILD an MCP server for Emacs?

### Medium Priority - Content

- [ ] **Add end-to-end example**
  - Show complete workflow: start server → configure client → make request → see result
  - Include actual AI interaction demonstrating MCP capabilities
  - Show what the AI can do with Emacs access that it couldn't otherwise

- [ ] **Document MCP concepts**
  - What is Model Context Protocol?
  - Why use it vs. direct tool integration?
  - What can elisp-dev-mcp server provide to AI?
  - Resources, tools, and prompts it exposes

- [ ] **Create comparison**
  - MCP approach vs. aider.el vs. gptel
  - When to use each
  - Advantages of giving AI Emacs access

- [ ] **Add practical examples**
  - AI refactoring code using Emacs buffer access
  - AI running tests via Emacs
  - AI querying Emacs documentation
  - AI manipulating multiple buffers

### Low Priority - Enhancement

- [ ] **Video/screencast**
  - Show AI + MCP + Emacs working together
  - Demonstrate something impressive that showcases the approach

- [ ] **Security considerations**
  - What access does the MCP server give to AI?
  - How to limit permissions?
  - Best practices for safe usage

- [ ] **Advanced integration**
  - Multiple MCP servers
  - Custom MCP tools
  - Extending elisp-dev-mcp

## Known Issues

- Not clear if MCP server has been tested
- Setup may require external dependencies (Node.js? Python?)
- MCP client configuration not documented
- May need specific Claude Desktop version or API access

## Questions to Answer

1. What exactly can the AI do with elisp-dev-mcp access?
2. Does this require Claude Desktop specifically or work with other MCP clients?
3. Is the server actively maintained?
4. Can you run MCP server + local Emacs instance?
5. What's the performance impact?
6. How does this compare to direct Emacs scripting?

## Dependencies to Verify

- [ ] MCP protocol version supported
- [ ] Claude Desktop version (if required)
- [ ] Node.js/npm version (if required)
- [ ] Python version (if required)
- [ ] Emacs version and required packages

## Success Criteria

This demo will be complete when:
- [ ] MCP server can be installed following documented steps
- [ ] At least one complete AI + MCP + Emacs workflow demonstrated
- [ ] docs/demo.org examples all work
- [ ] Clear explanation of what MCP adds vs. other approaches
- [ ] Security/permissions documented
- [ ] Prerequisites and dependencies clearly listed

---

**Status**: Documented but MCP integration not verified
**Last Updated**: 2025-11-06
