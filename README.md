# org-test.el

Literal testing tools for org-mode.

## Usage

1. Write test code in src block
2. Name it with prefix `test-${test-name}`.
3. Write expected output in example block
4. Name it `expect-{test-name}-${test-type}`
   - Available match types:
     - `exact`: output matches exactly
     - `including`: output contains expected text
     - `excluding` / `not-including`: output must NOT contain expected text
     - `contains-all`: output contains all lines from expected (order-independent)
     - `matches` / `matching`: output matches expected regex pattern
5. Run tests:
   - `M-x org-test-run-current-buffer` - test current buffer
   - `(org-test-run "file.org")` - test a file
   - `(org-test-run "tests/")` - test all .org files in directory
   - `(org-test-run "api.org" "ui.org")` - test multiple files

## Example

~~~org
#+NAME: test-cowsay
#+begin_src bash
nix run nixpkgs#cowsay -- "Hello"
#+end_src

#+NAME: expect-cowsay-including
#+begin_example
Hello
#+end_example

#+NAME: expect-cowsay-exact
#+begin_example
< Hello >
 ----
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

#+end_example
~~~

For more examples, see [examples](examples/).

## Static Results (No Execution)

For tests that don't need execution (e.g., static content or pre-computed results), use `:eval no`:

~~~org
#+NAME: test-static
#+begin_src emacs-lisp :eval no
"Code won't run"
#+end_src

#+RESULTS: test-static
: "Hello, World!"

#+NAME: expect-static-exact
: "Hello, World!"
~~~

The test will use the cached result from `#+RESULTS:` block instead of executing.

## Configuration

### Timeout

Set global timeout for test execution (default: 30 seconds):

```elisp
;; Set timeout to 60 seconds
(setq org-test-default-timeout 60)

;; Disable timeout
(setq org-test-default-timeout nil)
```

## Development

### Setup

```bash
# Enter development environment (auto-installs git hooks)
nix develop

# Run tests manually
check
```

The git hooks are automatically installed when you enter the nix dev shell. Tests will run before each commit.

### Testing

Run tests with:
```bash
nix develop -c check
```

Or use nix flake check:
```bash
nix flake check
```

## API

### Functions

- `org-test-run-current-buffer`: Test current buffer (interactive)
- `org-test-run &rest targets`: Test given targets (variadic)
  - Accepts: buffers, file paths, directory paths, or any combination
  - Examples:
    ```elisp
    (org-test-run (current-buffer))
    (org-test-run "tests/api.org")
    (org-test-run "tests/")
    (org-test-run "api.org" "ui.org" "integration.org")
    ```

### Variables

- `org-test-default-timeout`: Global timeout in seconds (default: 30, nil to disable)
