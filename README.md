# org-test.el

Literal testing tools for org-mode.

## Usage

1. Write expected block named `expect-<name>-<type>`
   - Available match types:
     - `equals`: output matches exactly
     - `includes`: output contains expected text
     - `matches`: output matches expected regex pattern
2. Write test code in src block named `<name>`.
3. Run tests:
   - `M-x org-test-run-current-buffer` - test current buffer
   - `(org-test-run "file.org")` - test a file
   - `(org-test-run "tests/")` - test all .org files in directory
   - `(org-test-run "api.org" "ui.org")` - test multiple files

## Example

~~~org
#+NAME: cowsay
#+begin_src bash
nix run nixpkgs#cowsay -- "Hello"
#+end_src

#+NAME: expect-cowsay-includes
#+begin_example
Hello
#+end_example

#+NAME: expect-cowsay-equals
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

### Testing

Run tests with:
```bash
nix develop -c check
# or
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
