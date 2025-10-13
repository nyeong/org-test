# org-test.el

Literal testing tools for org-mode.

## Usage

1. Write test code in src block
2. Name it with prefix `test-${test-name}`.
3. Write expected output in example block
4. Name it `expect-{test-name}-${test-type}`
   - Available match types:
     - `including`: output contains expected text
     - `exact`: output matchees exactly
4. Run `M-x org-test-run-current-buffer`

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

- `org-test-run-current-buffer`: Test current buffer.
- `org-test-run`: Test given files, buffers, directories or string.
