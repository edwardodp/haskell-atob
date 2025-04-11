# haskell-atob

An interpreter for the esoteric language **A=B** (pronounced *"a to b"*), written entirely in [Haskell](https://www.haskell.org/).

---

## What is A=B?

A=B is a minimalist esoteric programming language that operates solely through **text replacement rules**. The program consists of a series of transformation instructions, and an **input string** is passed in at runtime. The instructions modify the string in a loop until no more transformations can be made.

### How it works

Each line of an A=B program contains:

```
string1=string2
```

- If `string1` exists in the current string, it is replaced by `string2`.
- If not, the program moves to the next line.
- When a rule is applied, the interpreter restarts from the top.
- The program halts only when **no rule** applies anymore.

---

## Special Syntax

A=B features several special tokens that add conditional behavior to replacement rules:

| Syntax                          | Meaning                                                                 |
|---------------------------------|-------------------------------------------------------------------------|
| `(start)`                       | Match only at the **start** of the input string                         |
| `(end)`                         | Match only at the **end** of the input string                           |
| `string1=(start)string2`        | Insert `string2` at **start** after removing `string1`                  |
| `string1=(end)string2`          | Insert `string2` at **end** after removing `string1`                    |
| `(start)string1=(end)string2`   | Match `string1` at start, delete it, append `string2` at end            |
| `string1=(return)string2`       | If `string1` is found, replace it with `string2` and **terminate**      |
| `(once)`                        | Rule can only be executed **once** during entire program execution      |

### Combined Example

```
(once)(end)abc=(start)xyz
```

If the string ends in `abc`, remove it and add `xyz` to the front — but only once.

---

## Running the Interpreter

Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed.

### Run an A=B program:
```bash
stack run -- programname inputstring
```

> Note: Omit the `.to` extension when passing `programname`.  
> Don't use quotation marks around the input string.  

### File Requirements:
- Program files must end with `.to` and be located in the `code/` directory.
- Input and instruction strings must be:
  - no spaces
  - no reserved characters: `(` `)` `=` `#`
  - No empty input strings, but empty instruction strings are permitted (and useful!)

### Comments
Use `#` at the beginning or middle of a line for a comment. Only one uncommented instruction per line is allowed.

---

## Project Structure

```
haskell-atob/
├── code/            # Your .to esolang program files
├── test/            # Integration tests and input/output cases
├── app/             # Entry point (Main.hs)
├── src/             # Library code (interpreter logic)
├── README.md
└── package.yaml
```

---

## Testing

Test cases are located in `test/cases/`. Each `.txt` file contains alternating lines:
```
input1
expected_output1
input2
expected_output2
...
```

The interpreter is tested using these pairs to ensure correctness.

To run tests:

```bash
stack test
```

You can also see the code solutions to these tests under `code/` (all of which primarily assume only "a", "b", and "c" are used for inputs)

---

## License

This project is licensed under a custom permissive license. See the [LICENSE](https://github.com/UnknownPaws/haskell-atob/blob/main/LICENSE) file for details.

---

## Author

Made by **Edward** when I definitely should've been studying for exams.

