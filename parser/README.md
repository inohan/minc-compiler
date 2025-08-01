# C Parser for a larger subset of the language

## Modifications
- `for` loops
- Variable declarations at any position inside a compound statement
- Variable assignment at declaration time
- Support for `short`, `int`, `float`, `double`, and `struct XX` types
- Different operators such as ternary `?:`, shifts `>>`, bitwise and logical and/or (`&`), unary dereference and reference (`*`, `&`), etc.
- Multiple variables can be declared in a single declaration (`int x = 0, y = 2`)

## Implementations
- Implemented using the [object model and walker of TatSu](https://tatsu.readthedocs.io/en/stable/mini-tutorial.html). In order to modify parser, run
    ```sh
    tatsu minc_extend.y > parser.py
    ```

## Usage
1. Activate the virtual environment
    ```sh
    cd ~/notebooks/pl08_minc/extra/parser
    source .venv/bin/activate
    ```

2. Run `main.py`
    ```sh
    python3 main.py filename_to_parse > filename_to_save
    ```

3. Deactivate virtual environment when done
    ```sh
    deactivate
    ```