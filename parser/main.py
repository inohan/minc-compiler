import tatsu
from parser import minCParser
import models_ as models
from walker import XmlGenerator
from argparse import ArgumentParser
from typing import cast

def main():
    parser = ArgumentParser(description="Generate XML from minC source code.")
    parser.add_argument("filename", help="Path to the minC source file.")
    args = parser.parse_args()
    parser = minCParser(semantics=models.minCModelBuilderSemantics())
    with open(args.filename, "r") as f:
        ast: models.Program = cast(models.Program, parser.parse(f.read()))
    xml = XmlGenerator() # pyright: ignore[reportUnreachable]
    xml.walk(node=ast)
    print(xml._root.toprettyxml())

if __name__ == "__main__":
    main()
