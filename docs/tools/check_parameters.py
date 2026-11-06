#!/usr/bin/env python3
"""Check the IdrAgra parameter reference against its Fortran sources.

The parser is authoritative for accepted parameter names.  Initialized
derived-type declarations are authoritative for types and defaults.  The
documentation remains authoritative for descriptions, valid ranges, and
modelling guidance.

The checker intentionally handles only the regular patterns used by
``read_sim_parameters``.  When it cannot resolve a declaration, it reports a
warning rather than guessing.
"""

from __future__ import annotations

import argparse
import re
import sys
from collections import Counter
from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Iterable, Sequence


REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_PARSER = REPOSITORY_ROOT / "src" / "cli_read_parameter.f90"
DEFAULT_DECLARATIONS = (
    REPOSITORY_ROOT / "src" / "mod_parameters.f90",
)
DEFAULT_DOCUMENTATION = REPOSITORY_ROOT / "docs" / "pages" / "parameters.md"

ROOT_TYPES = {
    "xml": "parameters",
}

# These cases parse or transform the input instead of reading it directly into
# the value whose declaration supplies the default.
DESTINATION_EXCEPTIONS = {
    "startsimulation": "xml%sim%start",
    "endsimulation": "xml%sim%end",
}

# User-facing formats for parser branches that translate text into a different
# internal representation, plus the one list read through a temporary array.
PARAMETER_METADATA_OVERRIDES = {
    "monthlyflag": ("String", "monthly"),
    "weekday": ("String or Integer", "monday"),
    "randsowdayssym": ("String", "symmetric"),
}


@dataclass(frozen=True)
class Declaration:
    name: str
    base_type: str
    derived_type: str | None
    initializer: str | None
    is_array: bool


@dataclass(frozen=True)
class CodeParameter:
    name: str
    destination: str | None
    type_name: str | None = None
    default: str | None = None


@dataclass(frozen=True)
class DocumentedParameter:
    name: str
    spelling: str
    line: int
    type_name: str | None
    default: str | None


def read_text(path: Path) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except OSError as exc:
        raise ValueError(f"cannot read {path}: {exc}") from exc


def strip_fortran_comment(line: str) -> str:
    """Remove a Fortran comment while preserving exclamation marks in strings."""
    quote: str | None = None
    index = 0
    while index < len(line):
        char = line[index]
        if quote:
            if char == quote:
                if index + 1 < len(line) and line[index + 1] == quote:
                    index += 2
                    continue
                quote = None
        elif char in "'\"":
            quote = char
        elif char == "!":
            return line[:index]
        index += 1
    return line


def logical_fortran_lines(text: str) -> Iterable[str]:
    """Yield simple free-form Fortran logical lines with continuations joined."""
    pending = ""
    for physical in text.splitlines():
        line = strip_fortran_comment(physical).strip()
        if not line:
            continue
        if line.startswith("&"):
            line = line[1:].lstrip()
        continued = line.endswith("&")
        if continued:
            line = line[:-1].rstrip()
        pending = f"{pending} {line}".strip()
        if not continued:
            yield pending
            pending = ""
    if pending:
        yield pending


def split_top_level(value: str, delimiter: str = ",") -> list[str]:
    parts: list[str] = []
    start = 0
    round_depth = 0
    square_depth = 0
    quote: str | None = None
    index = 0
    while index < len(value):
        char = value[index]
        if quote:
            if char == quote:
                if index + 1 < len(value) and value[index + 1] == quote:
                    index += 2
                    continue
                quote = None
        elif char in "'\"":
            quote = char
        elif char == "(":
            round_depth += 1
        elif char == ")":
            round_depth -= 1
        elif char == "[":
            square_depth += 1
        elif char == "]":
            square_depth -= 1
        elif char == delimiter and round_depth == 0 and square_depth == 0:
            parts.append(value[start:index].strip())
            start = index + 1
        index += 1
    parts.append(value[start:].strip())
    return parts


def split_assignment(value: str) -> tuple[str, str | None]:
    parts = split_top_level(value, "=")
    if len(parts) == 1:
        return parts[0], None
    return parts[0], "=".join(parts[1:]).strip()


def declaration_base_type(specification: str) -> tuple[str | None, str | None]:
    lowered = specification.casefold().strip()
    derived = re.match(r"type\s*\(\s*([a-z_]\w*)\s*\)", lowered)
    if derived:
        return "derived", derived.group(1)
    for base in ("integer", "logical", "character", "real", "double precision"):
        if lowered.startswith(base):
            return ("real" if base == "double precision" else base), None
    return None, None


def parse_declarations(paths: Sequence[Path]) -> dict[str, dict[str, Declaration]]:
    types: dict[str, dict[str, Declaration]] = {}
    for path in paths:
        current_type: str | None = None
        for line in logical_fortran_lines(read_text(path)):
            type_start = re.match(r"^type\s*(?:::)?\s*([a-z_]\w*)\s*$", line, re.I)
            if type_start and not line.casefold().startswith("type("):
                current_type = type_start.group(1).casefold()
                if current_type is not None:
                    types.setdefault(current_type, {})
                continue
            if re.match(r"^end\s*type\b", line, re.I):
                current_type = None
                continue
            if current_type is None or "::" not in line:
                continue

            specification, variables = line.split("::", 1)
            base_type, derived_type = declaration_base_type(specification)
            if base_type is None:
                continue
            specification_is_array = bool(re.search(r"\bdimension\s*\(", specification, re.I))
            for variable in split_top_level(variables):
                lhs, initializer = split_assignment(variable)
                match = re.match(r"^([a-z_]\w*)\s*(\([^)]*\))?", lhs.strip(), re.I)
                if not match:
                    continue
                name = match.group(1).casefold()
                types[current_type][name] = Declaration(
                    name=name,
                    base_type=base_type,
                    derived_type=derived_type.casefold() if derived_type else None,
                    initializer=initializer,
                    is_array=specification_is_array or match.group(2) is not None,
                )
    return types


def extract_subroutine(text: str, name: str) -> list[str]:
    lines = text.splitlines()
    start = None
    for index, line in enumerate(lines):
        if re.match(rf"^\s*subroutine\s+{re.escape(name)}\b", strip_fortran_comment(line), re.I):
            start = index
            break
    if start is None:
        raise ValueError(f"subroutine {name!r} was not found")
    result: list[str] = []
    for index in range(start, len(lines)):
        line = strip_fortran_comment(lines[index])
        result.append(line)
        if index > start and re.match(rf"^\s*end\s+subroutine\s+{re.escape(name)}\b", line, re.I):
            return result
    raise ValueError(f"subroutine {name!r} has no matching end statement")


def destination_from_block(name: str, block: Sequence[str]) -> str | None:
    if name in DESTINATION_EXCEPTIONS:
        return DESTINATION_EXCEPTIONS[name]

    read_pattern = re.compile(
        r"\bread\s*\(\s*buffer\b[^)]*\)\s*"
        r"(xml(?:%[a-z_]\w*)+(?:\([^)]*\))?)",
        re.I,
    )
    for line in block:
        match = read_pattern.search(line)
        if match:
            return match.group(1)

    assignment_pattern = re.compile(
        r"\b(xml(?:%[a-z_]\w*)+(?:\([^)]*\))?)\s*=",
        re.I,
    )
    destinations = [
        match.group(1)
        for line in block
        for match in assignment_pattern.finditer(line)
    ]
    if destinations:
        return Counter(item.casefold() for item in destinations).most_common(1)[0][0]
    return None


def parse_parser(path: Path) -> tuple[list[CodeParameter], list[str]]:
    lines = extract_subroutine(read_text(path), "read_sim_parameters")
    select_pattern = re.compile(r"^\s*select\s+case\s*\((.*?)\)", re.I)
    case_pattern = re.compile(r"^\s*case\s*\((.*?)\)", re.I)
    end_pattern = re.compile(r"^\s*end\s*select\b", re.I)
    quoted_pattern = re.compile(r"(['\"])(.*?)\1")

    depth = 0
    target_depth: int | None = None
    current_names: list[str] = []
    current_block: list[str] = []
    parameters: list[CodeParameter] = []

    def finish_block() -> None:
        if not current_names:
            return
        for parameter_name in current_names:
            parameters.append(
                CodeParameter(
                    name=parameter_name,
                    destination=destination_from_block(parameter_name, current_block),
                )
            )

    for line in lines:
        select = select_pattern.match(line)
        if select:
            depth += 1
            if target_depth is None and select.group(1).strip().casefold() == "label":
                target_depth = depth
            if current_names:
                current_block.append(line)
            continue

        if end_pattern.match(line):
            if target_depth is not None and depth == target_depth:
                finish_block()
                target_depth = None
                current_names = []
                current_block = []
            elif current_names:
                current_block.append(line)
            depth -= 1
            continue

        case = case_pattern.match(line)
        if target_depth is not None and depth == target_depth and case:
            finish_block()
            current_names = [
                match.group(2).strip().casefold()
                for match in quoted_pattern.finditer(case.group(1))
            ]
            current_block = []
            continue

        if current_names:
            current_block.append(line)

    counts = Counter(parameter.name for parameter in parameters)
    duplicates = sorted(name for name, count in counts.items() if count > 1)
    return parameters, duplicates


def parse_destination(destination: str) -> tuple[str, list[str], tuple[int, ...] | None]:
    pieces = destination.casefold().split("%")
    root = pieces[0]
    indices: tuple[int, ...] | None = None
    final = pieces[-1]
    indexed = re.match(r"^([a-z_]\w*)\s*\(([^)]*)\)$", final)
    if indexed:
        pieces[-1] = indexed.group(1)
        try:
            indices = tuple(int(value.strip()) for value in indexed.group(2).split(","))
        except ValueError:
            indices = None
    return root, pieces[1:], indices


def resolve_declaration(
    destination: str,
    declarations: dict[str, dict[str, Declaration]],
) -> tuple[Declaration, tuple[int, ...] | None] | None:
    root, fields, indices = parse_destination(destination)
    current_type = ROOT_TYPES.get(root)
    if current_type is None:
        return None
    declaration: Declaration | None = None
    for position, field in enumerate(fields):
        declaration = declarations.get(current_type, {}).get(field)
        if declaration is None:
            return None
        if position < len(fields) - 1:
            if declaration.base_type != "derived" or declaration.derived_type is None:
                return None
            current_type = declaration.derived_type
    return (declaration, indices) if declaration else None


def array_element(initializer: str, indices: tuple[int, ...]) -> str | None:
    """Return a scalar from the simple array constructors used by the defaults."""
    value = initializer.strip()
    dimensions: list[int] | None = None
    reshape = re.match(r"^reshape\s*\(\s*\[(.*)\]\s*,\s*\[([^]]+)\]\s*\)$", value, re.I)
    if reshape:
        values = split_top_level(reshape.group(1))
        try:
            dimensions = [int(item.strip()) for item in split_top_level(reshape.group(2))]
        except ValueError:
            return None
    else:
        constructor = re.match(r"^\[(.*)\]$", value, re.S)
        if not constructor:
            return None
        values = split_top_level(constructor.group(1))
        dimensions = [len(values)]

    if len(indices) != len(dimensions) or any(index < 1 for index in indices):
        return None
    offset = 0
    stride = 1
    for index, dimension in zip(indices, dimensions):
        if index > dimension:
            return None
        offset += (index - 1) * stride
        stride *= dimension
    return values[offset].strip() if offset < len(values) else None


def display_type(declaration: Declaration, indices: tuple[int, ...] | None) -> str:
    names = {
        "integer": "Integer",
        "logical": "Boolean",
        "character": "String",
        "real": "Real",
    }
    if declaration.base_type == "derived":
        result = (declaration.derived_type or "Derived type").replace("_", " ").title()
    else:
        result = names[declaration.base_type]
    if declaration.is_array and indices is None:
        result += " array"
    return result


def display_default(declaration: Declaration, indices: tuple[int, ...] | None) -> str:
    if declaration.initializer is None:
        return "none"
    initializer = declaration.initializer.strip()
    if indices:
        element = array_element(initializer, indices)
        if element is not None:
            initializer = element
    if declaration.base_type == "character":
        quoted = re.match(r"^(['\"])(.*)\1$", initializer, re.S)
        if quoted:
            return quoted.group(2).replace(quoted.group(1) * 2, quoted.group(1))
    if declaration.base_type == "logical":
        if initializer.casefold() == ".true.":
            return "true"
        if initializer.casefold() == ".false.":
            return "false"
    return re.sub(r"\s+", " ", initializer)


def enrich_parameters(
    parameters: Sequence[CodeParameter],
    declarations: dict[str, dict[str, Declaration]],
) -> list[CodeParameter]:
    enriched: list[CodeParameter] = []
    for parameter in parameters:
        override = PARAMETER_METADATA_OVERRIDES.get(parameter.name)
        if override:
            enriched.append(
                CodeParameter(
                    name=parameter.name,
                    destination=parameter.destination,
                    type_name=override[0],
                    default=override[1],
                )
            )
            continue
        resolved = (
            resolve_declaration(parameter.destination, declarations)
            if parameter.destination
            else None
        )
        if resolved:
            declaration, indices = resolved
            enriched.append(
                CodeParameter(
                    name=parameter.name,
                    destination=parameter.destination,
                    type_name=display_type(declaration, indices),
                    default=display_default(declaration, indices),
                )
            )
        else:
            enriched.append(parameter)
    return enriched


def metadata_from_section(lines: Sequence[str]) -> tuple[str | None, str | None]:
    metadata: dict[str, str] = {}
    inline_pattern = re.compile(
        r"\*\*(Type|Default|Required|Unit):\*\*\s*(.*?)"
        r"(?=\s+·\s+\*\*(?:Type|Default|Required|Unit):\*\*|$)",
        re.I,
    )
    field_pattern = re.compile(r"^\s*:(Type|Default|Required|Unit):\s*(.*?)\s*$", re.I)
    table_key_pattern = re.compile(r"^\s*\*\s+-\s*(Type|Default)\s*$", re.I)
    table_value_pattern = re.compile(r"^\s*-\s*(.*?)\s*$")

    for index, line in enumerate(lines):
        for match in inline_pattern.finditer(line):
            key, value = match.group(1).casefold(), match.group(2).strip()
            if key == "required":
                metadata["default"] = "none"
            else:
                metadata[key] = value
        field = field_pattern.match(line)
        if field:
            key, value = field.group(1).casefold(), field.group(2).strip()
            if key == "required":
                metadata["default"] = "none"
            else:
                metadata[key] = value
            continue
        key = table_key_pattern.match(line)
        if key:
            for following in lines[index + 1 :]:
                if not following.strip():
                    continue
                value = table_value_pattern.match(following)
                if value:
                    metadata[key.group(1).casefold()] = value.group(1).strip()
                break
    return metadata.get("type"), metadata.get("default")


def parse_documentation(path: Path) -> tuple[list[DocumentedParameter], list[str]]:
    lines = read_text(path).splitlines()
    heading_pattern = re.compile(
        r"^(#{2,6})\s+`([^`]+)`"
        r"(?:\s+<span\s+class=\"parameter-heading-default\">\s*=\s*"
        r"<code>(.*?)</code>\s*</span>)?\s*(?:#+\s*)?$"
    )
    headings: list[tuple[int, int, str, str | None]] = []
    for index, line in enumerate(lines):
        match = heading_pattern.match(line)
        if match:
            headings.append(
                (index, len(match.group(1)), match.group(2).strip(), match.group(3))
            )

    documented: list[DocumentedParameter] = []
    for position, (index, _level, spelling, heading_default) in enumerate(headings):
        # Every parameter heading starts a fresh metadata block, including a
        # nested H4 parameter below an H3 controller.
        end = headings[position + 1][0] if position + 1 < len(headings) else len(lines)
        type_name, default = metadata_from_section(lines[index + 1 : end])
        default = default or heading_default
        documented.append(
            DocumentedParameter(
                name=spelling.casefold(),
                spelling=spelling,
                line=index + 1,
                type_name=type_name,
                default=default,
            )
        )

    counts = Counter(parameter.name for parameter in documented)
    duplicates = sorted(name for name, count in counts.items() if count > 1)
    return documented, duplicates


def inline_code_value(value: str) -> str:
    code_spans = re.findall(r"`([^`]+)`", value)
    return code_spans[0].strip() if len(code_spans) == 1 else value.strip()


def type_matches(documented: str, expected: str) -> bool:
    actual = inline_code_value(documented).casefold().replace("_", " ")
    expected_lower = expected.casefold()
    aliases = {
        "integer": ("integer",),
        "boolean": ("boolean", "logical"),
        "string": ("string", "character"),
        "real": ("real", "float", "floating point"),
        "date": ("date",),
    }
    expected_base = expected_lower.removesuffix(" array")
    words = aliases.get(expected_base, (expected_base,))
    if not any(re.search(rf"\b{re.escape(word)}\b", actual) for word in words):
        return False
    if expected_lower.endswith(" array"):
        return bool(re.search(r"\b(array|list|sequence)\b", actual))
    return True


def numeric_value(value: str) -> Decimal | None:
    cleaned = value.strip().casefold()
    cleaned = re.sub(r"_\w+$", "", cleaned)
    cleaned = re.sub(r"(?<=\d)[dD](?=[+-]?\d)", "e", cleaned)
    try:
        return Decimal(cleaned)
    except InvalidOperation:
        return None


def normalized_default(value: str) -> str | Decimal:
    cleaned = inline_code_value(value).strip()
    quoted = re.match(r"^(['\"])(.*)\1$", cleaned, re.S)
    if quoted:
        cleaned = quoted.group(2).replace(quoted.group(1) * 2, quoted.group(1))
    lowered = cleaned.casefold()
    logical_aliases = {
        ".true.": "true",
        "true": "true",
        ".false.": "false",
        "false": "false",
        "none": "none",
        "required": "none",
        "no default": "none",
    }
    if lowered in logical_aliases:
        return logical_aliases[lowered]
    number = numeric_value(cleaned)
    if number is not None:
        return number
    return re.sub(r"\s+", " ", cleaned).casefold()


def relative(path: Path) -> str:
    try:
        return str(path.relative_to(REPOSITORY_ROOT))
    except ValueError:
        return str(path)


def check(
    code_parameters: Sequence[CodeParameter],
    documented_parameters: Sequence[DocumentedParameter],
    parser_duplicates: Sequence[str],
    documentation_duplicates: Sequence[str],
    documentation_path: Path,
    allow_undocumented: bool,
) -> tuple[list[str], list[str]]:
    errors: list[str] = []
    warnings: list[str] = []
    code_by_name = {parameter.name: parameter for parameter in code_parameters}
    docs_by_name = {parameter.name: parameter for parameter in documented_parameters}

    for name in parser_duplicates:
        errors.append(f"parser contains duplicate parameter {name!r}")
    for name in documentation_duplicates:
        errors.append(f"documentation contains duplicate parameter {name!r}")

    for name in sorted(docs_by_name.keys() - code_by_name.keys()):
        item = docs_by_name[name]
        errors.append(
            f"{relative(documentation_path)}:{item.line}: {item.spelling!r} is not accepted by the parser"
        )

    missing = sorted(code_by_name.keys() - docs_by_name.keys())
    if missing:
        message = "parameters accepted by code but missing from documentation: " + ", ".join(missing)
        (warnings if allow_undocumented else errors).append(message)

    for name in sorted(code_by_name.keys() & docs_by_name.keys()):
        code = code_by_name[name]
        document = docs_by_name[name]
        location = f"{relative(documentation_path)}:{document.line}"
        if code.type_name is None or code.default is None:
            warnings.append(
                f"{location}: could not resolve code metadata for {document.spelling!r}; "
                "name was checked, type and default were not"
            )
            continue
        if document.type_name is None:
            errors.append(f"{location}: {document.spelling!r} has no Type metadata")
        elif not type_matches(document.type_name, code.type_name):
            errors.append(
                f"{location}: {document.spelling!r} type is {document.type_name!r}; "
                f"code says {code.type_name!r}"
            )
        if document.default is None:
            errors.append(f"{location}: {document.spelling!r} has no Default metadata")
        elif normalized_default(document.default) != normalized_default(code.default):
            errors.append(
                f"{location}: {document.spelling!r} default is {document.default!r}; "
                f"code says {code.default!r}"
            )
    return errors, warnings


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--parser", type=Path, default=DEFAULT_PARSER)
    parser.add_argument(
        "--declarations",
        type=Path,
        nargs="+",
        default=list(DEFAULT_DECLARATIONS),
        help="Fortran files containing the relevant derived-type declarations",
    )
    parser.add_argument("--documentation", type=Path, default=DEFAULT_DOCUMENTATION)
    parser.add_argument(
        "--allow-undocumented",
        action="store_true",
        help="report missing documentation as a warning while the guide is being drafted",
    )
    parser.add_argument(
        "--list",
        action="store_true",
        help="print the code-derived name, type, and default, then exit",
    )
    return parser


def print_parameter_table(parameters: Sequence[CodeParameter]) -> None:
    rows = [
        (parameter.name, parameter.type_name or "?", parameter.default or "?")
        for parameter in parameters
    ]
    headers = ("Name", "Type", "Default")
    widths = [
        max(len(headers[column]), *(len(row[column]) for row in rows))
        for column in range(len(headers))
    ]
    template = "  ".join(f"{{:<{width}}}" for width in widths)
    print(template.format(*headers).rstrip())
    print(template.format(*("-" * width for width in widths)).rstrip())
    for row in rows:
        print(template.format(*row).rstrip())


def main(argv: Sequence[str] | None = None) -> int:
    arguments = build_argument_parser().parse_args(argv)
    try:
        parsed, parser_duplicates = parse_parser(arguments.parser)
        declarations = parse_declarations(arguments.declarations)
        code_parameters = enrich_parameters(parsed, declarations)
    except ValueError as exc:
        print(f"parameter check failed: {exc}", file=sys.stderr)
        return 2

    if arguments.list:
        print_parameter_table(code_parameters)
        return 0

    try:
        documented, documentation_duplicates = parse_documentation(arguments.documentation)
    except ValueError as exc:
        print(f"parameter check failed: {exc}", file=sys.stderr)
        return 2

    errors, warnings = check(
        code_parameters,
        documented,
        parser_duplicates,
        documentation_duplicates,
        arguments.documentation,
        arguments.allow_undocumented,
    )
    for warning in warnings:
        print(f"warning: {warning}", file=sys.stderr)
    for error in errors:
        print(f"error: {error}", file=sys.stderr)

    if errors:
        print(
            f"parameter check failed with {len(errors)} error(s) and {len(warnings)} warning(s)",
            file=sys.stderr,
        )
        return 1
    print(
        f"parameter check passed: {len(documented)} documented / "
        f"{len(code_parameters)} accepted parameter(s), {len(warnings)} warning(s)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
