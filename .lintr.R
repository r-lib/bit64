linters = all_linters(
  packages = "lintr",
  implicit_assignment_linter(allow_lazy=TRUE, allow_scoped=TRUE),
  implicit_integer_linter(allow_colon=TRUE),
  line_length_linter(120L),
  semicolon_linter(allow_compound=TRUE),
  undesirable_function_linter(fun = modify_defaults(
    defaults = default_undesirable_functions,
    library = NULL,
    options = NULL,
    par = NULL
  )),
  assignment_linter = NULL,
  commented_code_linter = NULL,
  condition_call_linter = NULL,
  cyclocomp_linter = NULL,
  function_argument_linter = NULL,
  indentation_linter = NULL,
  infix_spaces_linter = NULL,
  library_call_linter = NULL,
  nonportable_path_linter = NULL,
  object_name_linter = NULL,
  quotes_linter = NULL,
  todo_comment_linter = NULL
)
