# validationResult

R6 class for storing validation results from Excel configuration files

## Public fields

- `data`:

  Successfully validated/processed data

- `critical_errors`:

  List of critical errors (blocking issues)

- `warnings`:

  List of warnings (non-blocking issues)

## Methods

### Public methods

- [`validationResult$new()`](#method-validationResult-new)

- [`validationResult$add_critical_error()`](#method-validationResult-add_critical_error)

- [`validationResult$add_warning()`](#method-validationResult-add_warning)

- [`validationResult$set_data()`](#method-validationResult-set_data)

- [`validationResult$is_valid()`](#method-validationResult-is_valid)

- [`validationResult$has_critical_errors()`](#method-validationResult-has_critical_errors)

- [`validationResult$get_formatted_messages()`](#method-validationResult-get_formatted_messages)

- [`validationResult$get_summary()`](#method-validationResult-get_summary)

- [`validationResult$clone()`](#method-validationResult-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new ValidationResult

#### Usage

    validationResult$new()

------------------------------------------------------------------------

### Method `add_critical_error()`

Add a critical error

#### Usage

    validationResult$add_critical_error(category, message, details = NULL)

#### Arguments

- `category`:

  Error category (e.g., "Structure", "Missing Fields", "Uniqueness")

- `message`:

  Error message

- `details`:

  Optional list with additional details (sheet, row, column)

------------------------------------------------------------------------

### Method `add_warning()`

Add a warning

#### Usage

    validationResult$add_warning(category, message, details = NULL)

#### Arguments

- `category`:

  Warning category (e.g., "Data", "Structure")

- `message`:

  Warning message

- `details`:

  Optional list with additional details (sheet, row, column)

------------------------------------------------------------------------

### Method `set_data()`

Set validated data

#### Usage

    validationResult$set_data(data)

#### Arguments

- `data`:

  The validated/processed data to store

------------------------------------------------------------------------

### Method `is_valid()`

Check if validation passed (no critical errors)

#### Usage

    validationResult$is_valid()

------------------------------------------------------------------------

### Method `has_critical_errors()`

Check if there are critical errors

#### Usage

    validationResult$has_critical_errors()

------------------------------------------------------------------------

### Method `get_formatted_messages()`

Get formatted messages for display

#### Usage

    validationResult$get_formatted_messages()

------------------------------------------------------------------------

### Method `get_summary()`

Get validation summary

#### Usage

    validationResult$get_summary()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    validationResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
