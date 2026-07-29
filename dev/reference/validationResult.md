# validationResult

R6 class for storing validation results

## Public fields

- `critical_errors`:

  List of critical errors (blocking issues)

- `warnings`:

  List of warnings (non-blocking issues)

## Methods

### Public methods

- [`validationResult$new()`](#method-validationResult-initialize)

- [`validationResult$addCriticalError()`](#method-validationResult-addCriticalError)

- [`validationResult$addWarning()`](#method-validationResult-addWarning)

- [`validationResult$isValid()`](#method-validationResult-isValid)

- [`validationResult$hasCriticalErrors()`](#method-validationResult-hasCriticalErrors)

- [`validationResult$getFormattedMessages()`](#method-validationResult-getFormattedMessages)

- [`validationResult$getSummary()`](#method-validationResult-getSummary)

- [`validationResult$clone()`](#method-validationResult-clone)

------------------------------------------------------------------------

### `validationResult$new()`

Initialize a new ValidationResult

#### Usage

    validationResult$new()

------------------------------------------------------------------------

### `validationResult$addCriticalError()`

Add a critical error

#### Usage

    validationResult$addCriticalError(category, message, details = NULL)

#### Arguments

- `category`:

  Error category (e.g., "Structure", "Missing Fields", "Uniqueness")

- `message`:

  Error message

- `details`:

  Optional list with additional details (sheet, row, column)

------------------------------------------------------------------------

### `validationResult$addWarning()`

Add a warning

#### Usage

    validationResult$addWarning(category, message, details = NULL)

#### Arguments

- `category`:

  Warning category (e.g., "Data", "Structure")

- `message`:

  Warning message

- `details`:

  Optional list with additional details (sheet, row, column)

------------------------------------------------------------------------

### `validationResult$isValid()`

Check if validation passed (no critical errors)

#### Usage

    validationResult$isValid()

------------------------------------------------------------------------

### `validationResult$hasCriticalErrors()`

Check if there are critical errors

#### Usage

    validationResult$hasCriticalErrors()

------------------------------------------------------------------------

### `validationResult$getFormattedMessages()`

Get formatted messages for display

#### Usage

    validationResult$getFormattedMessages()

------------------------------------------------------------------------

### `validationResult$getSummary()`

Get validation summary

#### Usage

    validationResult$getSummary()

------------------------------------------------------------------------

### `validationResult$clone()`

The objects of this class are cloneable with this method.

#### Usage

    validationResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
