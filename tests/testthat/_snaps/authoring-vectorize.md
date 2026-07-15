# .assertIdVector rejects empty, NA, or non-character ids

    Code
      .assertIdVector(character(0))
    Condition
      Error:
      ! `id` must be a non-empty character vector with no NA or empty element.

---

    Code
      .assertIdVector(c("a", NA))
    Condition
      Error:
      ! `id` must be a non-empty character vector with no NA or empty element.

---

    Code
      .assertIdVector(c("a", ""))
    Condition
      Error:
      ! `id` must be a non-empty character vector with no NA or empty element.

---

    Code
      .assertIdVector(1:3)
    Condition
      Error:
      ! `id` must be a non-empty character vector with no NA or empty element.

# .recycleField aborts on a length that is neither 1 nor N

    Code
      .recycleField(c("a", "b"), 3L, "weight")
    Condition
      Error:
      ! `weight` must be length 1 or length 3 (the number of ids).
      x It is length 2.

# .alignAuthoringArgs propagates a length error naming the field

    Code
      .alignAuthoringArgs(id = c("a", "b", "c"), scalarFields = list(weight = c(60,
        70)))
    Condition
      Error:
      ! `weight` must be length 1 or length 3 (the number of ids).
      x It is length 2.

