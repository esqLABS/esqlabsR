# .canonicalizeId warns naming each changed id

    Code
      out <- .canonicalizeId("My ID*")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "My ID*" -> "my_id_"

# .canonicalizeId canonicalizes and warns on an id containing braces instead of aborting

    Code
      out <- .canonicalizeId("Conc{Organ}")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Conc{Organ}" -> "conc{organ}"

# .canonicalizeId errors on a post-canonicalization collision

    Code
      .canonicalizeId(c("ID", "id"))
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "ID" -> "id"
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "ID" and "id" -> "id"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

---

    Code
      .canonicalizeId(c("a/b", "a:b"))
    Condition
      Warning:
      Canonicalized 2 ids to a safe form:
      * "a/b" -> "a_b"
      * "a:b" -> "a_b"
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "a/b" and "a:b" -> "a_b"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

# .canonicalizeId still errors when distinct ids collide via characters

    Code
      .canonicalizeId(c("a/b", "a_b"))
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "a/b" -> "a_b"
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "a/b" and "a_b" -> "a_b"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

# a public authoring call aborts on a case-differing id collision

    Code
      addIndividual(project, c("Foo", "foo"), species = "Human", gender = "MALE")
    Condition
      Warning:
      Canonicalized 1 id to a safe form:
      * "Foo" -> "foo"
      Error in `.canonicalizeId()`:
      ! Ids collide after canonicalization:
      x "Foo" and "foo" -> "foo"
      i Two distinct ids that canonicalize to the same id are ambiguous; rename so they differ by more than case or forbidden characters.

# .canonicalizeId errors on an id too long to be a filename

    Code
      .canonicalizeId(longId)
    Condition
      Error in `FUN()`:
      ! Definition id is too long to be a safe filename: 300 bytes (limit 250).
      x "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      i An id becomes the file '<id>.json'; shorten it to at most 250 bytes.

# .canonicalizeIdRef canonicalizes and warns on a reference containing braces instead of aborting

    Code
      out <- .canonicalizeIdRef("Ind{Organ}")
    Condition
      Warning:
      Canonicalized 1 referenced id to a safe form:
      * "Ind{Organ}" -> "ind{organ}"

