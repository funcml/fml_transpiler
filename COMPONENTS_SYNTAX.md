# FML Syntax Reference

This document outlines the syntax of the FML language as defined by the parser in `src/FML/Parser/Component.hs`.

## Components

The top-level structure in FML is a component. A component definition consists of a name, the `=>` operator, and a body enclosed in parentheses.

### Syntax
```fml
ComponentName => (RootElement)
```

- `ComponentName`: An alphanumeric identifier for your component.
- `RootElement`: A single FML element that serves as the entry point for the component's structure.

### Example
```fml
MyComponent => (
  div.container (
    h1 "Hello, FML!"
  )
)
```

## Elements

Elements are the fundamental building blocks of FML, analogous to HTML tags. They consist of a tag, optional attributes, and optional children.

### Syntax
```fml
tag(attributes...) (children...)
```
or for child-free elements:
```fml
tag(attributes...)
```

- `tag`: An identifier for the element, like `div`, `p`, `h1`.

### Examples
```fml
// An element with children
div (
  p $ "Some text."
)

// A child-free element (like HTML's <img> or <hr>)
img.hero-image src="/path/to/image.png"
```

## Attributes

Attributes provide additional information about an element. They are space-separated and placed after the element's tag.

### 1. Standard Attributes
Standard attributes are key-value pairs.

**Syntax**: `key="value"`

**Example**: `a href="/about" title="About Us"`

### 2. Class Shorthand
A dot (`.`) followed by a class name is a shorthand for the `class` attribute.

**Syntax**: `.className`

**Example**: `div.card.shadow` is equivalent to `div class="card shadow"` (Note: The current parser has a bug and may not handle multiple dot-shorthands correctly on the same element).

### 3. ID Shorthand
A hash (`#`) followed by an ID is a shorthand for the `id` attribute.

**Syntax**: `#idName`

**Example**: `div#main-content` is equivalent to `div id="main-content"`

### Combining Attributes
All attribute styles can be mixed and matched.
```fml
// A div with an ID, two classes, and a data attribute
div#profile.user-card.active data-userid="123"
```

## Children

An element's children define its nested content. FML provides several flexible ways to declare children.

### 1. Parenthesized List `()`
A comma-separated list of child elements inside parentheses. This is the most common way to define multiple complex children.

**Syntax**: `parent(child1, child2, ...)`

**Example**:
```fml
ul (
  li $ "First item",
  li $ "Second item",
  li $ "Third item"
)
```
An empty list `()` is also valid.

### 2. Inline Child `$`
A dollar sign (`$`) prefixes a single child element. This is useful for elements that have only one child.

**Syntax**: `parent $ child`

**Example**:
```fml
div $ p "This paragraph is the only child of the div."
```

### 3. String Literals
A double-quoted string can be used directly as a child, representing a text node.

**Example**: `h1 "This is a heading"`

## Syntax Combinations & Examples

The different syntaxes can be combined to create complex structures.

### Nested Elements
```fml
article (
  h1 "Article Title",
  p (
    "This is a paragraph with "
    strong "bold text"
    " and "
    em "italic text."
  )
)
```

### Mixed Child Syntax
```fml
// A div with children defined using all three styles
div.content (
  h1 "Mixed Content"
) $ p "An inline child paragraph." span // An implicit child
```

### Full Component Example
```fml
ProfileCard => (
  div#user-123.card.active (
    img.card-avatar src="/avatars/123.png",
    div.card-body (
      h2.card-title "User Name",
      p.card-text "A short bio about the user.",
      a.card-link href="/users/123" $ "View Profile"
    )
  )
)
```
