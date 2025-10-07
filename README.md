# FunML - A (Fun)ctional Markup Language!

<img src="/assets/images/funml.png" alt="funml" width="200"/>
## Project Overview

**FunML (Functional Markup Language)** is an innovative markup language designed to bring the power and expressiveness of functional programming concepts to document authoring and data representation. FunML enables users to write concise, readable, and highly composable markup that can be transformed and processed with functional paradigms.

### Milestone
- [x] Parsing Components and Tags
- [ ] Parsing Attributes
- [ ] Parsing Props
- [ ] Parsing State
- [ ] Parsing Event Pipeline

### Syntax Examples

Here are some basic FunML syntax examples:

```funml
:style (
    * {
        padding: 0;
        margin: 0;
    }
)

:script CounterScript (
    const increment = (count) => {
        set(count, count + 1)
    }

    use(increment, () => {
        console.log(increment)
    })
)

:component Counter (initial)
    :using CounterScript
    :state count initial
    (
        :div class="flex items-center justify-center" (
            :h1 (
                "The count is: " + get(count)
            )
            :button class="rounded-md px-3 py-2 border" $ @click |> increment(count) $ "Increment Count!"
        )
    )

:component App
(
    :main (
        :h1 $ "This is the app!"
        :Counter initial=5
    )
)
```

### Use Cases

- **Data Transformation Pipelines:** Build robust pipelines for transforming and validating data.
- **Configurable Document Generation:** Generate dynamic documents, reports, or configuration files with reusable functional components.
- **Educational Tool:** Teach functional programming concepts in a markup-based environment.

### Getting Started

1. **Install FunML:**  
    Clone the repository and follow the installation instructions in the [docs](./docs/INSTALL.md).

2. **Write Your First FunML Document:**  
    See the [examples](./examples/) folder for sample FunML files.

3. **Explore the Language:**  
    Check out the [language reference](./docs/LANGUAGE.md) for syntax and advanced features.

### Contributing

We welcome contributions! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for guidelines.

### License

FunML is released under the MIT License.

---
For more information, visit the [project website](https://github.com/pemfung/funml) or join the discussion on our [community forum](https://community.funml.dev).
