# FunML - A (Fun)ctional Markup Language!

<img src="/assets/images/funml.png" alt="funml" width="200"/>

## Project Overview

**FunML (Functional Markup Language)** is an innovative markup language designed to bring the power and expressiveness of functional programming concepts to document authoring and data representation. FunML enables users to write concise, readable, and highly composable markup that can be transformed and processed with functional paradigms.

### Milestone
- [x] Parsing Components and Tags
- [x] Parsing Attributes
- [x] Parsing Props
- [x] Parsing State

### Syntax Examples

Here are some basic FunML syntax examples:

```funml
script (
  import { createSignal } from '@lib';
  
  const [count, setCount] = createSignal(0);
)

Counter => (
  div class="p-4 bg-gray-100 rounded-lg text-center" (
    h1 class="text-2xl font-bold mb-4" $ "Hello world",
    p class="" $ [() => `Counter: ${count()}`],
    button onclick=[() => setCount(prev => prev + 1)] $ "Increment"
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


### 📚 Lesson Learnt
##### Parser Combinator

Untuk melakukan parsing dan transpilasi, kita menggabungkan berbagai macam instance dari type Parser berikut untuk consume string input jika pattern tertentu terpenuhi.

##### “Power” dari Monad

Monad memungkinkan komposisi yang mentransformasi suatu value serta manajemen state secara pure.

##### Balanced Parentheses / Bracket Parsing

- Problem: Misalkan input berikut, “[ ]” digunakan untuk menandakan javascript expression. Namun javascript expression sendiri bisa mengandung “[ ]“. Permasalahan terjadi ketika parser tidak tahu closing bracket “]” itu milik scope js expression atau merupakan string dari kode js.

- Solution: Proses parsing tidak dapat dilakukan secara greedy dari kiri ke kanan. Parsing dilakukan dari “[ ]“ terluar hingga terdalam secara rekursif.

##### Design Syntax

- Dari pembuatan parser untuk templating language ini, saya mendapati mengapa format syntax HTML/JSX cukup populer. Tidak hanya karena format ini umum digunakan, tapi juga proses parsing menjadi jauh lebih sederhana. Hal ini karena start dan end serta delimiter pada syntax HTML/JSX memiliki ambiguitas yang lebih sedikit. Hal ini menurunkan jumlah edge cases serta kompleksitas dari parser.

- Jadi lesson learned disini adalah banyak sekali edge case yang perlu dipikirkan saat merancang syntax. Dan perlu dipertimbangkan juga apakah solusi baru itu lebih baik dibanding solusi yang sudah ada. Dari sini juga terlihat terdapat beberapa design choice  “hidden” pada bahasa yang biasa kita gunakan  serta compromise antara kompleksitas parsing/compiling dengan developer experience.

##### Manual vs Automatic Parsing

- Selama ini kita menggunakan manual top-down parsing dengan sebagian besar berupa algoritma recursive descent parsing
- Namun ada juga alternatif lain seperti algoritma automatic parsing seperti LALR yang merupakan algoritma bottom-up parsing yang secara otomatis menghasilkan parser dari input berupa grammar.
- Pada minggu ini kita mencoba menggunakan parser bernama lezer yang merupakan sebuah LR(1) dan pseudo-LALR ketika kita mencoba mengimplementasikan syntax highlighting di codemirror code editor di REPL.
- Kita hanya perlu menuliskan grammar dan regex tokenizer dan secara otomatis parser akan dihasilkan.

###### Manual parser (terutama RD parser)
- Bersifat top-down (lebih intuitif). bisa dengan mudah mengimplementasikan dan menyelesaikan beberapa masalah seperti nested brackets atau paired bracket dan syntax if/else.
- Lebih mudah di debug dan juga lebih mudah diatur output errornya.
- Dapat mengakomodasi lebih banyak jenis grammar dan bahkan jika ada beberapa grammar set yang berbeda dalam satu bahasa (ada LR(1), LL(K), context-free, context-sensitive)

###### Automatic parser seperti LALR,
- Lebih mudah ditulis tapi tidak fleksibel. Hanya menerima grammar yang spesifik seperti LL(1) saja.
- Digunakan untuk syntax highlighting karena dengan ini seorang developer dapat dengan mudah membuat extensi highlighting sendiri tanpa mengimplementasikan parser sendiri.
- Selain itu digunakan untuk syntax highligting karena proses ini lebih sederhana dan butuh konteks yang lebih sedikit (tidak perlu tahu itu if/else block hanya perlu tahu itu keyword, argumen function, dst.)

##### Apakah bisa bahasa kompleks di parse dengan automatic parser?

- Jawabanya Iya! Namun, perlu tambahan lagi yaitu semantic actions.

- Jadi semantic actions adalah simbol yang disisipkan di AST sebagai “penanda” setelah suatu action di detect ketika parsing, logic tambahan dilakukan.

Misal
```
Branch → “if” C1 expr “then” C2 expr C3 “end” “if” C4
```

- Sistemnya seperti interrupt di OS. Setiap C1, C2, ... akan trigger suatu action/kode yang menghandle verifikasi, symbol lookup, dst.



Note: Go [here](https://github.com/funcml/funcml_runtime/blob/main/README.md) to see our lesson learnt in the other aspect (the runtime side)

### Contributing

We welcome contributions! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for guidelines.

### License

FunML is released under the MIT License.

---
For more information, visit the [project website](https://github.com/pemfung/funml) or join the discussion on our [community forum](https://community.funml.dev).

